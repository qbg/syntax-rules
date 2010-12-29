(ns qbg.syntax-rules.template-fill
  (:require
   [qbg.syntax-rules.pattern-parse :as pp]))

(declare fill-form fill-amp fill-head)

(defn- fill-literal
  [form state mappings]
  (second form))

(defn- multisegment?
  [name]
  (boolean (some #{\.} (str name))))

(defn- split-symbol
  [sym]
  (map symbol (.split (str sym) "\\.")))

(defn- fill-simple-variable
  [variable state]
  (let [v (get (:vars state) variable)]
    (if (= (:amp-depth v) 0)
      (:val v)
      (let [mesg (format "Inconsistent ellipsis depth: %s" variable)]
	(throw (IllegalStateException. mesg))))))

(defn- fill-symbol
  [sym mappings]
  (if (contains? mappings sym)
    (get mappings sym)
    ;; Hack
    (symbol (subs (str (resolve sym)) 2))))

(defn- fill-variable
  [form state mappings]
  (let [sym (second form)]
    (if (not (contains? (:vars state) sym))
      (fill-symbol sym mappings)
      (fill-simple-variable sym state))))

(defn- fill-seq
  [form state mappings]
  (loop [res [], form form]
    (if (seq form)
      (cond
       (= (first (first form)) :amp)
       (recur (into res (fill-amp (first form) state mappings)) (next form))
       (= (first (first form)) :head)
       (recur (into res (fill-head (first form) state mappings)) (next form))
       :else
       (recur (conj res (fill-form (first form) state mappings)) (next form)))
      res)))

(defn- fill-list
  [form state mappings]
  (seq (fill-seq (rest form) state mappings)))

(defn- fill-vector
  [form state mappings]
  (fill-seq (rest form) state mappings))

(defn- get-var-length
  [vars state]
  (count (:val (get (:vars state) (first vars)))))

(defn- assert-vars
  [vars state]
  (let [lengths (map #(count (:val (get (:vars state) %))) vars)]
    (if (apply = lengths)
      true
      (throw (IllegalStateException. "Variables under ellipsis have unequal lengths")))))

(defn- demote-vars
  [vars state]
  (let [vstate (:vars state)
	vstate (reduce (fn [state v]
			 (let [sym (get state v)
			       sym (assoc sym
				     :amp-depth (dec (:amp-depth sym))
				     :val (first (:val sym)))]
			   (assoc state v sym)))
		       vstate vars)]
    (assoc state :vars vstate)))

(defn- drop-vars
  [vars state]
  (let [vstate (:vars state)
	vstate (reduce (fn [state v]
			 (let [sym (get state v)
			       sym (assoc sym :val (rest (:val sym)))]
			   (assoc state v sym)))
		       vstate vars)]
    (assoc state :vars vstate)))

(defn- fill-amp
  [form state mappings]
  (let [[_ vars & forms] form
	length (get-var-length vars state)]
    (assert-vars vars state)
    (loop [res [], n 0, state state]
      (if (< n length)
        (recur
          (conj res (fill-seq forms (demote-vars vars state) mappings))
          (inc n)
          (drop-vars vars state))
        (apply concat res)))))

(defn- fill-head
  [form state mappings]
  (let [forms (rest form)]
    (map #(fill-form % state mappings) forms)))

(defn- fill-code
  [form state mappings]
  (let [[_ ns code] form]
    (binding [*ns* (find-ns ns)]
      (eval code))))

(defn- fill-form
  [form state mappings]
  ((condp = (first form)
    :variable fill-variable
    :list fill-list
    :vector fill-vector
    :amp fill-amp
    :literal fill-literal
    :code fill-code)
    form state mappings))

(defn- find-symbols
  [state form]
  (let [patvars (pp/pattern-vars form)]
    (into #{} (remove (:vars state) patvars))))

(defn- make-mappings
  [syms]
  (let [needed (filter #(and
			 (not (multisegment? %))
			 (not (resolve %))) syms)
        mappings (zipmap needed (map gensym needed))]
    (reduce #(assoc %1 %2 %2)
	    mappings '[quote def var recur do if throw try monitor-enter
		       monitor-exit . new set!])))

(defn- map-depth
  [f depth coll]
  (if (= depth 0)
    (f coll)
    (vec (map #(map-depth f (dec depth) %) coll))))

(defn- flatten-n
  [n coll]
  (if (= n 0)
    [coll]
    (mapcat #(flatten-n (dec n) %) coll)))

(defn- get-nested-val
  [state var-parts]
  (let [v (first var-parts)
	vm ((:vars state) v)
	d (:amp-depth vm)]
    (if vm
      (if (empty? (next var-parts))
	(if (contains? (:varm state) v)
	  [d (map-depth :mega d (:val vm))]
	  [d (:val vm)])
	(let [vps (next var-parts)
	      init (map-depth #(get-nested-val % vps) d (:val vm))
	      parts (flatten-n d init)]
	  (if (some false? parts)
	    false
	    (let [depths (map first parts)
		  _ (if (not (apply = depths))
		      (throw (IllegalStateException. "Inconsistent ellipsis depth")))
		  depth (first depths)]
	      [(+ depth d) (map-depth second d init)]))))
      false)))

(defn- create-nested-var
  [state sym]
  (let [var-parts (split-symbol sym)
	nv (get-nested-val state var-parts)]
    (if nv
      (let [[d val] nv]
	[[sym {:amp-depth d :val val}]])
      [])))

(defn- add-nested-vars
  [vars state]
  (let [new-vars (mapcat #(create-nested-var state %) vars)]
    (assoc state
      :vars (into {} new-vars)
      :varm #{})))

(defn- compact-state
  [state form]
  (let [vars (pp/pattern-vars form)]
    (add-nested-vars vars state)))
  
(defn fill-template
  [form state]
  (let [state (compact-state state form)
	syms (find-symbols state form)
	mappings (make-mappings syms)]
    (fill-form form state mappings)))
