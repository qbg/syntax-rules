(ns qbg.syntax-rules.template-fill)

(declare fill-form fill-amp fill-head)

(defn- fill-literal
  [form state mappings]
  (second form))

(defn- fill-simple-variable
  [variable state]
  (let [v (get (:vars state) variable)]
    (if (= (:amp-depth v) 0)
      (:val v)
      (throw (IllegalStateException. (format "Inconsistent ampersand depth: %s" variable))))))

(defn- fill-symbol
  [sym mappings]
  (if (contains? mappings sym)
    (get mappings sym)
    ;; Hack
    (symbol (subs (str (resolve sym)) 2))))

(defn- fill-variable
  [form state mappings]
  (if (not (contains? (:vars state) (second form)))
    (fill-symbol (second form) mappings)
    (fill-simple-variable (second form) state)))

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
      (throw (IllegalStateException. "Variables under ampersand do not have equal lengths")))))

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
    (map fill-form forms)))

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
  ([state]
    #{})
  ([state form]
    (condp = (first form)
	:variable (if (contains? (:vars state) (second form))
		    #{(second form)}
		    #{})
	:list (apply find-symbols (rest form))
	:vector (apply find-symbols (rest form))
	:amp (apply find-symbols (rest (rest form)))
	:literal #{}
	:code #{}))
  ([state form & forms]
     (reduce into (find-symbols state form)
	     (map #(find-symbols state %) forms))))

(defn- make-mappings
  [syms]
  (let [needed (filter #(or (nil? %) (not (resolve %))) syms)
        mappings (zipmap needed (map gensym needed))]
    (reduce #(assoc %1 %2 %2)
	    mappings '[quote def var recur do if throw try monitor-enter monitor-exit
		       . new set!])))

(defn- compact-state
  [state]
  (letfn [(pack-state [state v]
	    (let [primary (into {} (map #(fix-name v %) (:vars state)))
		  f (fn [[k v]] (pack-state v k))]
	      (apply merge primary (map f (:varm state)))))
	  (fix-name [pre [k v]] [(symbol (str pre "." k)) v])]
    (let [vars (:vars state)
	  secondary (map (fn [[k v]] (pack-state v k)) (:varm state))]
      (assoc state
	:vars (apply merge vars secondary)))))

(defn fill-template
  [form state]
  (let [state (compact-state state)
	syms (find-symbols state form)
	mappings (make-mappings syms)]
    (fill-form form state mappings)))
