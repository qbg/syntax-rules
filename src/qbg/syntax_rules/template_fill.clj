(ns qbg.syntax-rules.template-fill
  (:require
   [qbg.syntax-rules.pattern-parse :as pp]))

(def *current-match*)

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

(defn- get-part
  [coll choices]
  (if-let [[choice & choices] (seq choices)]
    (recur (nth coll choice) choices)
    coll))

(defn- get-variable-part
  [state parts choices]
  (let [vm (get (:vars state) (first parts))
	ad (:amp-depth vm)]
    (let [[first-choices next-choices] (split-at ad choices)
	  next-vm (get-part (:val vm) first-choices)]
      (cond
       (empty? (next parts))
       next-vm
       
       (< (count choices) ad)
       next-vm

       (> (count choices) ad)
       (recur next-vm (next parts) next-choices)

       :else (get (:vars next-vm) (second parts))))))

(defn contains-var?
  [state var]
  (if (multisegment? var)
    (let [parts (split-symbol var)
	  part (get-variable-part state (butlast parts) (:fill-stack state))]
      (contains? (:vars part) (last parts)))
    (contains? (:vars state) var)))

(defn- get-var-value
  [state fill-stack parts]
  (let [[v & parts] parts
	vm (get (:vars state) v)
	depth (:amp-depth vm)
	[fs next-fill-stack] (split-at depth fill-stack)
	part (get-part (:val vm) fs)]
    (if (empty? parts)
      (if (not= depth (count fill-stack))
	(throw (IllegalArgumentException. "Inconsistant ellipsis depth"))
	(if (contains? (:varm state) v)
	  (:mega part)
	  part))
      (if (contains? (:varm state) v)
	(recur part next-fill-stack parts)
	(throw (IllegalArgumentException. "No such pattern variable"))))))

(defn- fill-simple-variable
  [variable state]
  (let [fill-stack (:fill-stack state)
	parts (split-symbol variable)]
    (if (contains? (:vars state) (first parts))
      (get-var-value state fill-stack parts)
      (throw (IllegalArgumentException. "No such pattern variable")))))

(defn- fill-symbol
  [sym mappings]
  (if (contains? mappings sym)
    (get mappings sym)
    ;; Hack
    (symbol (subs (str (resolve sym)) 2))))

(defn- fill-variable
  [form state mappings]
  (let [sym (second form)]
    (if (or (multisegment? sym) (contains? (:vars state) sym))
      (fill-simple-variable sym state)
      (fill-symbol sym mappings))))

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
  [state v]
  (let [fs (:fill-stack state)
	parts (split-symbol v)
	vp (get-variable-part state parts fs)]
    (if (vector? vp)
      (count vp)
      (count (:val vp)))))

(defn- get-vars-length
  [vars state]
  (let [lengths (map #(get-var-length state %) vars)]
    (if (apply = lengths)
      (first lengths)
      (throw (IllegalStateException. "Variables under ellipsis have unequal lengths")))))

(defn- fill-amp
  [form state mappings]
  (let [[_ vars & forms] form
	length (get-vars-length vars state)
	fs (:fill-stack state)]
    (loop [res [], n 0]
      (if (< n length)
	(let [next-fs (conj fs n)
	      state (assoc state :fill-stack next-fs)]
	  (recur (conj res (fill-seq forms state mappings)) (inc n)))
        (apply concat res)))))

(defn- fill-head
  [form state mappings]
  (let [forms (rest form)]
    (map #(fill-form % state mappings) forms)))

(defn- fill-code
  [form state mappings]
  (let [[_ _ ns code] form]
    (binding [*ns* (find-ns ns)
	      *current-match* state]
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
  (let [patvars (pp/pattern-vars form)
	clean #(or (get (:vars state) %) (multisegment? %))]
    (into #{} (remove clean patvars))))

(defn- make-mappings
  [syms]
  (let [needed (filter #(and
			 (not (multisegment? %))
			 (not (resolve %))) syms)
        mappings (zipmap needed (map gensym needed))]
    (reduce #(assoc %1 %2 %2)
	    mappings '[quote def var recur do if throw try monitor-enter
		       monitor-exit . new set!])))
  
(defn fill-template
  [form state]
  (let [state (assoc state :fill-stack (or (:fill-stack state) []))
	syms (find-symbols state form)
	mappings (make-mappings syms)]
    (fill-form form state mappings)))
