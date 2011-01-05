(ns qbg.syntax-rules.template.fill
  (:require
   [qbg.syntax-rules.template.parse :as parse]
   [qbg.syntax-rules.core :as core]))

(declare fill-form fill-amp fill-code)

(defn fill-literal
  [form state mappings]
  (second form))

(defn multisegment?
  [name]
  (boolean (some #{\.} (str name))))

(defn split-symbol
  [sym]
  (map symbol (.split (str sym) "\\.")))

(defn get-var-value
  ([vm fill-stack parts]
     (get-var-value vm fill-stack parts false))
  ([vm fill-stack parts soft]
     (if (seq fill-stack)
       (cond
	(contains? vm :ell)
	(recur (nth (:ell vm) (first fill-stack))
	       (next fill-stack) parts soft)
	(contains? vm :vars)
	(if (seq parts)
	  (if (contains? (:vars vm) (first parts))
	    (recur ((:vars vm) (first parts)) fill-stack (next parts) soft)
	    (or soft (throw (IllegalStateException. "No such pattern variable"))))
	  (or soft (throw (IllegalStateException. "Inconsistant ellipsis depth"))))
	:else
	(throw (IllegalStateException. "Inconsistant ellipsis depth")))
       (if (seq parts)
	 (if (contains? vm :vars)
	   (if (contains? (:vars vm) (first parts))
	     (recur ((:vars vm) (first parts)) fill-stack (next parts) soft)
	     (or soft (throw (IllegalStateException. "No such pattern variable"))))
	   vm)
	 vm))))

(defn contains-var?
  [state var]
  (if (multisegment? var)
    (let [parts (split-symbol var)
	  part (get-var-value state parts (:fill-stack state) :fail)]
      (not= :fail part))
    (contains? (:vars state) var)))

(defn fill-simple-variable
  [variable state]
  (let [fill-stack (:fill-stack state)
	parts (split-symbol variable)
	vm (get-var-value ((:vars state) (first parts)) fill-stack (next parts))]
    (if (and vm (contains? vm :val))
      (:val vm)
      (throw (IllegalStateException. "No such pattern variable")))))

(defn actually-variable?
  [state mappings sym]
  (or (multisegment? sym)
      (not (contains? mappings sym))))

(defn fill-variable
  [form state mappings]
  (let [sym (second form)]
    (if (actually-variable? state mappings sym)
      (fill-simple-variable sym state)
      (get mappings sym))))

(defn fill-head
  [form state mappings]
  (let [forms (rest form)]
    (map #(fill-form % state mappings) forms)))

(defn fill-seq
  [form state mappings]
  (loop [res [], form form]
    (if (seq form)
      (cond
       (= (first (first form)) :amp)
       (recur (into res (fill-amp (first form) state mappings)) (next form))
       (= (first (first form)) :head)
       (recur (into res (fill-head (first form) state mappings)) (next form))
       (= (first (first form)) :scode)
       (recur (into res (fill-code (first form) state mappings)) (next form))
       :else
       (recur (conj res (fill-form (first form) state mappings)) (next form)))
      res)))

(defn fill-list
  [form state mappings]
  (seq (fill-seq (rest form) state mappings)))

(defn fill-vector
  [form state mappings]
  (fill-seq (rest form) state mappings))

(defn get-var-length
  [state v]
  (let [fs (:fill-stack state)
	parts (split-symbol v)
	vp (get-var-value ((:vars state) (first parts)) fs (next parts) :fail)]
    (if (= vp :fail) 0 (count (:ell vp)))))

(defn get-vars-length
  [vars state mappings]
  (let [vars (filter #(actually-variable? state mappings %) vars)
	lengths (map #(get-var-length state %) vars)]
    (if (seq lengths)
      (if (apply = lengths)
	(first lengths)
	(throw (IllegalStateException. "Variables under ellipsis have unequal lengths")))
      0)))

(defn fill-amp
  [form state mappings]
  (let [[_ vars & forms] form
	length (get-vars-length vars state mappings)
	fs (:fill-stack state)]
    (loop [res [], n 0]
      (if (< n length)
	(let [next-fs (conj fs n)
	      state (assoc state :fill-stack next-fs)]
	  (recur (conj res (fill-seq forms state mappings)) (inc n)))
        (apply concat res)))))

(defn fill-code
  [form state mappings]
  (let [[_ _ fn-n] form]
    (binding [core/*current-match* state]
      ((get (:params state) fn-n)))))

(defn fill-form
  [form state mappings]
  ((condp = (first form)
    :variable fill-variable
    :list fill-list
    :vector fill-vector
    :amp fill-amp
    :literal fill-literal
    :code fill-code)
    form state mappings))

(defn find-symbols
  [state form]
  (let [patvars (parse/pattern-vars form)
	clean #(or (get (:vars state) %) (multisegment? %))]
    (into #{} (remove clean patvars))))

(defn- resolve-sym
  [sym]
  ;; Hack
  (symbol (subs (str (resolve sym)) 2)))

(defn make-mappings
  [state syms]
  (let [syms (remove #(or (multisegment? %)
			  (contains? (:vars state) %))
		     syms)
	resolvable (filter resolve syms)
	gensyms (remove resolve syms)
        mappings (conj
		  (zipmap resolvable (map resolve-sym resolvable))
		  (zipmap gensyms (map gensym gensyms)))]
    (reduce #(assoc %1 %2 %2)
	    mappings '[quote def var recur do if throw try monitor-enter
		       monitor-exit . new set! case* fn* let* catch finally])))
  
(defn fill-template
  [form state]
  (let [state (assoc state :fill-stack (or (:fill-stack state) []))
	syms (find-symbols state form)
	mappings (make-mappings state syms)]
    (fill-form form state mappings)))
