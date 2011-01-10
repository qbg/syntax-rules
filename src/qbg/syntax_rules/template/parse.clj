(ns qbg.syntax-rules.template.parse)

(defn pattern-vars
  ([]
     #{})
  ([form]
     (condp = (first form)
	 :variable #{(second form)}
	 :literal #{}
	 :code (second form)
	 :scode (second form)
	 :head (apply pattern-vars (rest form))
	 :list (apply pattern-vars (rest form))
	 :vector (apply pattern-vars (rest form))
	 :amp (second form)))
  ([form & forms]
     (reduce into (pattern-vars form) (map pattern-vars forms))))	      

(declare parse-pattern parse-seq)

(defn method-form?
  [sym]
  (= \. (first (str sym))))

(defn ctor-form?
  [sym]
  (= \. (last (str sym))))

(defn resolve-ctor
  [sym ns]
  (let [chop-end (fn [s] (subs s 0 (dec (count s))))
	class-sym (symbol (chop-end (str sym)))
	full-class (ns-resolve ns class-sym)]
    (symbol (str (.getName full-class) "."))))

(defn parse-symbol
  [form options]
  (cond
   (contains? (:literals options) form)
   [`(:literal ~form) options]
   
   (method-form? form)
   [`(:literal ~form) options]
   
   (ctor-form? form)
   [`(:literal ~(resolve-ctor form (:ns options))) options]
   
   (namespace form)
   [`(:literal ~(ns-resolve (:ns options) form)) options]

   :else [`(:variable ~form) options]))

(defn parse-literal
  [form options]
  [`(:literal ~form) options])

(defn parse-ellipsis
  [pattern options]
  (let [vars (pattern-vars pattern)]
    [`(:amp ~vars ~pattern) options]))

(defn parse-sugar-head
  [patterns options]
  (let [[pats options] (parse-seq patterns options)]
    [`(:head ~@pats) options]))

(defn parse-seq
  [form options]
  (loop [res [], form (seq form), options options]
    (if (seq form)
      (cond
       (= (first form) ':!)
       (let [[pat options] (parse-literal (second form) options)]
	 (recur (conj res pat) (nthnext form 2) options))

       (= (first form) ':&)
       (let [[pat options] (parse-sugar-head (second form) options)] 
	 (recur (conj res pat) (nthnext form 2) options))
       
       (= (first form) '...)
       (let [[pat options] (parse-ellipsis (peek res) options)]
	 (recur (conj (pop res) pat) (next form) options))

       :else
       (let [[pat options] (parse-pattern (first form) options)]
	 (recur (conj res pat) (next form) options)))
      [res options])))

(defn map-parse-seq
  [forms options]
  (loop [res [], forms forms, options options]
    (if (seq forms)
      (let [[pats options] (parse-seq (first forms) options)]
	(recur (conj res pats) (next forms) options))
      [res options])))

(defn build-trivial
  [head forms options]
  (let [[pats options] (parse-seq forms options)]
    [(cons head pats) options]))

(defn build-code
  [head vars code options]
  (let [fns (:fns options)
	n (:n options)
	fns (assoc fns n `(fn [] ~code))]
    [`(~head ~(set vars) ~n) (assoc options :n (inc n) :fns fns)]))

(defn parse-list
  [form options]
  (cond
   (= (first form) '+literal) [`(:literal ~(second form)) options]
   (= (first form) '+head) (build-trivial :head (rest form) options)
   (= (first form) '+code) (build-code :code (second form) (nth form 2) options)
   (= (first form) '+scode) (build-code :scode (second form) (nth form 2) options)
   :else (let [[pats options] (parse-seq form options)]
	   [(cons :list pats) options])))

(defn parse-vector
  [form options]
  (let [[pats options] (parse-seq form options)]
    [(cons :vector pats) options]))

(defn parse-pattern
  [pattern options]
  (cond
    (symbol? pattern) (parse-symbol pattern options)
    (seq? pattern) (parse-list pattern options)
    (vector? pattern) (parse-vector pattern options)
    :else (parse-literal pattern options)))

(defn parse-template
  "Return [parse-template fns] given a template, literals, and the ns"
  [template literals ns]
  (let [literals (set literals)
	options {:literals literals :ns ns :fns {} :n 0}
	[template options] (parse-pattern template options)]
    [template (:fns options)]))

