(ns qbg.syntax-rules.pattern-parse)

(defn pattern-vars
  ([]
     #{})
  ([form]
     (condp = (first form)
	 :variable #{(second form)}
	 :varclass #{(second form)}
	 :literal #{}
	 :pattern (pattern-vars (second form))
	 :describe (pattern-vars (nth form 2))
	 :guard #{}
	 :code (second form)
	 :scode (second form)
	 :head (apply pattern-vars (rest form))
	 :and (apply pattern-vars (rest form))
	 :or (apply pattern-vars (rest form))
	 :list (apply pattern-vars (rest form))
	 :vector (apply pattern-vars (rest form))
	 :options (second form)
	 :amp (second form)))
  ([form & forms]
     (reduce into (pattern-vars form) (map pattern-vars forms))))	      

(declare parse-pattern parse-seq)

(defn- method-form?
  [sym]
  (= \. (first (str sym))))

(defn- ctor-form?
  [sym]
  (= \. (last (str sym))))

(defn- resolve-ctor
  [sym ns]
  (let [chop-end (fn [s] (subs s 0 (dec (count s))))
	class-sym (symbol (chop-end (str sym)))
	full-class (ns-resolve ns class-sym)]
    (symbol (str (.getName full-class) "."))))

(defn- parse-symbol
  [form options]
  (cond
   (contains? (:literals options) form)
   [`(:literal ~form) options]
   
   (method-form? form)
   [`(:literal ~form) options]
   
   (ctor-form? form)
   [`(:literal ~(resolve-ctor form (:ns options))) options]
   
   :else [`(:variable ~form) options]))

(defn- parse-varclass
  [form options]
  (let [[_ variable class & args] form
	klass (cons class args)
	fns (:fns options)
	n (:n options)
	fns (assoc fns n `(fn [] ~klass))]
    [`(:varclass ~variable ~n) (assoc options :n (inc n) :fns fns)]))

(defn- parse-literal
  [form options]
  [`(:literal ~form) options])

(defn- parse-ellipsis
  [pattern options]
  (let [vars (pattern-vars pattern)]
    [`(:amp ~vars ~pattern) options]))

(defn- parse-sugar-head
  [patterns options]
  (let [[pats options] (parse-seq patterns options)]
    [`(:head ~@pats) options]))

(defn- parse-sugar-varclass
  [var klass options]
  (let [klass (if (seq? klass) klass (list klass))
	fns (:fns options)
	n (:n options)
	fns (assoc fns n `(fn [] ~klass))]
    [`(:varclass ~var ~n) (assoc options :fns fns :n (inc n))]))

(defn- parse-seq
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

       (= (second form) ':>)
       (let [[pat options] (parse-sugar-varclass (first form) (nth form 2) options)]
	 (recur (conj res pat) (nthnext form 3) options))
       
       (= (first form) '...)
       (let [[pat options] (parse-ellipsis (peek res) options)]
	 (recur (conj (pop res) pat) (next form) options))

       :else
       (let [[pat options] (parse-pattern (first form) options)]
	 (recur (conj res pat) (next form) options)))
      [res options])))

(defn- map-parse-seq
  [forms options]
  (loop [res [], forms forms, options options]
    (if (seq forms)
      (let [[pats options] (parse-seq (first forms) options)]
	(recur (conj res pats) (next forms) options))
      [res options])))

(defn- parse-describe
  [form options]
  (let [[_ mesg & pattern] form
	[[pat] options] (parse-seq pattern options)]
    [`(:describe ~mesg ~pat) options]))

(defn- parse-pattern-form
  [form options]
  (let [[_ & pt] form
	[[pattern template] options] (parse-seq pt options)]
    [`(:pattern ~pattern ~template) options]))

(defn- parse-guard
  [form options]
  (let [[_ code mesg] form
	fns (:fns options)
	n (:n options)
	fns (assoc fns n `(fn [] ~code))
	m (inc n)
	fns (assoc fns m `(fn [] ~mesg))]
    [`(:guard ~n ~m) (assoc options :n (inc m) :fns fns)]))

(defn- parse-options
  [form options]
  (let [[patterns options] (map-parse-seq (rest form) options)
	patterns (map (fn [p] `(:head ~@p)) patterns)
	vars (apply pattern-vars patterns)]
    [`(:options ~vars ~@patterns) options]))

(defn- build-trivial
  [head forms options]
  (let [[pats options] (parse-seq forms options)]
    [(cons head pats) options]))

(defn- build-code
  [head vars code options]
  (let [fns (:fns options)
	n (:n options)
	fns (assoc fns n `(fn [] ~code))]
    [`(~head ~(set vars) ~n) (assoc options :n (inc n) :fns fns)]))

(defn- parse-?
  [form options]
  (let [[pats options] (parse-seq (rest form) options)]
    [`(:or (:head ~@pats) (:head)) options]))

(defn- parse-list
  [form options]
  (cond
   (= (first form) '+literal) [`(:literal ~(second form)) options]
   (= (first form) '+describe) (parse-describe form options)
   (= (first form) '+var) (parse-varclass form options)
   (= (first form) '+head) (build-trivial :head (rest form) options)
   (= (first form) '+and) (build-trivial :and (rest form) options)
   (= (first form) '+or) (build-trivial :or (rest form) options)
   (= (first form) '+pattern) (parse-pattern-form form options)
   (= (first form) '+guard) (parse-guard form options)
   (= (first form) '+code) (build-code :code (second form) (nth form 2) options)
   (= (first form) '+scode) (build-code :scode (second form) (nth form 2) options)
   (= (first form) '+options) (parse-options form options)
   (= (first form) '+?) (parse-? form options)
   :else (let [[pats options] (parse-seq form options)]
	   [(cons :list pats) options])))

(defn- parse-vector
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

(defn build-rule-template
  [rule template literals ns]
  (let [literals (set literals)
	options {:literals literals :ns ns :fns {} :n 0}
	[rule options] (parse-pattern rule options)
        [template options] (parse-pattern template options)]
    [rule template (:fns options)]))

(defn- sc-split
  [form]
  (loop [body (next form), res [], part [(first form)]]
    (cond
     (empty? body)
     (conj res part)

     (not (keyword (first body)))
     (recur (next body) (conj res part) [(first body)])

     (= (first body) :fail-when)
     (recur (nthnext body 3) res (conj part :fail-when (nth body 1) (nth body 2)))

     (= (first body) :with)
     (recur (nthnext body 3) res (conj part :with (nth body 1) (nth body 2))))))

(defn- build-part-pattern
  [part options]
  (let [[res options] (parse-pattern (first part) options)]
    (loop [res [res], parts (next part), options options]
      (cond
       (empty? parts) [`(:head ~@res) options]
       
       (= (first parts) :fail-when)
       (let [[_ code mesg & parts] parts
	     [guard options] (parse-guard `(+guard ~code ~mesg) options)]
	 (recur (conj res guard) parts options))
       
       (= (first parts) :with)
       (let [[_ pattern template & parts] parts
	     [pattern options] (parse-pattern pattern options)
	     [template options] (parse-pattern template options)
	     pat `(:pattern ~pattern ~template)]
	 (recur (conj res pat) parts options))))))

(defn- build-parts
  [parts options]
  (loop [res [], parts parts, options options]
    (if (seq parts)
      (let [[pat options] (build-part-pattern (first parts) options)]
	(recur (conj res pat) (next parts) options))
      [res options])))

(defn build-class-pattern
  [description literals ns body]
  (let [literals (set literals)
	options {:literals literals :ns ns :fns {} :n 0}
	parts (sc-split body)
	[parts-pattern options] (build-parts parts options)]
    [`(:describe ~description (:or ~@parts-pattern)) (:fns options)]))
