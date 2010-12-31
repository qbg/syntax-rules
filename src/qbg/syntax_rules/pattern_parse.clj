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
   `(:literal ~form)

   (method-form? form)
   `(:literal ~form)

   (ctor-form? form)
   `(:literal ~(resolve-ctor form (:ns options)))

   :else `(:variable ~form)))

(defn- parse-varclass
  [form options]
  (let [[_ variable class & args] form]
    `(:varclass ~variable ~(ns-resolve (:ns options) class) ~@args)))

(defn- parse-literal
  [form options]
  `(:literal ~form))

(defn- parse-ellipsis
  [pattern options]
  (let [vars (pattern-vars pattern)]
    `(:amp ~vars ~pattern)))

(defn- parse-sugar-head
  [patterns options]
  `(:head ~@(parse-seq patterns options)))

(defn- parse-sugar-varclass
  [var klass options]
  (if (seq? klass)
    `(:varclass ~var ~(ns-resolve (:ns options) (first klass)) ~@(next klass))
    `(:varclass ~var ~(ns-resolve (:ns options) klass))))

(defn- parse-seq
  [form options]
  (loop [res [], form (seq form)]
    (if (seq form)
      (cond
       (= (first form) ':!)
       (recur (conj res (parse-literal (second form) options)) (nthnext form 2))

       (= (first form) ':&)
       (recur (conj res (parse-sugar-head (second form) options)) (nthnext form 2))

       (= (second form) ':>)
       (recur (conj res
		    (parse-sugar-varclass (first form) (nth form 2) options))
	      (nthnext form 3))
       
       (= (first form) '...)
       (recur (conj (pop res)
		    (parse-ellipsis (peek res) options))
	      (next form))

       :else
       (recur (conj res (parse-pattern (first form) options)) (next form)))
      res)))

(defn- parse-describe
  [form options]
  (let [[_ mesg & pattern] form]
    `(:describe ~mesg ~(first (parse-seq pattern options)))))

(defn- parse-pattern-form
  [form options]
  (let [[_ & pt] form
	[pattern template] (parse-seq pt options)]
    `(:pattern ~pattern ~template)))

(defn- parse-guard
  [form]
  (let [[_ code mesg] form]
    `(:guard ~(ns-name *ns*) ~code ~mesg)))

(defn- parse-options
  [form options]
  (let [patterns (map #(parse-seq % options) (rest form))
	patterns (map (fn [p] `(:head ~@p)) patterns)
	vars (apply pattern-vars patterns)]
    `(:options ~vars ~@patterns)))

(defn- parse-list
  [form options]
  (cond
   (= (first form) '+literal) `(:literal ~(second form))
   (= (first form) '+describe) (parse-describe form options)
   (= (first form) '+var) (parse-varclass form options)
   (= (first form) '+head) `(:head ~@(parse-seq (rest form) options))
   (= (first form) '+and) `(:and ~@(parse-seq (rest form) options))
   (= (first form) '+or) `(:or ~@(parse-seq (rest form) options))
   (= (first form) '+pattern) (parse-pattern-form form options)
   (= (first form) '+guard) (parse-guard form)
   (= (first form) '+code) `(:code ~(set (second form))
				   ~(ns-name (:ns options))
				   ~(nth form 2))
   (= (first form) '+options) (parse-options form options)
   :else (cons :list (parse-seq form options))))

(defn- parse-vector
  [form options]
  (cons :vector (parse-seq form options)))

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
	options {:literals literals :ns ns}
	rule (parse-pattern rule options)
        template (parse-pattern template options)]
    [rule template]))

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
  (loop [res [(parse-pattern (first part) options)], parts (next part)]
    (cond
     (empty? parts) `(:head ~@res)

     (= (first parts) :fail-when)
     (let [[_ code mesg & parts] parts
	   guard `(:guard ~(ns-name *ns*) ~code ~mesg)]
       (recur (conj res guard) parts))

     (= (first parts) :with)
     (let [[_ pattern template & parts] parts
	   pattern (parse-pattern pattern options)
	   template (parse-pattern template options)
	   pat `(:pattern ~pattern ~template)]
       (recur (conj res pat) parts)))))

(defn build-class-pattern
  [description literals ns body]
  (let [literals (set literals)
	options {:literals literals :ns ns}
	parts (sc-split body)
	parts-pattern (map #(build-part-pattern % options) parts)]
    `(:describe ~description (:or ~@parts-pattern))))