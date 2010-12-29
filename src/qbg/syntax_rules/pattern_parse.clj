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
	 :code #{}
	 :head (apply pattern-vars (rest form))
	 :and (apply pattern-vars (rest form))
	 :or (apply pattern-vars (rest form))
	 :list (apply pattern-vars (rest form))
	 :vector (apply pattern-vars (rest form))
	 :amp (second form)))
  ([form & forms]
     (reduce into (pattern-vars form) (map pattern-vars forms))))

(declare parse-pattern parse-seq)

(defn- parse-symbol
  [form options]
  (if (contains? (:literals options) form)
    `(:literal ~form)
    `(:variable ~form)))

(defn- parse-varclass
  [form options]
  (let [[_ variable class & args] form]
    `(:varclass ~variable ~(ns-resolve (:ns options) class) ~@args)))

(defn- parse-literal
  [form options]
  `(:literal ~form))

(defn- parse-ellipsis
  [pattern options]
  (let [pat (parse-pattern pattern options)
	vars (pattern-vars pat)]
    `(:amp ~vars ~pat)))

(defn- parse-seq
  [form options]
  (loop [res [], form (seq form)]
    (if (seq form)
      (if (= (second form) '...)
        (recur (conj res (parse-ellipsis (first form) options)) (nthnext form 2))
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
  (let [[_ mesg code] form]
    `(:guard ~mesg ~(ns-name *ns*) ~code)))

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
   (= (first form) '+code) `(:code ~(ns-name (:ns options)) ~(second form))
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
     (let [[_ mesg code & parts] parts
	   guard `(:guard ~mesg ~(ns-name *ns*) ~code)]
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