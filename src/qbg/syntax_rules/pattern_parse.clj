(ns qbg.syntax-rules.pattern-parse)

(defn- pattern-vars
  ([]
     #{})
  ([form]
     (condp = (first form)
	 :variable #{(second form)}
	 :varclass #{(second form)}
	 :literal #{}
	 :describe (pattern-vars (nth form 2))
	 :head (apply pattern-vars (rest form))
	 :and (apply pattern-vars (rest form))
	 :or (apply pattern-vars (rest form))
	 :list (apply pattern-vars (rest form))
	 :vector (apply pattern-vars (rest form))
	 :amp (apply pattern-vars (rest (rest form)))))
  ([form & forms]
     (reduce into (pattern-vars form) (map pattern-vars forms))))

(declare parse-pattern parse-seq)

(defn- parse-symbol
  [form literals]
  (if (contains? literals form)
    `(:literal ~form)
    (let [parts (.split (str form) "\\.")]
      `(:variable ~@(map symbol parts)))))

(defn- parse-varclass
  [form literals]
  (let [[_ variable class & args] form]
    `(:varclass ~variable ~(resolve class) ~@args)))

(defn- parse-literal
  [form literals]
  `(:literal ~form))

(defn- parse-amp
  [form literals]
  (let [templates (rest form)
        body (parse-seq templates literals)
        vars (apply pattern-vars body)]
    `(:amp ~vars ~@body)))

(defn- parse-ellipsis
  [pattern literals]
  (let [pat (parse-pattern pattern literals)
	vars (pattern-vars pat)]
    `(:amp ~vars ~pat)))

(defn- parse-seq
  [form literals]
  (loop [res [], form (seq form)]
    (if (seq form)
      (if (= (second form) '...)
        (recur (conj res (parse-ellipsis (first form) literals)) (nthnext form 2))
        (recur (conj res (parse-pattern (first form) literals)) (next form)))
      res)))

(defn- parse-describe
  [form literals]
  (let [[_ mesg & pattern] form]
    `(:describe ~mesg ~(first (parse-seq pattern literals)))))

(defn- parse-list
  [form literals]
  (cond
   (= (first form) '+literal) `(:literal ~(second form))
   (= (first form) '+&) (parse-amp form literals)
   (= (first form) '+describe) (parse-describe form literals)
   (= (first form) '+var) (parse-varclass form literals)
   (= (first form) '+head) `(:head ~@(parse-seq (rest form) literals))
   (= (first form) '+and) `(:and ~@(parse-seq (rest form) literals))
   (= (first form) '+or) `(:or ~@(parse-seq (rest form) literals))
   :else (cons :list (parse-seq form literals))))

(defn- parse-vector
  [form literals]
  (cons :vector (parse-seq form literals)))

(defn parse-pattern
  [pattern literals]
  (cond
    (symbol? pattern) (parse-symbol pattern literals)
    (seq? pattern) (parse-list pattern literals)
    (vector? pattern) (parse-vector pattern literals)
    :else (parse-literal pattern literals)))

(defn build-rule-template
  [rule template literals]
  (let [literals (set literals)
	rule (parse-pattern rule literals)
        template (parse-pattern template literals)]
    [rule template]))
