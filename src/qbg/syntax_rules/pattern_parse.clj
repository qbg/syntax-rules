(ns qbg.syntax-rules.pattern-parse)

(defn- pattern-vars
  ([]
    #{})
  ([form]
    (condp = (first form)
      :variable #{(second form)}
      :symbol #{}
      :literal #{}
      :describe (pattern-vars (nth form 2))
      :list (apply pattern-vars (rest form))
      :vector (apply pattern-vars (rest form))
      :amp (apply pattern-vars (rest (rest form)))))
  ([form & forms]
    (reduce into (pattern-vars form) (map pattern-vars forms))))

(declare parse-pattern parse-seq)

(defn- parse-symbol
  [form]
  `(:variable ~form))

(defn- parse-literal
  [form]
  `(:literal ~form))

(defn- parse-amp
  [form]
  (let [templates (rest form)
        body (parse-seq templates)
        vars (apply pattern-vars body)]
    `(:amp ~vars ~@body)))

(defn- parse-ellipsis
  [pattern]
  (let [pat (parse-pattern pattern)
	vars (pattern-vars pat)]
    `(:amp ~vars ~pat)))

(defn- parse-seq
  [form]
  (loop [res [], form (seq form)]
    (if (seq form)
      (if (= (second form) '...)
        (recur (conj res (parse-ellipsis (first form))) (nthnext form 2))
        (recur (conj res (parse-pattern (first form))) (next form)))
      res)))

(defn- parse-describe
  [form]
  (let [[_ mesg & pattern] form]
    `(:describe ~mesg ~(first (parse-seq pattern)))))
  
(defn- parse-list
  [form]
  (cond
   (= (first form) '+literal) `(:literal ~(second form))
   (= (first form) '+&) (parse-amp form)
   (= (first form) '+describe) (parse-describe form)
   :else (cons :list (parse-seq form))))

(defn- parse-vector
  [form]
  (cons :vector (parse-seq form)))

(defn parse-pattern
  [pattern]
  (cond
    (symbol? pattern) (parse-symbol pattern)
    (seq? pattern) (parse-list pattern)
    (vector? pattern) (parse-vector pattern)
    :else (parse-literal pattern)))

(defn- convert-vars
  [pattern vars literals]
  (letfn [(convert-seq
            [coll]
            (map #(convert-vars % vars literals) coll))
          (filter-vars
            [v]
            (set (filter vars v)))]
    (condp = (first pattern)
	:variable (cond
		   (vars (second pattern)) pattern
		   (literals (second pattern)) `(:literal ~(second pattern))
		   :else `(:symbol ~(second pattern)))
	:symbol pattern
	:literal pattern
	:list `(:list ~@(convert-seq (rest pattern))) 
	:vector `(:vector ~@(convert-seq (rest pattern)))
	:describe `(:describe ~(second pattern)
			      ~(convert-vars (nth pattern 2) vars))
	:amp `(:amp
	       ~(filter-vars (nth pattern 1))
	       ~@(convert-seq (nthnext pattern 2))))))

(defn build-rule-template
  [rule template literals]
  (let [rule (parse-pattern rule)
        template (parse-pattern template)
        vars (pattern-vars rule)
        template (convert-vars template vars (set literals))]
    [rule template]))
