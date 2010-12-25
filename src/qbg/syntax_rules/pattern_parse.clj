(ns qbg.syntax-rules.pattern-parse)

(defn- pattern-vars
  ([]
    #{})
  ([form]
    (condp = (first form)
      :variable #{(second form)}
      :symbol #{}
      :literal #{}
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

(defn- parse-seq
  [form]
  (loop [res [], form (seq form)]
    (if (seq form)
      (if (= (first form) '&)
        (recur (conj res (parse-amp form)) nil)
        (recur (conj res (parse-pattern (first form))) (next form)))
      res)))

(defn- parse-list
  [form]
  (cond
   (= (first form) '+literal) `(:literal ~(second form))
   (= (first form) '+&) (parse-amp form)
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
  [pattern vars]
  (letfn [(convert-seq
            [coll]
            (map #(convert-vars % vars) coll))
          (filter-vars
            [v]
            (set (filter vars v)))]
    (condp = (first pattern)
      :variable (if (contains? vars (second pattern))
                  pattern
                  `(:symbol ~(second pattern)))
      :symbol pattern
      :literal pattern
      :list `(:list ~@(convert-seq (rest pattern))) 
      :vector `(:vector ~@(convert-seq (rest pattern)))
      :amp `(:amp
              ~(filter-vars (nth pattern 1))
              ~@(convert-seq (nthnext pattern 2))))))

(defn build-rule-template
  [rule template]
  (let [rule (parse-pattern rule)
        template (parse-pattern template)
        vars (pattern-vars rule)
        template (convert-vars template vars)]
    [rule template]))
