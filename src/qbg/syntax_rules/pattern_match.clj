(ns qbg.syntax-rules.pattern-match)

(defn- ms
  [depth val]
  {:amp-depth depth :val val})

(defn- merge-symbols
  [s1 s2]
  (if (= (:amp-depth s1) (inc (:amp-depth s2)))
    (assoc s1 :val (conj (:val s1) (:val s2)))
    (throw (IllegalStateException. "Inconsistent amperstand depth"))))

(defn- merge-states
  [& states]
  (if (every? identity states)
    (apply merge-with merge-symbols states)
    false))

(declare match-seq match)

(defn- match-symbol
  [pattern form]
  (if (= (second pattern) '_)
    {}
    {(second pattern) (ms 0 form)}))

(defn- match-literal
  [pattern form]
  (if (= form (second pattern))
    {}
    false))

(defn- promote
  [state]
  (if (= state false)
    false
    (into {} (map (fn [[k v]]
                    [k (assoc v
                         :amp-depth (inc (:amp-depth v))
                         :val [(:val v)])]) state))))

(defn- match-amp
  [pattern form]
  (let [patterns (nthnext pattern 2)]
    (if (zero? (rem (count form) (count patterns)))
      (let [parts (partition (count patterns) form)
            matches (map match-seq (repeat patterns) parts)]
        (apply merge-states (promote (first matches)) (rest matches)))
      false)))

(defn- match-seq
  [pattern form]
  (loop [states [], form form, pattern pattern]
    (if (seq pattern)
      (if (= (first (first pattern)) :amp)
        (recur (conj states (match-amp (first pattern) form)) nil nil)
        (if (seq form)
          (recur (conj states (match (first pattern) (first form)))
            (next form) (next pattern))
          false))
      (if (empty? form)
        (apply merge-states states)
        false))))

(defn- match-list
  [pattern form]
  (if (seq? form)
    (match-seq (rest pattern) form)
    false))

(defn- match-vector
  [pattern form]
  (if (vector? form)
    (match-seq (rest pattern) form)
    false))

(defn match
  [pattern form]
  ((condp = (first pattern)
    :variable match-symbol
    :literal match-literal
    :list match-list
    :vector match-vector)
    pattern form))
