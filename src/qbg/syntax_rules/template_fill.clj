(ns qbg.syntax-rules.template-fill)

(declare fill-form fill-amp)

(defn- fill-literal
  [form state mappings]
  (second form))

(defn- fill-variable
  [form state mappings]
  (let [v (get state (second form))]
    (if (= (:amp-depth v) 0)
      (:val v)
      (throw (IllegalStateException. "Inconsistent ampersand depth")))))

(defn- fill-symbol
  [form state mappings]
  (let [sym (second form)]
    (if (contains? mappings sym)
      (get mappings sym)
      ; Hack
      (symbol (subs (str (resolve sym)) 2)))))

(defn- fill-seq
  [form state mappings]
  (loop [res [], form form]
    (if (seq form)
      (if (= (first (first form)) :amp)
        (recur (into res (fill-amp (first form) state mappings)) (next form))
        (recur (conj res (fill-form (first form) state mappings)) (next form)))
      res)))

(defn- fill-list
  [form state mappings]
  (seq (fill-seq (rest form) state mappings)))

(defn- fill-vector
  [form state mappings]
  (fill-seq (rest form) state mappings))

(defn- get-var-length
  [vars state]
  (count (:val (get state (first vars)))))

(defn- assert-vars
  [vars state]
  (let [lengths (map #(count (:val (get state %))) vars)]
    (if (apply = lengths)
      true
      (throw (IllegalStateException. "Variables under ampersand do not have equal lengths")))))

(defn- demote-vars
  [vars state]
  (reduce (fn [state v]
            (let [sym (get state v)
                  sym (assoc sym
                        :amp-depth (dec (:amp-depth sym))
                        :val (first (:val sym)))]
              (assoc state v sym)))
    state vars))

(defn- drop-vars
  [vars state]
  (reduce (fn [state v]
            (let [sym (get state v)
                  sym (assoc sym :val (rest (:val sym)))]
              (assoc state v sym)))
    state vars))

(defn- fill-amp
  [form state mappings]
  (let [[_ vars & forms] form
        length (get-var-length vars state)]
    (assert-vars vars state)
    (loop [res [], n 0, state state]
      (if (< n length)
        (recur
          (conj res (fill-seq forms (demote-vars vars state) mappings))
          (inc n)
          (drop-vars vars state))
        (apply concat res)))))

(defn- fill-form
  [form state mappings]
  ((condp = (first form)
    :variable fill-variable
    :symbol fill-symbol
    :list fill-list
    :vector fill-vector
    :amp fill-amp
    :literal fill-literal)
    form state mappings))

(defn- find-symbols
  ([]
    #{})
  ([form]
    (condp = (first form)
      :variable #{}
      :symbol #{(second form)}
      :list (apply find-symbols (rest form))
      :vector (apply find-symbols (rest form))
      :amp (apply find-symbols (rest (rest form)))
      :literal #{}))
  ([form & forms]
    (reduce into (find-symbols form) (map find-symbols forms))))

(defn- make-mappings
  [syms]
  (let [needed (filter #(or (nil? %) (not (resolve %))) syms)
        mappings (zipmap needed (map gensym needed))]
    (reduce #(assoc %1 %2 %2)
	    mappings '[quote def var recur do if throw try monitor-enter monitor-exit
		       . new set!])))

(defn fill-template
  [form state]
  (fill-form form state (make-mappings (find-symbols form))))
