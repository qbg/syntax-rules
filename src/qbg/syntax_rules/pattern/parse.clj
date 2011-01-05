(ns qbg.syntax-rules.pattern.parse
  (:require [qbg.syntax-rules.pattern.match :as match]))

(declare parse-pattern parse-seq)

(defn parse-symbol
  [form options]
  (cond
   (contains? (:literals options) form)
   `(match/match-literal '~form)
   :else
   `(match/match-variable '~form)))

(defn- parse-varclass
  [form options]
  (let [[_ variable class & args] form]
    `(match/match-varclass '~variable (fn [] (~class ~@args)))))

(defn parse-literal
  [form options]
  `(match/match-literal '~form))

(defn parse-ellipsis
  [pattern options]
  `(match/match-ellipsis ~pattern))

(defn parse-sugar-head
  [patterns options]
  (let [pats (parse-seq patterns options)]
    `(match/match-head ~@pats)))

(defn parse-sugar-varclass
  [var klass options]
  (let [klass (if (seq? klass) klass (list klass))]
    `(match/match-varclass '~var (fn [] ~klass))))

(defn parse-seq
  [form options]
  (loop [res [], form (seq form), options options]
    (if (seq form)
      (cond
       (= (first form) ':!)
       (let [pat (parse-literal (second form) options)]
	 (recur (conj res pat) (nthnext form 2) options))

       (= (first form) ':&)
       (let [pat (parse-sugar-head (second form) options)] 
	 (recur (conj res pat) (nthnext form 2) options))

       (= (second form) ':>)
       (let [pat (parse-sugar-varclass (first form) (nth form 2) options)]
	 (recur (conj res pat) (nthnext form 3) options))
       
       (= (first form) '...)
       (let [pat (parse-ellipsis (peek res) options)]
	 (recur (conj (pop res) pat) (next form) options))

       :else
       (let [pat (parse-pattern (first form) options)]
	 (recur (conj res pat) (next form) options)))
      res)))

(defn parse-describe
  [form options]
  (let [[_ mesg & pattern] form
	[pat] (parse-seq pattern options)]
    `(match/describe ~mesg ~pat)))

(defn parse-pattern-form
  [form options]
  (let [[_ pattern template] form
	pattern (parse-pattern pattern options)]
    `(match/pattern ~pattern (fn [] ~template))))

(defn parse-guard
  [form options]
  (let [[_ code mesg] form]
    `(match/guard (fn [] ~code) (fn [] ~mesg))))

(defn parse-options
  [form options]
  (let [patterns (map #(parse-seq % options) (rest form))
	patterns (map (fn [p] `(match/match-head ~@p)) patterns)]
    `(match/options ~@patterns)))

(defn parse-list
  [form options]
  (condp = (first form)
      '+literal (parse-literal (second form) options)
      '+describe (parse-describe form options)
      '+var (parse-varclass form options)
      '+head `(match/match-head ~@(parse-seq (next form) options))
      '+and `(match/match-and ~@(parse-seq (next form) options))
      '+or `(match/match-or ~@(parse-seq (next form) options))
      '+c (second form)
      '+pattern (parse-pattern-form form options)
      '+guard (parse-guard form options)
      '+options (parse-options form options)
      '+? `(match/optional ~@(parse-seq (next form) options))
      `(match/match-list ~@(parse-seq form options))))

(defn parse-vector
  [form options]
  `(match/match-vector ~@(parse-seq form options)))

(defn parse-pattern
  [pattern options]
  (cond
    (symbol? pattern) (parse-symbol pattern options)
    (seq? pattern) (parse-list pattern options)
    (vector? pattern) (parse-vector pattern options)
    :else (parse-literal pattern options)))

(defn parse
  [pattern literals]
  (parse-pattern pattern {:literals (set literals)}))

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
  [part templater options]
  (let [res (parse-pattern (first part) options)]
    (loop [res [res], parts (next part)]
      (cond
       (empty? parts) `(match/match-head ~@res)
       
       (= (first parts) :fail-when)
       (let [[_ code mesg & parts] parts
	     guard (parse-guard `(+guard ~code ~mesg) options)]
	 (recur (conj res guard) parts))
       
       (= (first parts) :with)
       (let [[_ pattern template & parts] parts
	     pattern (parse-pattern pattern options)
	     pat `(match/pattern ~pattern ~(templater template))]
	 (recur (conj res pat) parts))))))

(defn- build-parts
  [parts templater options]
  (loop [res [], parts parts]
    (if (seq parts)
      (let [pat (build-part-pattern (first parts) templater options)]
	(recur (conj res pat) (next parts)))
      res)))

(defn build-class-pattern
  [description templater literals body]
  (let [literals (set literals)
	options {:literals literals}
	parts (sc-split body)
	parts-pattern (build-parts parts templater options)]
    `(match/describe ~description (match/match-or ~@parts-pattern))))
