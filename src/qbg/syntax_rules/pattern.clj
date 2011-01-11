(ns qbg.syntax-rules.pattern
  (:require
   [qbg.syntax-rules.pattern.match :as match]
   [qbg.syntax-rules.pattern.parse :as parse]))

(defn match
  [pattern form]
  (match/match pattern form))

(defmacro pattern
  ([literals form]
     (parse/parse form literals))
  ([form]
     `(pattern [] ~form)))

(defn parse-syntax-class
  [description parts]
  `(match/describe
    ~description
    (match/match-or
     ~@(map (fn [p] `(pattern (~'+head ~@p))) parts))))
