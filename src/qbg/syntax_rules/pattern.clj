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
  [description templater literals body]
  (parse/build-class-pattern
   description templater literals body))
