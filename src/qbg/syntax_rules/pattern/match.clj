(ns qbg.syntax-rules.pattern.match
  (:require [qbg.syntax-rules.pattern.primitives :as p]))

(defn match-variable
  "Match a pattern variable sym."
  [sym]
  (p/combine
   (p/in-progress) (p/store-variable sym) (p/forward)
   (p/clear-failure) (p/out-progress)))

(defn match-varclass
  "Match a pattern variable sym whose varclass is given in the varclass thunk."
  [sym varclass]
  (p/combine
   (p/store-variable sym) (p/push-vars) (p/eval-in-context varclass)
   (p/pop-nesting-vars sym)))

(defn match-literal
  "Match a literal lit"
  [lit]
  (p/combine
   (p/assert-literal lit) (p/forward) (p/clear-failure)))

(defn- match-seq
  [pred mesg patterns]
  (p/combine
   (p/in-progress) (p/assert-pred pred mesg) (p/nest)
   (apply p/combine patterns)
   (p/assert-end) (p/clear-failure) (p/unnest) (p/forward) (p/out-progress)))

(defn match-list
  "Match a list whose contents match patterns."
  [& patterns]
  (match-seq seq? "list" patterns))

(defn match-vector
  "Match a vector whose contents match patterns."
  [& patterns]
  (match-seq vector? "vector" patterns))

(defn describe
  "Match pattern, describing pattern as mesg."
  [mesg pattern]
  (p/combine (p/push-describe mesg) pattern (p/pop-describe)))

(defn match-ellipsis
  "Repeatedly match pattern in an ellipsis."
  [pattern]
  (p/combine
   (p/push-vars) (p/perform-rep pattern (p/merge-vars))
   (p/pop-vars)))

(defn match-head
  "Match each pattern in patterns in turn."
  [& patterns]
  (apply p/combine patterns))

(defn match-and
  "Match all pattern in patterns on top of each other."
  [& patterns]
  (apply p/perform-and patterns))

(defn match-or
  "Match first successful pattern in patterns."
  [& patterns]
  (apply p/perform-or patterns))

(defn pattern
  "Match pattern against the result of executing form-thunk."
  [pattern form-thunk]
  (p/aside form-thunk pattern))

(defn guard
  "If guard-thunk returns a true value, fail blaming the return value with the
message mesg-thunk."
  [guard-thunk mesg-thunk]
  (p/guard guard-thunk mesg-thunk))

(defn options
  "Match a permutation of a subset of patterns. A pattern can be repeated as
many times as the pattern will match. Useful for optional options."
  [& patterns]
  (p/perform-rep (apply p/perform-or patterns)))

(defn optional
  "Match patterns if possible, otherwise consume no input."
  [& patterns]
  (p/perform-or (apply p/combine patterns) (p/combine)))

(defn match
  "Match pattern against form, return the state of the match."
  [pattern form]
  (-> (p/make-state form)
      pattern
      p/fixup-state))
