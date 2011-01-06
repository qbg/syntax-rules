(ns qbg.syntax-rules.pattern.match
  (:require [qbg.syntax-rules.pattern.primitives :as prim]))

(defn match-variable
  "Match a pattern variable sym."
  [sym]
  (prim/combine
   (prim/in-progress) (prim/store-variable sym) (prim/forward)
   (prim/pop-describe) (prim/out-progress)))

(defn match-varclass
  "Match a pattern variable sym whose varclass is given in the varclass thunk."
  [sym varclass]
  (prim/combine
   (prim/store-variable sym) (prim/push-vars) (prim/eval-in-context varclass)
   (prim/pop-nesting-vars sym)))

(defn match-literal
  "Match a literal lit"
  [lit]
  (prim/combine
   (prim/assert-literal lit) (prim/forward)))

(defn- match-seq
  [pred mesg patterns]
  (prim/combine
   (prim/in-progress) (prim/assert-pred pred mesg) (prim/nest)
   (apply prim/combine patterns)
   (prim/assert-end) (prim/unnest) (prim/pop-describe) (prim/forward)
   (prim/out-progress)))

(defn match-list
  "Match a list whose contents match patterns."
  [& patterns]
  (match-seq seq? "list" patterns))

(defn match-vector
  "Match a vector whose contents match patterns."
  [& patterns]
  (match-seq? vector? "vector" patterns))

(defn describe
  "Match pattern, describing pattern as mesg."
  [mesg pattern]
  (prim/combine
   (prim/push-describe mesg) pattern (prim/pop-describe)
   (prim/phase-out-describe)))

(defn match-ellipsis
  "Repeatedly match pattern in an ellipsis."
  [pattern]
  (prim/combine
   (prim/push-vars) (prim/perform-rep pattern (prim/merge-vars))
   (prim/pop-vars)))

(defn match-head
  "Match each pattern in patterns in turn."
  [& patterns]
  (apply prim/combine patterns))

(defn match-and
  "Match all pattern in patterns on top of each other."
  [& patterns]
  (apply prim/perform-and patterns))

(defn match-or
  "Match first successful pattern in patterns."
  [& patterns]
  (apply prim/perform-or patterns))

(defn pattern
  "Match pattern against the result of executing form-thunk."
  [pattern form-thunk]
  (prim/aside form-thunk pattern))

(defn guard
  "If guard-thunk returns a true value, fail blaming the return value with the
message mesg-thunk."
  [guard-thunk mesg-thunk]
  (prim/guard guard-thunk mesg-thunk))

(defn options
  "Match a permutation of a subset of patterns. A pattern can be repeated as
many times as the pattern will match. Useful for optional options."
  [& patterns]
  (prim/perform-rep (apply prim/perform-or patterns)))

(defn optional
  "Match patterns if possible, otherwise consume no input."
  [& patterns]
  (prim/perform-or (apply prim/combine patterns) (prim/combine)))

(defn match
  "Match pattern against form, return the state of the match."
  [pattern form]
  (-> (prim/make-state form)
      pattern
      prim/fixup-state))
