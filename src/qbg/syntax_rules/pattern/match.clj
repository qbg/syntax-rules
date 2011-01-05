(ns qbg.syntax-rules.pattern.match
  (:require [qbg.syntax-rules.pattern.primitives :as prim]))

(defn match-variable
  [sym]
  (prim/combine
   (prim/in-progress) (prim/store-variable sym) (prim/forward)
   (prim/pop-describe) (prim/out-progress)))

(defn match-varclass
  [sym varclass]
  (prim/combine
   (prim/store-variable sym) (prim/eval-in-context varclass)))

(defn match-literal
  [lit]
  (prim/assert-literal lit))

(defn match-list
  [& patterns]
  (prim/combine
   (prim/in-progress) (prim/assert-pred seq? "list") (prim/nest)
   (apply prim/combine patterns)
   (prim/assert-end) (prim/unnest) (prim/pop-describe) (prim/forward)
   (prim/out-progress)))

(defn match-vector
  [& patterns]
  (prim/combine
   (prim/in-progress) (prim/assert-pred vector? "vector") (prim/nest)
   (apply prim/combine patterns)
   (prim/assert-end) (prim/unnest) (prim/pop-describe) (prim/forward)
   (prim/out-progress)))

(defn describe
  [mesg pattern]
  (prim/combine
   (prim/push-describe mesg) pattern (prim/pop-describe)
   (prim/phase-out-describe)))

(defn match-ellipsis
  [pattern]
  (prim/combine
   (prim/push-vars) (prim/perform-rep pattern (prim/merge-vars))
   (prim/pop-vars)))

(defn match-head
  [& patterns]
  (apply prim/combine patterns))

(defn match-and
  [& patterns]
  (apply prim/perform-and patterns))

(defn match-or
  [& patterns]
  (apply prim/perform-or patterns))

(defn pattern
  [pattern form-thunk]
  (prim/aside form-thunk pattern))

(defn guard
  [guard-thunk mesg-thunk]
  (prim/guard guard-thunk mesg-thunk))

(defn options
  [& patterns]
  (prim/perform-rep (apply prim/perform-or patterns)))

(defn optional
  [& patterns]
  (prim/perform-or (apply prim/combine patterns) (prim/combine)))

(defn match
  [pattern form]
  (-> (prim/make-state form)
      pattern
      prim/fixup-state))
