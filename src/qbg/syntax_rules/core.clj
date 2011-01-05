(ns qbg.syntax-rules.core)

(def *current-match*)

(defn order-progress
  [p1 p2]
  (cond
   (and (empty? p1) (empty? p2)) false
   (and (empty? p1) (seq p2)) false
   (and (seq? p1) (empty? p2)) true
   (= (first p1) (first p2)) (recur (next p1) (next p2))
   (= (first p1) :late) true
   (= (first p2) :late) false
   (> (first p1) (first p2)) true
   :else false))
