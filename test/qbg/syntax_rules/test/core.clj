(ns qbg.syntax-rules.test.core
  (:use
    clojure.test
    [qbg.syntax-rules.core :reload-all true]))

(deftest test-order-progress
  (are [a b r] (= (order-progress a b) r)
       [2] [1] true
       [0 1] [1] false
       [1] [0 1] true
       [0 1 0] [0 2] false
       [0 :late] [0] true
       [0 :late] [1] false
       [0 :late] [0 5] true))
