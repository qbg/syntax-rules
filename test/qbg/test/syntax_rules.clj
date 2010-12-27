(ns qbg.test.syntax-rules
  (:use
    clojure.test
    [qbg.syntax-rules :as sr :reload-all true]))

(def ar
  (make-apply-rules
   'let
   []
   '[(let [(+& var rhs)] body ...)]
   '[((fn [var ...] body ...) rhs ...)]))

(deftest test-apply-rules
  (are [form res] (= (ar form) res)
    '(let [a 1 b 2] (+ a b))
    '((clojure.core/fn [a b] (+ a b)) 1 2)))

(deftest test-order-progress
  (are [a b r] (= (#'sr/order-progress a b) r)
       [2] [1] true
       [0 1] [1] false
       [1] [0 1] true
       [0 1 0] [0 2] false))
