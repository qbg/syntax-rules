(ns qbg.syntax-rules.test.pattern-match
  (:use
    clojure.test
    [qbg.syntax-rules.pattern-match :as pm :reload-all true]))

(defn ms
  [amp-depth val]
  {:amp-depth amp-depth :val val})

(defn s=
  [res target]
  (= target {:vars (:vars target) :good (:good target)}))

(def match-baseline {:vars {} :good true})

(deftest test-match
  (are [pattern form result fns] (s= (match pattern fns 'form) (merge match-baseline result))
       '(:variable a) 5 {:vars {'a (ms 0 5)}} {}
       '(:literal 5) 5 {} {}
       '(:literal 5) 3 {:good false} {}
       '(:list (:variable a) (:variable b)) (1 2) {:vars {'a (ms 0 1) 'b (ms 0 2)}} {}
       '(:list (:variable a) (:variable b)) [1 2] {:good false} {}
       '(:vector (:variable a) (:variable b)) [1 2] {:vars {'a (ms 0 1) 'b (ms 0 2)}} {}
       '(:vector (:variable a) (:variable b)) (1 2) {:good false} {}
       '(:list (:amp #{a b} (:variable a) (:variable b))) (1 2 3 4) {:vars {'a (ms 1 [1 3]) 'b (ms 1 [2 4])}} {}
       '(:list (:amp #{a b} (:variable a) (:list (:amp #{b} (:variable b))))) (1 (2) 3 (4 5)) {:vars {'a (ms 1 [1 3]) 'b (ms 2 [[2] [4 5]])}} {}
       '(:and (:variable a) (:variable b)) 5 {:vars {'a (ms 0 5) 'b (ms 0 5)}} {}
       '(:list (:variable a) (:pattern (:variable b) (:variable a))) (5) {:vars {'a (ms 0 5) 'b (ms 0 5)}} {}

       '(:list (:head (:variable a) (:pattern (:variable b) (:variable a))))
       (5)
       {:vars {'a (ms 0 5) 'b (ms 0 5)}}
       {}

       '(:list (:pattern (:variable a) (:code #{} 0)))
       ()
       {:vars {'a (ms 0 4)}}
       {0 (clojure.core/fn [] (+ 2 2))}

       '(:head (:variable a) (:guard 0 1))
       5
       {:good false}
       {0 (clojure.core/fn [] true), 1 (clojure.core/fn [] "")}

       '(:list (:only (:amp #{} (:literal 1))) (:literal 2))
       '(1 1 1 2)
       {:good false}
       {}))

(deftest test-order-progress
  (are [a b r] (= (#'pm/order-progress a b) r)
       [2] [1] true
       [0 1] [1] false
       [1] [0 1] true
       [0 1 0] [0 2] false
       [0 :late] [0] true
       [0 :late] [1] false
       [0 :late] [0 5] true))
