(ns qbg.syntax-rules.test.pattern-match
  (:use
    clojure.test
    [qbg.syntax-rules.pattern-match :as pm :reload-all true]))

(deftest test-compile-pattern
  (are [form instr] (= (#'pm/compile-pattern 'form) 'instr)
       (:variable foo) ((:store foo))
       (:literal 5) ((:literal 5))
       (:amp #{a b} (:variable a) (:variable b)) ((:push-vars #{a b}) (:rep (:store a) (:store b) (:collect-vars #{a b})) (:pop-vars 1 #{a b}))
       (:list (:variable a) (:literal 5)) ((:list) (:store a) (:literal 5) (:eos))
       (:vector (:variable a) (:literal 5)) ((:vector) (:store a) (:literal 5) (:eos))
       (:describe "Foobar" (:literal 5)) ((:push-describe "Foobar") (:literal 5) (:pop-describe))))

(defn ms
  [amp-depth val]
  {:amp-depth amp-depth :val val})

(defn s=
  [res target]
  (= target {:vars (:vars target) :good (:good target) :describe (:describe target)}))

(deftest test-match
  (are [pattern form result] (s= (match 'pattern 'form) result)
       (:variable a) 5 {:vars {'a (ms 0 5)} :good true :describe "Bad syntax"}
       (:literal 5) 5 {:vars {} :good true :describe "Bad syntax"}
       (:literal 5) 3 {:vars {} :good false :describe "Bad syntax"}
       (:list (:variable a) (:variable b)) (1 2) {:vars {'a (ms 0 1) 'b (ms 0 2)} :good true :describe "Bad syntax"}
       (:list (:variable a) (:variable b)) [1 2] {:vars {} :good false :describe "Bad syntax"}
       (:vector (:variable a) (:variable b)) [1 2] {:vars {'a (ms 0 1) 'b (ms 0 2)} :good true :describe "Bad syntax"}
       (:vector (:variable a) (:variable b)) (1 2) {:vars {} :good false :describe "Bad syntax"}
       (:list (:amp #{a b} (:variable a) (:variable b))) (1 2 3 4) {:vars {'a (ms 1 [1 3]) 'b (ms 1 [2 4])} :good true :describe "Bad syntax"}
       (:list (:amp #{a b} (:variable a) (:list (:amp #{b} (:variable b))))) (1 (2) 3 (4 5)) {:vars {'a (ms 1 [1 3]) 'b (ms 2 [[2] [4 5]])} :good true :describe "Bad syntax"}))
