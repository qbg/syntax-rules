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
       (:vector (:variable a) (:literal 5)) ((:vector) (:store a) (:literal 5) (:eos))))

(deftest test-store
  (are [cmd state tgt] (= (#'pm/exe-store 'cmd 'state) 'tgt)
       (:store a) {:vars {} :input [5]} {:vars {a {:val [5]}} :input nil}))

(deftest test-literal
  (are [cmd state tgt] (= (#'pm/exe-literal 'cmd 'state) 'tgt)
       (:literal 5) {:input [5]} {:input nil}
       (:literal 5) {:input [4]} {:input [4] :good false}))

(deftest test-push-vars
  (are [cmd state tgt] (= (#'pm/exe-push-vars 'cmd 'state) 'tgt)
       (:push-vars #{a b}) {:vars {a {:val [nil]} b {:val [nil]}}} {:vars {a {:val [[] nil]} b {:val [[] nil]}}}))

(deftest test-collect-vars
  (are [cmd state tgt] (= (#'pm/exe-collect-vars 'cmd 'state) 'tgt)
       (:collect-vars #{a b}) {:vars {a {:val [[] 5]} b {:val [[] 6]}}} {:vars {a {:val [[5] nil]} b {:val [[6] nil]}}}))

(defn ms
  [amp-depth val]
  {:amp-depth amp-depth :val val})

(deftest test-match
  (are [pattern form result] (= (match 'pattern 'form) result)
       (:variable a) 5 {:vars {'a (ms 0 5)} :good true}
       (:literal 5) 5 {:vars {} :good true}
       (:literal 5) 3 {:vars {} :good false}
       (:list (:variable a) (:variable b)) (1 2) {:vars {'a (ms 0 1) 'b (ms 0 2)} :good true}
       (:list (:variable a) (:variable b)) [1 2] {:vars {} :good false}
       (:vector (:variable a) (:variable b)) [1 2] {:vars {'a (ms 0 1) 'b (ms 0 2)} :good true}
       (:vector (:variable a) (:variable b)) (1 2) {:vars {} :good false}
       (:list (:amp #{a b} (:variable a) (:variable b))) (1 2 3 4) {:vars {'a (ms 1 [1 3]) 'b (ms 1 [2 4])} :good true}
       (:list (:amp #{a b} (:variable a) (:list (:amp #{b} (:variable b))))) (1 (2) 3 (4 5)) {:vars {'a (ms 1 [1 3]) 'b (ms 2 [[2] [4 5]])} :good true}))
