(ns qbg.syntax-rules.test.pattern-match
  (:use
    clojure.test
    [qbg.syntax-rules.pattern-match :as pm :reload-all true]))

(defn ms
  [amp-depth val]
  {:amp-depth amp-depth :val val})

(deftest test-merge-symbols
  (are [s1 s2 res] (= (#'pm/merge-symbols s1 s2) res)
    (ms 1 [5]) (ms 0 6) (ms 1 [5 6])))

(deftest test-merge-states
  (are [s1 s2 res] (= (#'pm/merge-states s1 s2) res)
    {'a (ms 1 [5])} {'a (ms 0 6)}
    {'a (ms 1 [5 6])}
    
    {'a (ms 0 5)} {'b (ms 0 6)}
    {'a (ms 0 5) 'b (ms 0 6)}
    
    false {'b (ms 0 6)}
    false
    
    {'a (ms 0 5)} false
    false))

(deftest test-promote
  (are [state result] (= (#'pm/promote state) result)
    false false
    {'a (ms 0 1)} {'a (ms 1 [1])}))
  
(deftest test-match-symbol
  (are [pattern form result] (= (#'pm/match-symbol pattern form) result)
    '(:variable a) 5 {'a (ms 0 5)}
    '(:variable _) 5 {}))

(deftest test-match-literal
  (are [pattern form result] (= (#'pm/match-literal pattern form) result)
    '(:literal 5) 5 {}
    '(:literal 5) 3 false))

(deftest test-match-list
  (are [pattern form result] (= (#'pm/match-list pattern form) result)
    '(:list (:variable a) (:variable b)) '(1 2) {'a (ms 0 1) 'b (ms 0 2)}
    '(:list (:variable a) (:variable b)) '[1 2] false
    '(:list (:variable a) (:variable b)) '(1 2 3) false))

(deftest test-match-vector
  (are [pattern form result] (= (#'pm/match-vector pattern form) result)
    '(:vector (:variable a) (:variable b)) '(1 2) false
    '(:vector (:variable a) (:variable b)) '[1 2] {'a (ms 0 1) 'b (ms 0 2)}
    '(:vector (:variable a) (:variable b)) '[1 2 3] false))

(deftest test-match-amp
  (are [pattern form result] (= (#'pm/match-amp pattern form) result)
    '(:amp #{a b} (:variable a) (:variable b)) '(1 2 3 4) {'a (ms 1 [1 3]) 'b (ms 1 [2 4])}
    '(:amp #{a b} (:variable a) (:variable b)) '(1 2 3) false))
