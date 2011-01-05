(ns qbg.syntax-rules.pattern.test.match
  (:use
    clojure.test
    [qbg.syntax-rules.pattern.match :reload-all true]))

(defn s=
  [res target]
  (= target {:vars (:vars target) :good (:good target)}))

(def match-baseline {:vars {} :good true})

(deftest test-match
  (are [pattern form result] (s= (match pattern 'form) (merge match-baseline result))
       (match-variable 'a) 5 {:vars {'a {:val 5}}}
       (match-literal 5) 5 {}
       (match-literal 5) 3 {:good false}
       
       (match-list (match-variable 'a) (match-variable 'b))
       (1 2)
       {:vars {'a {:val 1} 'b {:val 2}}}

       (match-vector (match-variable 'a) (match-variable 'b))
       [1 2]
       {:vars {'a {:val 1} 'b {:val 2}}}

       (match-list (match-variable 'a) (match-variable 'b))
       [1 2]
       {:good false}

       (match-vector (match-variable 'a) (match-variable 'b))
       (1 2)
       {:good false}

       (match-list (match-ellipsis (match-variable 'a)))
       (1 2 3)
       {:vars {'a {:ell [{:val 1} {:val 2} {:val 3}]}}}

       (match-list (match-ellipsis (match-list (match-ellipsis (match-variable 'a)))))
       ((1 2) (3))
       {:vars {'a {:ell [{:ell [{:val 1} {:val 2}]} {:ell [{:val 3}]}]}}}

       (match-head (match-variable 'a) (guard (fn [] 0) (fn [] "")))
       5
       {:good false}))

