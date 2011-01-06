(ns qbg.syntax-rules.pattern.test.match
  (:use
    clojure.test
    [qbg.syntax-rules.pattern.match :only [match] :reload-all true]
    [qbg.syntax-rules.pattern :only [pattern]]))

(defn vars
  [vm]
  (cond
   (vector? vm) {:ell (vec (map vars vm))}
   (map? vm) {:vars (into {} (map (fn [[k v]] [k (vars v)]) vm))}
   :else {:val vm}))
  
(deftest test-match
  (are [pat form result] (= (:vars (match (pattern pat) 'form))
			    (:vars (vars 'result)))
       a 5 {a 5}
       5 5 {}
       [5 2] [5 2] {}
       (a b) (1 2) {a 1 b 2}
       [a b] [1 2] {a 1 b 2}
       (a ...) (1 2 3) {a [1 2 3]}
       ((a ...) ...) ((1 2) (3)) {a [[1 2] [3]]}
       [1 (+head 2 3) 4] [1 2 3 4] {}
       (+and [a] [1]) [1] {a 1}
       (+or [:a a] [:b a]) [:a 5] {a 5}
       (+or [:a a] [:b a]) [:b 5] {a 5}
       [(+pattern a (+ 2 2))] [] {a 4}
       [(+options [:a a] [:b b])] [:a 1 :b 2] {a 1 b 2}
       [(+options [:a a] [:b b])] [:b 2 :a 1] {a 1 b 2}
       [1 (+? 2) 3] [1 3] {}
       [1 (+? 2) 3] [1 2 3] {}
       (_ _) (1 2) {})
  (are [pat form] (= (:good (match (pattern pat) 'form)) false)
       5 3
       (a b) [1 2]
       (a a) [1 2]
       [a b] (1 2)
       (+and [a] [2]) [1] 
       (+head a (+guard 0 "")) 5
       [1 (+? 2) 3] [1 4 3]
       [(+options [:a a] [:b b])] [:b 2 :a 1 :b 2]))
