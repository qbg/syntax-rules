(ns qbg.syntax-rules.template.test.fill
  (:use
   clojure.test
   [qbg.syntax-rules.template.fill :reload-all true]))

(deftest test-fill-literal
  (are [form res] (= (fill-literal 'form {} {}) 'res)
    (:literal 1) 1
    (:literal :foo) :foo))

(deftest test-fill-symbol
  (are [form res] (= (fill-variable 'form {} {'a 'b, '+ 'clojure.core/+}) 'res)
    (:variable a) b
    (:variable +) clojure.core/+))

(deftest test-fill-variable
  (let [s {:vars {'a {:val 5} 'b {:vars {'c {:val 6}}}}}]
    (are [form res] (= (fill-variable 'form s {}) 'res)
	 (:variable a) 5
	 (:variable b.c) 6)))

(deftest test-fill-seq
  (are [form res] (= (fill-seq 'form {:vars {'a {:val 5}}} {'b 'c}) 'res)
       ((:variable b) (:list (:variable a)) (:vector (:variable a))) (c (5) [5])))

(deftest test-fill-amp
  (are [form res] (= (fill-amp 'form {:vars {'a {:ell [{:val 1} {:val 2} {:val 3}]}}
				     :fill-stack []}
			       {'b 'c, 'fn 'clojure.core/fn}) 'res)
       (:amp #{a} (:variable a) (:variable b)) (1 c 2 c 3 c)

       (:amp #{a fn} (:list (:variable fn) (:vector) (:variable a)))
       ((clojure.core/fn [] 1) (clojure.core/fn [] 2) (clojure.core/fn [] 3))))

(def samp-match
  {:vars {'a {:ell [{:val 1} {:val 2} {:val 3}]}
	  'b {:ell [{:vars {'c {:ell [{:val 1} {:val 2} {:val 3}]}}}
		    {:vars {'c {:ell [{:val 4} {:val 5} {:val 6}]}}}]}}
   :params {0 (fn [] (+ 2 2))}})

(deftest test-find-syms
  (are [form res] (= (find-symbols '{:vars {a {:val 5}}} 'form) 'res)
       (:list (:variable a) (:variable b)) #{b}))

(deftest test-fill-template
  (are [form res] (= (fill-template 'form samp-match) 'res)
       (:vector (:literal 1) (:literal 2) (:amp #{a} (:literal 3) (:variable a))) [1 2 3 1 3 2 3 3]
       (:variable def) def
       (:variable recur) recur
       (:vector (:amp #{a} (:variable a)) (:amp #{a} (:variable a))) [1 2 3 1 2 3]
       (:code #{} 0) 4
       
       (:vector (:amp #{b.c} (:vector (:amp #{b.c} (:variable b.c)))))
       [[1 2 3] [4 5 6]]))
