(ns qbg.syntax-rules.test.template-fill
  (:use
    clojure.test
    [qbg.syntax-rules.template-fill :as tf :reload-all true]))

(deftest test-fill-literal
  (are [form res] (= (#'tf/fill-literal form {} {}) res)
    '(:literal 1) 1
    '(:literal :foo) :foo))

(deftest test-fill-symbol
  (are [form res] (= (#'tf/fill-variable form {} {'a 'b}) res)
    '(:variable a) 'b
    '(:variable +) 'clojure.core/+))

(deftest test-fill-variable
  (let [s {:vars {'a {:amp-depth 0 :val 5}
		  'b {:amp-depth 0 :val {:vars {'c {:amp-depth 0 :val 6}}
					 :varm #{}}}}
	   :varm #{'b}
	   :fill-stack []}]
    (are [form res] (= (#'tf/fill-variable form s {}) res)
	 '(:variable a) 5
	 '(:variable b.c) 6)))

(deftest test-fill-seq
  (are [form res] (= (#'tf/fill-seq form {:vars {'a {:amp-depth 0 :val 5}}} {'b 'c}) res)
       '((:variable b) (:list (:variable a)) (:vector (:variable a))) '(c (5) [5])))

(deftest test-fill-amp
  (are [form res] (= (#'tf/fill-amp form {:vars {'a {:amp-depth 1 :val [1 2 3]}}
					  :fill-stack []}
				    {'b 'c}) res)
       '(:amp #{a} (:variable a) (:variable b))
       '(1 c 2 c 3 c)

       '(:amp #{a fn} (:list (:variable fn) (:vector) (:variable a)))
       '((clojure.core/fn [] 1) (clojure.core/fn [] 2) (clojure.core/fn [] 3))))

(def samp-match
  {:vars {'a {:amp-depth 1 :val [1 2 3]}
	  'b {:amp-depth 1 :val [{:vars {'c {:amp-depth 1 :val [1 2 3]}} :varm #{}}
				 {:vars {'c {:amp-depth 1 :val [4 5 6]}} :varm #{}}]}}
   :varm #{'b}
   :params {0 (fn [] (+ 2 2))}})

(deftest test-find-syms
  (are [form res] (= (#'tf/find-symbols '{:vars {a {:amp-depth 0 :val 5}}} 'form) 'res)
       (:list (:variable a) (:variable b)) #{b}))

(deftest test-fill-template
  (are [form res] (= (fill-template form samp-match) res)
       '(:vector (:literal 1) (:literal 2) (:amp #{a} (:literal 3) (:variable a))) [1 2 3 1 3 2 3 3]
       '(:variable def) 'def
       '(:variable recur) 'recur
       '(:vector (:amp #{a} (:variable a)) (:amp #{a} (:variable a))) [1 2 3 1 2 3]
       '(:code #{} 0) 4
       
       '(:vector (:amp #{b.c} (:vector (:amp #{b.c} (:variable b.c)))))
       [[1 2 3] [4 5 6]]))
