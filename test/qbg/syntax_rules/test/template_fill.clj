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
  (let [s {:vars {'a {:amp-depth 0 :val 5}}
	   :varm {'b {:vars {'c {:amp-depth 0 :val 6}}}}}]
    (are [form res] (= (#'tf/fill-variable form s {}) res)
	 '(:variable a) 5
	 '(:variable b c) 6)))

(deftest test-fill-seq
  (are [form res] (= (#'tf/fill-seq form {:vars {'a {:amp-depth 0 :val 5}}} {'b 'c}) res)
       '((:variable b) (:list (:variable a)) (:vector (:variable a))) '(c (5) [5])))

(deftest test-fill-amp
  (are [form res] (= (#'tf/fill-amp form {:vars {'a {:amp-depth 1 :val [1 2 3]}}} {'b 'c}) res)
    '(:amp #{a} (:variable a) (:variable b)) '(1 c 2 c 3 c)))

(deftest test-fill-template
  (are [form res] (= (fill-template form {:vars {'a {:amp-depth 1 :val [1 2 3]}}}) res)
       '(:vector (:literal 1) (:literal 2) (:amp #{a} (:literal 3) (:variable a))) [1 2 3 1 3 2 3 3]
       '(:variable def) 'def
       '(:variable recur) 'recur
       '(:vector (:amp #{a} (:variable a)) (:amp #{a} (:variable a))) [1 2 3 1 2 3]))
