(ns qbg.syntax-rules.test.pattern-parse
  (:use
    clojure.test
    [qbg.syntax-rules.pattern-parse :as pp :reload-all true]))

(deftest test-parse-symbol
  (are [form res] (= (#'pp/parse-symbol form) res)
       'dolists '(:variable dolists)
       'foo.bar '(:variable foo bar)
       'foo.bar.baz '(:variable foo bar baz)))

(deftest test-parse-literal
  (are [form res] (= (#'pp/parse-literal form) res)
    1 '(:literal 1)
    :foo '(:literal :foo)
    {:a 1, :b 2} '(:literal {:a 1, :b 2})))

(deftest test-parse-amp
  (are [form res] (= (#'pp/parse-amp form) res)
    '(& a b) '(:amp #{a b} (:variable a) (:variable b))
    '(& (a b)) '(:amp #{a b} (:list (:variable a) (:variable b)))
    '(& a b ...) '(:amp #{a b} (:variable a) (:amp #{b} (:variable b)))))

(declare foo)

(deftest test-parse-list
  (binding [*ns* (find-ns 'qbg.syntax-rules.test.pattern-parse)]
    (are [form res] (= (#'pp/parse-list form) res)
	 '(a 1 b ...) '(:list (:variable a) (:literal 1) (:amp #{b} (:variable b)))
	 '(+literal a) '(:literal a)
	 '(+describe "Foobar" 5) '(:describe "Foobar" (:literal 5))
	 '(+describe "Foobar" a ...) '(:describe "Foobar" (:amp #{a} (:variable a)))
	 '(+& a) '(:amp #{a} (:variable a))
	 '(+var a foo) `(:varclass ~'a ~#'qbg.syntax-rules.test.pattern-parse/foo)
	 '(+head a b) '(:head (:variable a) (:variable b))
	 '(+or a b) '(:or (:variable a) (:variable b))
	 '(+and a b) '(:and (:variable a) (:variable b)))))

(deftest test-parse-vector
  (are [form res] (= (#'pp/parse-vector form) res)
    '[a 1 b ...] '(:vector (:variable a) (:literal 1) (:amp #{b} (:variable b)))))

(deftest test-parse-pattern
  (binding [*ns* (find-ns 'qbg.syntax-rules.test.pattern-parse)]
    (are [form res] (= (parse-pattern form) res)
	 1 '(:literal 1)
	 'b '(:variable b)
	 '(a b) '(:list (:variable a) (:variable b))
	 '[a b] '(:vector (:variable a) (:variable b))
	 '((+var a foo) ...) `(:list (:amp #{~'a} (:varclass ~'a ~#'qbg.syntax-rules.test.pattern-parse/foo))))))

(deftest test-convert-vars
  (are [form vars res] (= (#'pp/convert-vars form vars '#{foobar}) res)
    '(:variable dolists) '#{dolists} '(:variable dolists)
    '(:variable a) '#{dolists} '(:symbol a)
    '(:literal 1) '#{dolists} '(:literal 1)
    '(:amp #{a b} (:variable a) (:variable b)) '#{a} '(:amp #{a} (:variable a) (:symbol b))
    '(:amp #{a b} (:list (:variable a) (:variable b))) '#{a} '(:amp #{a} (:list (:variable a) (:symbol b)))
    '(:list (:variable a) (:amp #{b} (:variable b))) '#{b} '(:list (:symbol a) (:amp #{b} (:variable b)))
    '(:vector (:variable a) (:amp #{b} (:variable b))) '#{b} '(:vector (:symbol a) (:amp #{b} (:variable b)))
    '(:variable foobar) '(:literal foobar)))

(deftest test-rule-template
  (are [rule template e-rule e-template]
    (= (build-rule-template rule template []) [e-rule e-template])
    
    '1 '1 '(:literal 1) '(:literal 1)
    
    '(let [(+& var rhs)] body) '((fn [var ...] body) rhs ...)
    '(:list (:variable let) (:vector (:amp #{var rhs} (:variable var) (:variable rhs))) (:variable body))
    '(:list (:list (:symbol fn) (:vector (:amp #{var} (:variable var))) (:variable body)) (:amp #{rhs} (:variable rhs)))))
