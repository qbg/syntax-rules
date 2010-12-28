(ns qbg.syntax-rules.test.pattern-parse
  (:use
    clojure.test
    [qbg.syntax-rules.pattern-parse :as pp :reload-all true]))

(deftest test-parse-symbol
  (are [form res] (= (#'pp/parse-symbol form {:literals #{} :ns *ns*}) res)
       'dolists '(:variable dolists)))

(deftest test-parse-literal
  (are [form res] (= (#'pp/parse-literal form {:literals #{} :ns *ns*}) res)
    1 '(:literal 1)
    :foo '(:literal :foo)
    {:a 1, :b 2} '(:literal {:a 1, :b 2})))

(deftest test-parse-amp
  (are [form res] (= (#'pp/parse-amp form {:literals #{} :ns *ns*}) res)
    '(& a b) '(:amp #{a b} (:variable a) (:variable b))
    '(& (a b)) '(:amp #{a b} (:list (:variable a) (:variable b)))
    '(& a b ...) '(:amp #{a b} (:variable a) (:amp #{b} (:variable b)))))

(declare foo)

(deftest test-parse-list
  (binding [*ns* (find-ns 'qbg.syntax-rules.test.pattern-parse)]
    (are [form res] (= (#'pp/parse-list form {:literals #{} :ns *ns*}) res)
	 '(a 1 b ...) '(:list (:variable a) (:literal 1) (:amp #{b} (:variable b)))
	 '(+literal a) '(:literal a)
	 '(+describe "Foobar" 5) '(:describe "Foobar" (:literal 5))
	 '(+describe "Foobar" a ...) '(:describe "Foobar" (:amp #{a} (:variable a)))
	 '(+& a) '(:amp #{a} (:variable a))
	 '(+var a foo) `(:varclass ~'a ~#'qbg.syntax-rules.test.pattern-parse/foo)
	 '(+head a b) '(:head (:variable a) (:variable b))
	 '(+or a b) '(:or (:variable a) (:variable b))
	 '(+and a b) '(:and (:variable a) (:variable b))
	 '(+pattern a 5) '(:pattern (:variable a) (:literal 5))
	 '(+guard "foobar" (+ 2 2)) '(:guard "foobar" qbg.syntax-rules.test.pattern-parse (+ 2 2)))))

(deftest test-parse-vector
  (are [form res] (= (#'pp/parse-vector form {:literals #{} :ns *ns*}) res)
    '[a 1 b ...] '(:vector (:variable a) (:literal 1) (:amp #{b} (:variable b)))))

(deftest test-parse-pattern
  (binding [*ns* (find-ns 'qbg.syntax-rules.test.pattern-parse)]
    (are [form res] (= (parse-pattern form {:literals #{} :ns *ns*}) res)
	 1 '(:literal 1)
	 'b '(:variable b)
	 '(a b) '(:list (:variable a) (:variable b))
	 '[a b] '(:vector (:variable a) (:variable b))
	 '((+var a foo) ...) `(:list (:amp #{~'a} (:varclass ~'a ~#'qbg.syntax-rules.test.pattern-parse/foo))))))

(deftest test-rule-template
  (are [rule template e-rule e-template]
    (= (build-rule-template rule template [] *ns*) [e-rule e-template])

    '1 '1 '(:literal 1) '(:literal 1)

    '(let [(+& var rhs)] body) '((fn [var ...] body) rhs ...)
    '(:list (:variable let) (:vector (:amp #{var rhs} (:variable var) (:variable rhs))) (:variable body))
    '(:list (:list (:variable fn) (:vector (:amp #{var} (:variable var))) (:variable body)) (:amp #{rhs} (:variable rhs)))))

(deftest test-build-syntax-class
  (binding [*ns* (find-ns 'qbg.syntax-rules.test.pattern-parse)]
    (are [descript body res] (= (build-class-pattern 'descript [] *ns* 'body) 'res)
	 "foo" ((1 2) :fail-when "bad" 5 (a b) :with c b)
	 (:describe "foo"
		    (:or (:head (:list (:literal 1) (:literal 2))
				(:guard "bad" qbg.syntax-rules.test.pattern-parse 5))
			 (:head (:list (:variable a) (:variable b))
				(:pattern (:variable c) (:variable b))))))))
