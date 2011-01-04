(ns qbg.syntax-rules.test.pattern-parse
  (:use
    clojure.test
    [qbg.syntax-rules.pattern-parse :as pp :reload-all true]))

(defn- fix
  [[x y]]
  [x (:fns y)])

(def stdopts {:literals #{} :ns *ns* :fns {} :n 0})

(deftest test-parse-symbol
  (are [form res fns] (= (fix (#'pp/parse-symbol form stdopts)) [res 'fns])
       'dolists '(:variable dolists) {}
       '.getName '(:literal .getName) {}
       'Exception. '(:literal java.lang.Exception.) {}))

(deftest test-parse-literal
  (are [form res fns] (= (fix (#'pp/parse-literal form stdopts)) [res 'fns])
    1 '(:literal 1) {}
    :foo '(:literal :foo) {}
    {:a 1, :b 2} '(:literal {:a 1, :b 2}) {}))

(deftest test-parse-seq
  (are [form res fns] (= (fix (#'pp/parse-seq 'form stdopts)) [[res] 'fns])
       (:! a) '(:literal a) {}
       (:& [a b c]) '(:head (:variable a) (:variable b) (:variable c)) {}
       (a :> foo ...) '(:amp #{a} (:varclass a 0)) {0 (clojure.core/fn [] (foo))}))

(deftest test-parse-list
  (are [form res fns] (= (fix (#'pp/parse-list form stdopts)) [res 'fns])
       '(a 1 b ...) '(:list (:variable a) (:literal 1) (:amp #{b} (:variable b))) {}
       '(+literal a) '(:literal a) {}
       '(+describe "Foobar" 5) '(:describe "Foobar" (:literal 5)) {}
       '(+describe "Foobar" a ...) '(:describe "Foobar" (:amp #{a} (:variable a))) {}
       '(+var a foo) `(:varclass ~'a 0) {0 (clojure.core/fn [] (foo))} 
       '(+head a b) '(:head (:variable a) (:variable b)) {}
       '(+or a b) '(:or (:variable a) (:variable b)) {}
       '(+and a b) '(:and (:variable a) (:variable b)) {}
       '(+pattern a 5) '(:pattern (:variable a) (:literal 5)) {}
       '(+guard (+ 2 2) "foobar") '(:guard 0 1) {0 (clojure.core/fn [] (+ 2 2)) 1 (clojure.core/fn [] "foobar")}
       '(+code [] (+ 2 2)) '(:code #{} 0) {0 (clojure.core/fn [] (+ 2 2))}
       '(+? foo) '(:or (:head (:variable foo)) (:head)) {}))

(deftest test-parse-vector
  (are [form res fns] (= (fix (#'pp/parse-vector form stdopts)) [res 'fns])
    '[a 1 b ...] '(:vector (:variable a) (:literal 1) (:amp #{b} (:variable b))) {}))

(deftest test-parse-pattern
  (are [form res fns] (= (fix (parse-pattern form stdopts)) [res 'fns])
       1 '(:literal 1) {}
       'b '(:variable b) {}
       '(a b) '(:list (:variable a) (:variable b)) {}
       '[a b] '(:vector (:variable a) (:variable b)) {}
       '((+var a foo) ...) `(:list (:amp #{~'a} (:varclass ~'a 0))) {0 (clojure.core/fn [] (foo))}))

(deftest test-rule-template
  (are [rule template e-rule e-template e-fns]
    (= (build-rule-template rule template [] *ns*) [e-rule e-template 'e-fns])

    '1 '1 '(:literal 1) '(:literal 1) {}

    '(let [(+head var rhs) ...] body) '((fn [var ...] body) rhs ...)
    '(:list (:variable let) (:vector (:amp #{var rhs} (:head (:variable var) (:variable rhs)))) (:variable body))
    '(:list (:list (:variable fn) (:vector (:amp #{var} (:variable var))) (:variable body)) (:amp #{rhs} (:variable rhs)))
    {}))

(deftest test-build-syntax-class
  (are [descript body res fns] (= (build-class-pattern 'descript [] *ns* 'body) '[res fns])
       "foo" ((1 2) :fail-when 5 "bad" (a b) :with c b)
       (:describe "foo"
		  (:or (:head (:list (:literal 1) (:literal 2))
			      (:guard 0 1))
		       (:head (:list (:variable a) (:variable b))
			      (:pattern (:variable c) (:variable b)))))
       {0 (clojure.core/fn [] 5) 1 (clojure.core/fn [] "bad")}))
