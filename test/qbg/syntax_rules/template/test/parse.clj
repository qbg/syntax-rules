(ns qbg.syntax-rules.test.pattern-parse
  (:use
    clojure.test
    [qbg.syntax-rules.pattern-parse :reload-all true]))

(defn- fix
  [[x y]]
  [x (:fns y)])

(def stdopts {:literals #{} :ns *ns* :fns {} :n 0})

(deftest test-parse-symbol
  (are [form res fns] (= (fix (parse-symbol form stdopts)) [res 'fns])
       'dolists '(:variable dolists) {}
       '.getName '(:literal .getName) {}
       'Exception. '(:literal java.lang.Exception.) {}))

(deftest test-parse-literal
  (are [form res fns] (= (fix (parse-literal form stdopts)) [res 'fns])
    1 '(:literal 1) {}
    :foo '(:literal :foo) {}
    {:a 1, :b 2} '(:literal {:a 1, :b 2}) {}))

(deftest test-parse-seq
  (are [form res fns] (= (fix (parse-seq 'form stdopts)) [[res] 'fns])
       (:! a) '(:literal a) {}
       (:& [a b c]) '(:head (:variable a) (:variable b) (:variable c)) {}))

(deftest test-parse-list
  (are [form res fns] (= (fix (parse-list form stdopts)) [res 'fns])
       '(a 1 b ...) '(:list (:variable a) (:literal 1) (:amp #{b} (:variable b))) {}
       '(+literal a) '(:literal a) {}
       '(+head a b) '(:head (:variable a) (:variable b)) {}
       '(+code [] (+ 2 2)) '(:code #{} 0) {0 (clojure.core/fn [] (+ 2 2))}))

(deftest test-parse-vector
  (are [form res fns] (= (fix (parse-vector form stdopts)) [res 'fns])
    '[a 1 b ...] '(:vector (:variable a) (:literal 1) (:amp #{b} (:variable b))) {}))

(deftest test-parse-pattern
  (are [form res fns] (= (fix (parse-pattern form stdopts)) [res 'fns])
       1 '(:literal 1) {}
       'b '(:variable b) {}
       '(a b) '(:list (:variable a) (:variable b)) {}
       '[a b] '(:vector (:variable a) (:variable b)) {}))
