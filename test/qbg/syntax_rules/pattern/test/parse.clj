(ns qbg.syntax-rules.pattern.test.parse
  (:use
   clojure.test
   [qbg.syntax-rules.pattern.parse :reload-all true])
  (:require
   [qbg.syntax-rules.pattern.match :as match]))

(def stdopts {:literals #{'literal}})

(deftest test-parse-symbol
  (are [form res] (= (parse-symbol 'form stdopts) res)
       dolists `(match/match-variable '~'dolists)
       literal `(match/match-literal '~'literal)))

(deftest test-parse-literal
  (are [form res] (= (parse-literal 'form stdopts) res)
    1 `(match/match-literal '1)
    :foo `(match/match-literal ':foo)
    {:a 1, :b 2} `(match/match-literal '{:a 1, :b 2})))

(deftest test-parse-seq
  (are [form res] (= (parse-seq 'form stdopts) [res])
       (:! a) `(match/match-literal '~'a)
       (:& [a b c]) `(match/match-head (match/match-variable '~'a)
				       (match/match-variable '~'b)
				       (match/match-variable '~'c))
       (a :> foo ...) `(match/match-ellipsis (match/match-varclass
					      '~'a (fn [] (~'foo))))))

(deftest test-parse-list
  (are [form res] (= (parse-list 'form stdopts) res)
       (a 1 b ...) `(match/match-list (match/match-variable '~'a)
				      (match/match-literal '1)
				      (match/match-ellipsis
				       (match/match-variable '~'b)))
       (+literal a) `(match/match-literal '~'a)
       (+describe "Foobar" 5) `(match/describe "Foobar" (match/match-literal '5))
       (+var a foo) `(match/match-varclass '~'a (fn [] (~'foo)))
       (+head a b) `(match/match-head (match/match-variable '~'a)
				      (match/match-variable '~'b))
       (+or a b) `(match/match-or (match/match-variable '~'a)
				      (match/match-variable '~'b))
       (+and a b) `(match/match-and (match/match-variable '~'a)
				      (match/match-variable '~'b))
       (+guard (+ 2 2) "foobar") `(match/guard (fn [] (~'+ 2 2)) (fn [] "foobar"))
       (+? foo) `(match/optional (match/match-variable '~'foo))))

(deftest test-parse-vector
  (are [form res] (= (parse-vector 'form stdopts) res)
    [a 1 b ...] `(match/match-vector (match/match-variable '~'a)
				     (match/match-literal '1)
				     (match/match-ellipsis
				      (match/match-variable '~'b)))))

(are [descript body res] (= (build-class-pattern 'descript identity [] 'body) res)
     "foo" ((1 2) :fail-when 5 "bad" (a b) :with c b)
     `(match/describe "foo"
		      (match/match-or
		       (match/match-head (match/match-list (match/match-literal '1)
							   (match/match-literal '2))
					 (match/guard (fn [] 5) (fn [] "bad")))
		       (match/match-head (match/match-list (match/match-variable '~'a)
							   (match/match-variable '~'b))
					 (match/pattern (match/match-variable '~'c)
							~'b)))))
