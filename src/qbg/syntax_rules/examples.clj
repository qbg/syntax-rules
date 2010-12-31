(ns qbg.syntax-rules.examples
  (:use qbg.syntax-rules)
  (:import [java.io StringWriter]))

(defsyntax-rules ex-dotimes []
  (dotimes [i :> c-symbol upper] body ...)
  (let [n (long upper)]
    (loop [i 0]
      (when (< i n)
	body ...
	(recur (unchecked-inc i))))))

(defsyntax-rules ex--> []
  (-> form) form
  (-> form (f args ...)) (f form args ...)
  (-> form f) (f form)
  (-> form f1 fs ...) (-> (-> form f1) fs ...))

(defsyntax-rules ex-with-out-str []
  (with-out-str body ...)
  (let [s (StringWriter.)]
    (binding [*out* s]
      body ...
      (str s))))

(defsyntax-rules ex-defonce []
  (defonce name :> c-symbol expr)
  (let [v (def name)]
    (when-not (.hasRoot v)
      (def name expr))))

(defsyntax-case ex-cond []
  (+describe "even number of clauses"
	     (cond (+head pred expr) ...))
  (loop [preds (reverse (syntax (pred ...)))
	 exprs (reverse (syntax (expr ...)))
	 res nil]
    (if (seq preds)
      (recur (next preds) (next exprs)
	     `(if ~(first preds)
		~(first exprs)
		~res))
      res)))

(defsyntax-rules ex-pvalues []
  (pvalues expr ...)
  (pcalls (fn [] expr) ...))

(defsyntax-rules ex-lazy-cat []
  (lazy-cat seqs ...)
  (concat (lazy-seq seqs) ...))

(defsyntax-class condp-clause []
  "condp clause"
  []
  (+head test-expr :>> result)
  :with type 2
  (+head test-expr result)
  :with type 1)

(defsyntax-case ex-condp []
  (condp pred expr
    clauses :> condp-clause ...
    (+or default (+head)))
  (let [pred-gs (gensym)
	expr-gs (gensym)
	pred (syntax pred)
	expr (syntax expr)
	clauses (reverse (syntax ((clauses.test-expr
				   clauses.type
				   clauses.result) ...)))
	res (if (absent? 'default)
	      `(throw (IllegalArgumentException. (str "No matching clause: " ~expr-gs)))
	      (syntax default))]
    (loop [clauses clauses, res res]
      (if-let [[[texpr type result] & clauses] (seq clauses)]
	(recur clauses
	       (if (= type 1)
		 `(if (~pred-gs ~expr-gs ~texpr) ~result ~res)
		 `(let [pred-val# (~pred-gs ~expr-gs ~texpr)]
		    (if pred-val# (~result pred-val#) ~res))))
	`(let [~pred-gs ~pred
	       ~expr-gs ~expr]
	   ~res)))))
