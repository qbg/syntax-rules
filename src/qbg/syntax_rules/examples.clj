(ns qbg.syntax-rules.examples
  (:use qbg.syntax-rules)
  (:import [java.io StringWriter]))

(defsyntax-rules ex-dotimes
  "Same as (doseq [i (range upper)] body)"
  (dotimes [i :> c-symbol upper] body ...)
  (let [n (long upper)]
    (loop [i 0]
      (when (< i n)
	body ...
	(recur (unchecked-inc i))))))

(defsyntax-rules ex-->
  "Thread form through a number of other expressions"
  (-> form) form
  (-> form (f args ...)) (f form args ...)
  (-> form f) (f form)
  (-> form f1 fs ...) (-> (-> form f1) fs ...))

(defsyntax-rules ex-with-out-str
  "Return a string of all output sent to *out* during the execution of body"
  (with-out-str body ...)
  (let [s (StringWriter.)]
    (binding [*out* s]
      body ...
      (str s))))

(defsyntax-rules ex-defonce
  "Define name to be expr only if name does not have a toplevel definition"
  (defonce name :> c-symbol expr)
  (let [v (def name)]
    (when-not (.hasRoot v)
      (def name expr))))

(defsyntax-case ex-cond
  "See cond"
  (cond (+describe "even number of clauses" (+head pred expr) ...))
  (loop [preds (reverse (syntax (pred ...)))
	 exprs (reverse (syntax (expr ...)))
	 res nil]
    (if (seq preds)
      (recur (next preds) (next exprs)
	     `(if ~(first preds)
		~(first exprs)
		~res))
      res)))

(defsyntax-rules ex-pvalues
  "Evaluate expr in parallel and return the results as a sequence"
  (pvalues expr ...)
  (pcalls (fn [] expr) ...))

(defsyntax-rules ex-lazy-cat
  "See lazy-cat"
  (lazy-cat seqs ...)
  (concat (lazy-seq seqs) ...))

(defn condp-clause
  []
  (syntax-class
   "condp clause"
   [test-expr :>> result (+pattern type 2)]
   [test-expr result (+pattern type 1)]))

(defsyntax-case ex-condp
  "See condp"
  (condp pred expr
    clauses :> condp-clause ...
    (+? default))
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
