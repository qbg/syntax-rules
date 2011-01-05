(ns qbg.syntax-rules
  (:require
   [qbg.syntax-rules.core :as core]
   [qbg.syntax-rules.pattern-parse :as pp]
   [qbg.syntax-rules.pattern-match :as pm]
   [qbg.syntax-rules.template :as temp]
   [clojure.template :as t]))

(defn- throw-match-error
  [name results line file]
  (let [res (first (sort-by :progress (comparator pm/order-progress) results))
	mesg (let [d (:describe res)]
	       (if (vector? d)
		 (let [[mesg cause] (:describe res)] 
		   (format "%s in: %s (%s:%d)" mesg cause file line))
		 (format "%s (%s:%d)" d file line)))]
    (throw (Exception. (format "%s: %s" name mesg)))))

(defn- perform-match
  [form rt fns]
  (let [[rule template] rt]
    (assoc (pm/match rule fns form)
      :template template)))

(defn syntax-to-form-evaluating
  "Functional version of syntax for advanced users"
  [template fns]
  (temp/fill-template template fns))

(defn syntax-to-form
  "Simplified functional version of syntax"
  ([template]
     (syntax-to-form template []))
  ([template literals]
     (let [[parsed fns] (temp/parse template literals)
	   fns (zipmap (keys fns) (map eval (vals fns)))]
       (syntax-to-form-evaluating parsed fns))))

(defn- build-pattern
  [pattern literals]
  (let [literals (set literals)
	options {:literals literals :ns *ns* :fns {} :n 0}
	[pattern options] (pp/parse-pattern pattern options)
	fns (:fns options)]
    [pattern fns]))

(defmacro syntax
  "Fill in the template and return it. Must be called in context of a pattern match."
  ([template]
     `(syntax ~template []))
  ([template literals]
     (assert (vector? literals))
     (let [[template fns] (temp/parse template literals)]
       `(syntax-to-form-evaluating '~template ~fns))))

(defn absent?
  "Return true if variable was not bound in the enclosing match."
  [variable]
  (not (temp/contains-var? variable)))

(defn make-apply-cases
  [name rules thunks fns]
  (let [file *file*]
    (fn [form]
      (let [rt (map vector rules thunks)
	    results (map (partial perform-match form) rt fns)
	    line (:line (meta form))]
	(if-let [m (first (filter :good results))]
	  (binding [core/*current-match* m]
	    ((:template m)))
	  (throw-match-error name results line file))))))

(defmacro defsyntax-case
  [name docstring literals & rt-pairs]
  (assert (vector? literals))
  (let [rules (take-nth 2 rt-pairs)
	thunks (take-nth 2 (rest rt-pairs))
	thunkify (fn [c] `(fn [] ~c))
	thunks (vec (map thunkify thunks))
	options {:literals literals :fns {} :ns *ns* :n 0}
	ros (map #(pp/parse-pattern % options) rules)
	rules (map first ros)
	fns (vec (map (comp :fns second) ros))]
    `(let [ac# (make-apply-cases '~name '~rules ~thunks ~fns)]
       (defmacro ~name
	 ~docstring
	 {:arglists '~rules}
	 [& ~'forms]
	 (ac# ~'&form)))))

(defmacro defsyntax-rules
  "Define a macro that uses the rule-template pairs to expand all invokations"
  [name docstring literals & rt-pairs]
  (assert (vector? literals))
  (let [rules (take-nth 2 rt-pairs)
        templates (take-nth 2 (rest rt-pairs))
	cases (map (fn [t] `(syntax ~t ~literals)) templates)
	rt-pairs (interleave rules cases)]
    `(defsyntax-case ~name
       ~docstring
       ~literals
       ~@rt-pairs)))

(defmacro defsyntax-class
  "Define a new syntax class"
  [name args description literals & body]
  (let [[class-pattern fns] (pp/build-class-pattern description literals *ns* body)]
    `(defn ~name
       ~(format "The %s syntax class" name)
       ~args
       (pm/make-syntax-class ~fns '~class-pattern))))

(defn check-duplicate
  "Return the duplicate item in coll if there is one, or false"
  [coll]
  (loop [seen #{}, coll coll]
    (if (seq coll)
      (if (contains? seen (first coll))
	(first coll)
	(recur (conj seen (first coll)) (next coll)))
      false)))

(defn pred-check
  "Return false if (pred coll) is true, coll otherwise"
  [pred coll]
  (if (pred coll)
    false
    coll))

(t/do-template
 [name main-descript descript pred]
 (defsyntax-class name []
   main-descript
   []
   form
   :fail-when (pred-check pred (syntax form)) descript)

 c-symbol "symbol" "expected symbol" symbol?
 c-number "number" "expected number" number?
 c-keyword "keyword" "expected keyword" keyword?
 c-map "map" "expected map" map?
 c-set "set" "expected set" set?
 c-string "string" "expected string" string?)

(defsyntax-class c-pred [pred mesg]
  "form satsifying predicate"
  []
  form
  :fail-when
  (pred-check pred (syntax form))
  (format "expected %s" mesg))
