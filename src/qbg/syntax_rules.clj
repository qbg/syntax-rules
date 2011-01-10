(ns qbg.syntax-rules
  (:require
   [qbg.syntax-rules.core :as core]
   [qbg.syntax-rules.pattern :as pattern]
   [qbg.syntax-rules.template :as temp]
   [clojure.template :as t]))

(defn- throw-match-error
  [name results line file]
  (let [res (first (sort-by :progress (comparator core/order-progress) results))
	mesg (let [d (:describe res)]
	       (if (vector? d)
		 (let [[mesg cause] (:describe res)] 
		   (format "%s in: %s (%s:%d)" mesg cause file line))
		 (format "%s (%s:%d)" d file line)))]
    (throw (Exception. (format "%s: %s" name mesg)))))

(defn- perform-match
  [form rt]
  (let [[rule template] rt]
    (assoc (pattern/match rule form)
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
  "Return a fn that will try each rule in turn on its argument. Upon the first
success, call the corresponding thunk with the match result implicitly. If none
match, throw an appropriate exception, using name to describe the source of the
error."
  [name rules thunks]
  (let [file *file*]
    (fn [form]
      (let [rt (map vector rules thunks)
	    results (map (partial perform-match form) rt)
	    line (:line (meta form))]
	(if-let [m (first (filter :good results))]
	  (binding [core/*current-match* m]
	    ((:template m)))
	  (throw-match-error name results line file))))))

(declare c-symbol c-string c-vector)

(defn- pred-core
  [pred mesg]
  (pattern/pattern
   (+describe
    mesg
    (+head form
	   (+guard (let [form (syntax form)]
		     (if (pred form)
		       false
		       form))
		   (format "expected %s" mesg))))))

(defn- rule-code-pair
  []
  (pattern/pattern
   (+describe
    "rule-code pair"
    (+head rule code))))

(def ^{:private true} syntax-case-core
  (make-apply-cases
   'defsyntax-case
   [(pattern/pattern
     (defsyntax-case
       name :> (pred-core symbol? "symbol")
       (+? doc :> (pred-core string? "string"))
       (+? literals :> (pred-core vector? "vector"))
       pairs :> rule-code-pair ...))]
   [(fn []
      (let [attr {}
	    attr (if (absent? 'doc) attr (conj attr {:doc (syntax doc)}))
	    attr (conj attr {:arglists (syntax '(pairs.rule ...))})
	    literals (if (absent? 'literals) [] (syntax literals))]
	(syntax
	 (let [ac (make-apply-cases
		   'name
		   [(pattern/pattern
		     (+code [] literals)
		     pairs.rule) ...]
		   [(fn []
		      pairs.code) ...])]
	   (defmacro name
	     (+code [] attr)
	     [:! & :! forms]
	     (ac :! &form))))))]))

(defmacro defsyntax-case
  {:doc "syntax-case version of defsyntax-rules"
   :arglists '((defsyntax-case
		name
		(+? doc :> c-string)
		(+? literals :> c-vector)
		pairs :> rule-code-pair ...))}
  [& forms]
  (syntax-case-core &form))

(defn- rule-template-pair
  []
  (pattern/pattern
   (+describe
    "rule-code pair"
    (+head rule template))))

(defn- maybe-expand
  [sym]
  (if (absent? sym)
    []
    [(syntax-to-form sym)]))

(defsyntax-case defsyntax-rules
  "Define a macro that uses the rule-template pairs to expand all invokations"
  (defsyntax-rules
    name :> c-symbol
    (+? doc :> c-string)
    (+? literals :> c-vector)
    pairs :> rule-template-pair ...)
  (let [lits (if (absent? 'literals) [] (syntax literals))]
    (syntax
     (defsyntax-case name
       (+scode [] (maybe-expand 'doc))
       (+scode [] (maybe-expand 'literals))
       :& [pairs.rule (syntax pairs.template (+code [] lits))] ...))))

(defmacro defsyntax-class
  "Define a new syntax class"
  [name args description literals & body]
  (let [temp (fn [form] `(syntax ~form ~literals))
	pat (pattern/parse-syntax-class description temp literals body)]
    `(defn ~name
       ~(format "The %s syntax class" name)
       ~args
       ~pat)))

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
 c-list "list" "expected list" seq?
 c-vector "vector" "expected vector" vector?
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
