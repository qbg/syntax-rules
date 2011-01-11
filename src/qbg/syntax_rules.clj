(ns qbg.syntax-rules
  (:require
   [qbg.syntax-rules.core :as core]
   [qbg.syntax-rules.pattern :as pattern]
   [qbg.syntax-rules.template :as temp]))

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

(defn pred-check
  "Return false if (pred coll) is true, coll otherwise"
  [pred coll]
  (if (pred coll)
    false
    coll))

(defn- pred-core
  [pred mesg]
  (pattern/pattern
   (+describe
    mesg
    (+head form (+guard (pred-check pred (syntax form))
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

(defmacro syntax-class
  [description & parts]
  (pattern/parse-syntax-class description parts))

(defn c-pred
  [pred mesg]
  "The syntax class for an arbitrary predicate. mesg describes what pred will
return true for."
  (syntax-class
   mesg
   [form (+guard (pred-check pred (syntax form))
		 (format "expected %s" mesg))]))

(defn c-symbol
  "Syntax class for a symbol"
  []
  (c-pred symbol? "symbol"))

(defn c-symbol
  "Syntax class for a symbol"
  []
  (c-pred symbol? "symbol"))

(defn c-list
  "Syntax class for a list/seq"
  []
  (c-pred seq? "list"))

(defn c-vector
  "Syntax class for a vector"
  []
  (c-pred vector? "vector"))

(defn c-number
  "Syntax class for a number"
  []
  (c-pred number? "number"))

(defn c-keyword
  "Syntax class for a keyword"
  []
  (c-pred keyword? "keyword"))

(defn c-map
  "Syntax class for a map"
  []
  (c-pred map? "keyword"))

(defn c-set
  "Syntax class for a set"
  []
  (c-pred set? "set"))

(defn c-string
  "Syntax class for a string"
  []
  (c-pred string? "string"))

