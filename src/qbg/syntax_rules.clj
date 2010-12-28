(ns qbg.syntax-rules
  (:require
    [qbg.syntax-rules.pattern-parse :as pp]
    [qbg.syntax-rules.pattern-match :as pm]
    [qbg.syntax-rules.template-fill :as tf]))

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
  [form rt]
  (let [[rule template] rt]
    (assoc (pm/match rule form)
      :template template)))

(defn syntax-to-form
  "Function version of syntax"
  ([template]
     (syntax-to-form template []))
  ([template literals]
     (let [literals (set literals)
	   template (pp/parse-pattern template literals)
	   res (tf/fill-template template pm/*current-match*)]
       res)))

(defmacro syntax
  "Fill in the template and return it. Must be called in context of a pattern match."
  ([template]
     `(syntax-to-form '~template))
  ([template literals]
     (assert (vector? literals))
     `(syntax-to-form '~template '~literals)))

(defn absent?
  "Return true if variable was not bound in the enclosing match."
  [variable]
  (let [full-name (map symbol (.split (str variable) "\\."))]
    (loop [name full-name, match pm/*current-match*]
      (if (empty? (next name))
	(not (contains? (:vars match) (first name)))
	(recur (next name) (get (:varm match) (first name)))))))

(defn make-apply-rules 
  [name literals rules templates]
  (let [ns *ns*
	rule-templates (map #(pp/build-rule-template %1 %2 literals ns)
			    rules templates)
	file *file*]
    (fn [form]
      (let [results (map (partial perform-match form) rule-templates)
	    line (:line (meta form))]
	(if-let [m (first (filter :good results))]
	  (tf/fill-template (:template m) m)
	  (throw-match-error name results line file))))))

(defmacro defsyntax-rules
  "Define a macro that uses the rule-template pairs to expand all invokations"
  [name literals & rt-pairs]
  (assert (vector? literals))
  (let [rules (take-nth 2 rt-pairs)
        templates (take-nth 2 (rest rt-pairs))]
    `(let [ar# (make-apply-rules '~name '~literals '~rules '~templates)]
       (defmacro ~name
         [& ~'forms]
         (ar# ~'&form)))))

(defmacro defsyntax-class
  "Define a new syntax class"
  [name args description literals & body]
  (let [class-pattern (pp/build-class-pattern description literals *ns* body)]
    `(defn ~name
       ~(format "The %s syntax class" name)
       ~args
       (pm/make-syntax-class '~args ~args '~class-pattern))))

(defn check-duplicate
  [coll]
  (loop [seen #{}, coll coll]
    (if (seq coll)
      (if (contains? seen (first coll))
	(first coll)
	(recur (conj seen (first coll)) (next coll)))
      false)))

(defsyntax-class distinct-bindings []
  "binding vector"
  []
  [(+head var rhs) ...]
  :fail-when "duplicate variable name" (check-duplicate (syntax (var ...))))

(defsyntax-rules plet []
  (plet (+var b distinct-bindings) body ...)
  ((fn [b.var ...] body ...) b.rhs ...))
