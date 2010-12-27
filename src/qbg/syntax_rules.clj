(ns qbg.syntax-rules
  (:require
    [qbg.syntax-rules.pattern-parse :as pp]
    [qbg.syntax-rules.pattern-match :as pm]
    [qbg.syntax-rules.template-fill :as tf]))

(defn- order-progress
  [p1 p2]
  (cond
   (and (empty? p1) (empty? p2)) false
   (and (empty? p1) (seq p2)) false
   (and (seq? p1) (empty? p2)) true
   (= (first p1) (first p2)) (recur (next p1) (next p2))
   (= (first p1) :late) true
   (= (first p2) :late) false
   (> (first p1) (first p2)) true
   :else false))

(defn- throw-match-error
  [name results line file]
  (let [res (first (sort-by :progress (comparator order-progress) results))
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
  (let [rule-templates (map #(pp/build-rule-template %1 %2 literals)
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
