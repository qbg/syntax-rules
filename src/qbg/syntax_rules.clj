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
   (> (first p1) (first p2)) true
   :else false))

(defn- throw-match-error
  [name results line file]
  (let [res (first (sort-by :progress (comparator order-progress) results))
	mesg (if-let [d (:describe res)]
	       (if (vector? d)
		 (apply format "%s: %s (%s:%d)" d file line)
		 (format "Expected %s (%s:%d)" d file line)))]
    (throw (Exception. (format "%s: %s" name mesg)))))

(defn- perform-match
  [form rt]
  (let [[rule template] rt]
    (assoc (pm/match rule form)
      :template template)))

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
