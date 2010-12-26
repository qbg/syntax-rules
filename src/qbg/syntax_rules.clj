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
   (= (first p2) :l) true
   (= (first p1) :l) false
   (= (first p2) :i) true
   (= (first p1) :i) false
   :else (recur (next p1) (next p2))))

(defn make-apply-rules 
  [name rules templates]
  (let [rule-templates (map pp/build-rule-template rules templates)]
    (fn [form]
      (loop [rt rule-templates, results []]
        (if-let [[[rule template] & rt] rt]
          (let [m (pm/match rule form)]
	    (if (:good m)
	      (tf/fill-template template (:vars m))
	      (recur rt (conj results m))))
	  (let [res (sort-by #(order-progress (:progress %1) (:progress %2)) results)
		res (first res)
		form (:form res)
		mesg (if-let [d (:describe res)]
		       (if (vector? d)
			 (format "%s: %s" (first d) (second d))
			 (format "Expected %s in %s" d (:form res)))
		       (format "Bad syntax in %s" form))]
	    (throw (Exception. (format "%s: %s" name mesg)))))))))

(defmacro defsyntax-rules
  "Define a macro that uses the rule-template pairs to expand all invokations"
  [name & rt-pairs]
  (let [rules (take-nth 2 rt-pairs)
        templates (take-nth 2 (rest rt-pairs))]
    `(let [ar# (make-apply-rules '~name '~rules '~templates)]
       (defmacro ~name
         [& ~'forms]
         (ar# ~'&form)))))
