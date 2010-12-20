(ns qbg.syntax-rules
  (:require
    [qbg.syntax-rules.pattern-parse :as pp]
    [qbg.syntax-rules.pattern-match :as pm]
    [qbg.syntax-rules.template-fill :as tf]))

(defn make-apply-rules 
  [name rules templates]
  (let [rule-templates (map pp/build-rule-template rules templates)]
    (fn [form]
      (loop [rt rule-templates]
        (if-let [[[rule template] & rt] rt]
          (if-let [m (pm/match rule form)]
            (tf/fill-template template m)
            (recur rt))
          (throw (Exception. (format "%s: Bad syntax in %s" name form))))))))

(defmacro defsyntax-rules
  "Define a macro that uses the rule-template pairs to expand all invokations"
  [name & rt-pairs]
  (let [rules (take-nth 2 rt-pairs)
        templates (take-nth 2 (rest rt-pairs))]
    `(let [ar# (make-apply-rules '~name '~rules '~templates)]
       (defmacro ~name
         [& ~'forms]
         (ar# ~'&form)))))
