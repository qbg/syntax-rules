(ns qbg.syntax-rules.template
  (:require
   [qbg.syntax-rules.template.parse :as parse]
   [qbg.syntax-rules.template.fill :as fill]
   [qbg.syntax-rules.core :as core]))

(defn parse
  "Return [parsed-template fns]"
  [template literals]
  (parse/parse-template template literals *ns*))

(defn fill-template
  "Fill in a parsed template with the supplied fns using match or the implicit
match state"
  ([parsed fns]
     (fill-template parsed fns core/*current-match*))
  ([parsed fns match]
     (let [match (assoc match :params fns)]
       (fill/fill-template parsed match))))

(defn contains-var?
  "Return true if sym is bound in match or the implicit current match"
  ([sym]
     (contains-var? sym core/*current-match*))
  ([sym match]
     (fill/contains-var? match sym)))
