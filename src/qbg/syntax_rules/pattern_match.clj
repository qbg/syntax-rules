(ns qbg.syntax-rules.pattern-match
  (:require [qbg.syntax-rules.template-fill :as tf]))

(declare exe-commands compile-pattern)

(defn- current-item
  [state]
  (first (:input state)))

(defn- lookup-var
  [vars v]
  (if (contains? vars v)
    (get vars v)
    {:amp-depth 0 :val []}))

(defn- fail-state
  [state]
  (assoc state :good false))

(defn- do-store
  [variable]
  (fn [state]
    (let [item (current-item state)
	  v (lookup-var (:vars state) variable)
	  v (update-in v [:val] conj item)]
      (assoc-in state [:vars variable] v))))

(defn- do-literal
  [lit]
  (fn [state]
    (let [item (current-item state)]
      (if (= item lit)
	state
	(fail-state state)))))

(defn- forward-progress
  [progress]
  (let [n (peek progress)]
    (conj (pop progress) (inc n))))

(defn- do-in-progress
  []
  (fn [state]
    (assoc state :progress (conj (:progress state) 0))))

(defn- do-out-progress
  []
  (fn [state]
    (let [p (pop (:progress state))
	  p (forward-progress p)]
      (assoc state :progress p))))

(defn- move-forward
  []
  (fn [state]
    (if-let [input (seq (:input state))]
      (assoc state
	:input (next input))
      (fail-state state))))

(defn- do-eos
  []
  (fn [state]
    (if (empty? (:input state))
      (assoc state
	:input (peek (:istack state))
	:istack (pop (:istack state)))
      (fail-state state))))

(defn- do-nest
  [pred]
  (fn [state]
    (let [item (current-item state)]
      (if (pred item)
	(assoc state
	  :input item
	  :istack (conj (:istack state) (:input state)))
	(fail-state state)))))

(defn- do-push-describe
  [mesg]
  (fn [state]
    (assoc state
      :dstack (conj (:dstack state) mesg))))

(defn- do-pop-describe
  []
  (fn [state]
    (assoc state
      :dstack (pop (:dstack state)))))

(defn- do-push-vars
  [vs]
  (fn [state]
    (let [vars (:vars state)
	  push-var (fn [vars v]
		     (let [vm (lookup-var vars v)]
		       (assoc vars v
			      {:amp-depth (max (:amp-depth vm)
					       (inc (count (:val vm))))
			       :val (conj (:val vm) [])})))]
      (assoc state :vars (reduce push-var vars vs)))))

(defn- do-collect-vars
  [vs]
  (fn [state]
    (let [vars (:vars state)
	  pack (fn [v]
		 (let [item (peek v)
		       v (pop v)
		       coll (peek v)
		       v (pop v)]
		   (conj v (conj coll item))))
	  collect-var (fn [vars v] (update-in vars [v :val] pack))]
      (assoc state :vars (reduce collect-var vars vs)))))

(defn- do-rep
  [cmds]
  (fn [state]
    (if (:good state)
      (if (seq (:input state))
	(recur (exe-commands cmds state))
	state)
      state)))

(defn- do-push-state
  []
  (fn [state]
    (let [entry {:vars (:vars state)
		 :varm (:varm state)}]
      (assoc state
	:vstack (conj (:vstack state) entry)
	:vars {}
	:varm {}))))

(defn- do-pop-state
  [variable]
  (fn [state]
    (let [entry {:vars (:vars state)
		 :varm (:varm state)}
	  top (peek (:vstack state))
	  vars (:vars top)
	  varm (assoc (:varm top) variable entry)]
      (assoc state
	:vstack (pop (:vstack state))
	:vars vars
	:varm varm))))

(defn- do-and
  [cmds]
  (fn [state]
    (let [intro (butlast cmds)
	  final (last cmds)
	  input (:input state)
	  progress (:progress state)]
      (loop [state state, intro intro]
	(if (seq intro)
	  (if (:good state)
	    (recur (assoc (exe-commands (first intro) state)
		     :input input
		     :progress progress)
		   (next intro))
	    state)
	  (if (:good state)
	    (exe-commands final state)
	    state))))))

(defn- do-or
  [cmds]
  (fn [state]
    (loop [cmds cmds]
      (if (seq cmds)
	(let [st (exe-commands (first cmds) state)]
	  (if (:good st)
	    st
	    (recur (next cmds))))
	(fail-state state)))))

(defn- do-varclass
  [klass & args]
  (fn [state]
    ((apply klass args) state)))

(defn- do-pattern
  [cmds template]
  (fn [state]
    (let [input (:input state)
	  progress (:progress state)
	  ft (tf/fill-template template state)
	  st (assoc state :input ft)
	  st (exe-commands cmds st)]
      (assoc st :progress progress :input input))))

(defn- compile-variable
  [form]
  [(do-in-progress) (do-store (second form)) (move-forward) (do-out-progress)])

(defn- compile-literal
  [form]
  [(do-in-progress) (do-literal (second form)) (move-forward) (do-out-progress)])

(defn- compile-list
  [form]
  (concat
   [(do-in-progress) (do-nest seq?)]
   (mapcat compile-pattern (rest form))
   [(do-eos) (move-forward) (do-out-progress)]))

(defn- compile-vector
  [form]
  (concat
   [(do-in-progress) (do-nest vector?)]
   (mapcat compile-pattern (rest form))
   [(do-eos) (move-forward) (do-out-progress)]))

(defn- compile-describe
  [form]
  (concat
   [(do-push-describe (second form))]
   (compile-pattern (nth form 2))
   [(do-pop-describe)]))

(defn- compile-amp
  [form]
  (let [vars (second form)
	patterns (nthnext form 2)
	pinstr (mapcat compile-pattern patterns)]
    [(do-push-vars vars) (do-rep (concat pinstr [(do-collect-vars vars)]))]))

(defn- compile-head
  [form]
  (mapcat compile-pattern (rest form)))

(defn- compile-varclass
  [form]
  (let [[_ variable klass & args] form]
    [(do-in-progress) (do-push-state) (do-store variable)
     (apply do-varclass klass args) (move-forward)
     (do-pop-state) (do-out-progress)]))

(defn- compile-and
  [form]
  [(do-and (map compile-pattern (rest form)))])

(defn- compile-or
  [form]
  [(do-or (map compile-pattern (rest form)))])

(defn- compile-pattern-form
  [form]
  [(do-pattern (compile-pattern (second form)) (nth form 2))])

(defn- compile-pattern
  [form]
  ((condp = (first form)
       :variable compile-variable
       :literal compile-literal
       :amp compile-amp
       :head compile-head
       :pattern compile-pattern-form
       :list compile-list
       :vector compile-vector
       :describe compile-describe
       :and compile-and
       :or compile-or)
   form))

(defn- make-state
  [input]
  {:vars {} :input [input] :istack [] :good true
   :dstack ["Bad syntax"] :progress [0]
   :vstack [] :varm {}})

(defn- exe-commands
  [cmds state]
  (if (:good state)
    (if (seq cmds)
      (recur (next cmds) ((first cmds) state))
      state)
    state))

(defn- fix-vars
  [state]
  (let [fix (fn [[k v]] [k (update-in v [:val] peek)])
	fixm (fn [[k v]] [k (fix-vars v)])]
    (assoc state
      :vars (into {} (map fix (:vars state)))
      :varm (into {} (map fixm (:varm state))))))

(defn- fixup-state
  [state]
  (let [state (fix-vars state)]
    {:vars (if (:good state) (:vars state) {})
     :varm (if (:good state) (:varm state) {})
     :good (:good state)
     :describe (peek (:dstack state))
     :progress (:progress state)}))

(defn match
  [pattern form]
  (->> (make-state form)
       (exe-commands (compile-pattern pattern))
       fixup-state))
