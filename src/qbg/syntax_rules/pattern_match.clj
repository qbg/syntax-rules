(ns qbg.syntax-rules.pattern-match)

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
	  :istack (conj (:istack state) (next (:input state))))
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
   [(do-eos) (do-out-progress)]))

(defn- compile-vector
  [form]
  (concat
   [(do-in-progress) (do-nest vector?)]
   (mapcat compile-pattern (rest form))
   [(do-eos) (do-out-progress)]))

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

(defn- compile-pattern
  [form]
  ((condp = (first form)
       :variable compile-variable
       :literal compile-literal
       :amp compile-amp
       :list compile-list
       :vector compile-vector
       :describe compile-describe)
   form))

(defn- make-state
  [input]
  {:vars {} :input [input] :istack [] :good true
   :dstack ["Bad syntax"] :progress [0]})

(defn- exe-commands
  [cmds state]
  (if (:good state)
    (if (seq cmds)
      (recur (next cmds) ((first cmds) state))
      state)
    state))

(defn- fixup-state
  [state]
  (let [vars (:vars state)
	fix (fn [[k v]] [k (update-in v [:val] peek)])]
    {:vars (if (:good state) (into {} (map fix vars)) {})
     :good (:good state)
     :describe (peek (:dstack state))
     :progress (:progress state)}))

(defn match
  [pattern form]
  (->> (make-state form)
       (exe-commands (compile-pattern pattern))
       fixup-state))
