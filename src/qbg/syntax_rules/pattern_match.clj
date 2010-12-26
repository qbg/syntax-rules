(ns qbg.syntax-rules.pattern-match)

(declare compile-pattern)

(defn- compile-variable
  [form state]
  `((:store ~(second form))))

(defn- compile-literal
  [form state]
  `((:literal ~(second form))))

(defn- compile-amp
  [form state]
  (let [vars (second form)
	state (update-in state [:depth] inc)
	patterns (nthnext form 2)
	pinstr (mapcat #(compile-pattern % state) patterns)]
    `((:push-vars ~vars) (:rep ~@pinstr (:collect-vars ~vars))
      (:pop-vars ~(:depth state) ~vars))))

(defn- compile-list
  [form state]
  `((:list) ~@(mapcat #(compile-pattern % state) (rest form)) (:eos)))

(defn- compile-vector
  [form state]
  `((:vector) ~@(mapcat #(compile-pattern % state) (rest form)) (:eos)))

(defn- compile-describe
  [form state]
  `((:push-describe ~(second form)) ~@(compile-pattern (nth form 2)) (:pop-describe)))

(defn- compile-pattern
  ([form state]
     ((condp = (first form)
	  :variable compile-variable
	  :literal compile-literal
	  :amp compile-amp
	  :list compile-list
	  :vector compile-vector
	  :describe compile-describe)
      form state))
  ([form]
     (compile-pattern form {:depth 0})))

(defn- make-state
  [input]
  {:vars {} :input [input] :istack [] :good true
   :dstack [] :pstack [[]] :form input :fstack []})

(declare exe-commands)

(defn- move-sym
  [pstack sym]
  (conj (pop pstack) (conj (peek pstack) sym)))

(defn- move-forward
  [pstack]
  (move-sym pstack :f))

(defn- move-in
  [pstack]
  (move-sym pstack :i))

(defn- move-late
  [pstack]
  (move-sym pstack :l))

(defn- exe-store
  [cmd state]
  (let [v (second cmd)
	store (fnil #(conj (pop %1) %2) [nil])]
    (if-let [[item & input] (:input state)]
      (assoc state
	:input input
	:vars (update-in (:vars state) [v :val] store item)
	:pstack (move-forward (:pstack state)))
      (assoc state :good false))))

(defn- exe-literal
  [cmd state]
  (let [lit (second cmd)]
    (if-let [[item & input] (:input state)]
      (if (= item lit)
	(assoc state
	  :input input
	  :pstack (move-forward (:pstack state)))
	(assoc state :good false
	       :pstack (move-in (:pstack state))))
      (assoc state :good false))))

(defn- exe-push-vars
  [cmd state]
  (let [vs (second cmd)
	push-var (fn [vars v]
		   (update-in vars [v :val] #(conj (pop (or % [nil])) [] nil)))]
    (assoc state :vars (reduce push-var (:vars state) vs))))

(defn- exe-collect-vars
  [cmd state]
  (let [vs (second cmd)
	merge-val (fn [v]
		    (let [slast (nth v (- (count v) 2))
			  last (nth v (dec (count v)))]
		      (conj (pop (pop v)) (conj slast last) nil)))
	collect-var (fn [vars v]
		      (update-in vars [v :val] merge-val))]
    (assoc state :vars (reduce collect-var (:vars state) vs))))

(defn- exe-pop-vars
  [cmd state]
  (let [depth (second cmd)
	vs (nth cmd 2)
	pop-var (fn [vars v]
		  (update-in vars [v] assoc
			     :amp-depth (max depth (or (:amp-depth (vars v)) 0))
			     :val (pop (:val (vars v)))))]
    (assoc state :vars (reduce pop-var (:vars state) vs))))

(defn- exe-rep
  [cmd state]
  (if (:good state)
    (if (empty? (:input state))
      state
      (recur cmd (exe-commands (rest cmd) state)))
    state))

(defn- exe-eos
  [cmd state]
  (if (empty? (:input state))
    (assoc state
      :input (peek (:istack state))
      :istack (pop (:istack state))
      :pstack (move-forward (pop (:pstack state))))
    (assoc state :good false)))

(defn- exe-nest
  [state test]
  (if-let [[item & inputs] (:input state)]
    (if (test item)
      (assoc state
	:input item
	:istack (conj (:istack state) inputs)
	:pstack (conj (:pstack state) (conj (peek (:pstack state)) :i)))
      (assoc state :good false))
    (assoc state :good false)))

(defn- exe-list
  [cmd state]
  (exe-nest state seq?))

(defn- exe-vector
  [cmd state]
  (exe-nest state vector?))

(defn- exe-push-describe
  [cmd state]
  (assoc state
    :dstack (conj (:dstack state) (second cmd))
    :fstack (conj (:fstack state) (:form state))
    :form (if (seq (:input state)) (first (:input state)) (:form state))))

(defn- exe-pop-describe
  [cmd state]
  (assoc state
    :dstack (pop (:dstack state))
    :fstack (pop (:fstack state))
    :form (peek (:fstack state))))

(defn- exe-command
  [cmd state]
  ((condp = (first cmd)
       :store exe-store
       :literal exe-literal
       :push-vars exe-push-vars
       :rep exe-rep
       :collect-vars exe-collect-vars
       :pop-vars exe-pop-vars
       :eos exe-eos
       :list exe-list
       :vector exe-vector
       :push-describe exe-push-describe
       :pop-describe exe-pop-describe)
   cmd state))

(defn- exe-commands
  [cmds state]
  (if (:good state)
    (if (seq cmds)
      (recur (next cmds) (exe-command (first cmds) state))
      state)
    state))

(defn- fixup-state
  [state]
  (let [vars (:vars state)
	fix (fn [[k v]] [k (assoc v
			     :amp-depth (or (:amp-depth v) 0)
			     :val (peek (:val v)))])]
    {:vars (if (:good state) (into {} (map fix vars)) {})
     :good (:good state)
     :describe (peek (:dstack state))
     :progress (peek (:pstack state))
     :form (:form state)}))

(defn match
  [pattern form]
  (->> (make-state form)
       (exe-commands (compile-pattern pattern))
       fixup-state))
