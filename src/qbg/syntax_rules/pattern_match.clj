(ns qbg.syntax-rules.pattern-match
  (:require [qbg.syntax-rules.template-fill :as tf]))

(declare exe-commands compile-pattern fixup-state)

(def var-marker (Object.))

(defn order-progress
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

(defn- current-item
  [state]
  (first (:input state)))

(defn- lookup-var
  [vars v]
  (if (contains? vars v)
    (get vars v)
    {:amp-depth 0 :val [var-marker]}))

(defn- fail-state
  [state]
  (assoc state :good false))

(defn- do-store
  [variable]
  (fn [state]
    (let [item (current-item state)
	  v (lookup-var (:vars state) variable)
	  good? (= (peek (:val v)) var-marker)
	  v (assoc v :val (conj (pop (:val v)) item))]
      (if good?
	(assoc-in state [:vars variable] v)
	(fail-state state)))))

(defn- do-literal
  [lit]
  (fn [state]
    (let [item (current-item state)]
      (if (= item lit)
	state
	(let [dstack (:dstack state)
	      dstack (conj dstack [(format "expected %s" (str lit)) item])]
	  (fail-state (assoc state :dstack dstack)))))))

(defn- forward-progress
  [progress]
  (let [n (peek progress)]
    (conj (pop progress) (inc n))))

(defn- late-progress
  [progress]
  (conj progress :late))

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

(defn- do-assert-end
  []
  (fn [state]
    (if (empty? (:input state))
      state
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
    (let [mesg (format "expected %s" mesg)
	  form (current-item state)]
      (assoc state
	:dstack (conj (:dstack state) [mesg form])))))

(defn- do-copy-describe
  []
  (fn [state]
    (let [mesg (peek (:dstack state))
	  dstack (conj (:dstack state) mesg)]
      (assoc state :dstack dstack))))

(defn- do-set-describe
  [mesg]
  (fn [state]
    (let [mesg (format "expected %s" mesg)
	  input (:input state)
	  dstack (:dstack state)
	  dstack (conj (pop dstack) [mesg input])]
      (assoc state :dstack dstack))))

(defn- do-pop-describe
  []
  (fn [state]
    (assoc state
      :dstack (pop (:dstack state)))))

(defn- do-push-vars
  [vs]
  (fn [state]
    (let [vars (:vars state)
	  good? (every? #(= % var-marker)
			(map (comp peek :val)
			     (map #(lookup-var vars %) vs)))
	  push-var (fn [vars v]
		     (let [vm (lookup-var vars v)
			   depth (inc (or (:depth vm) 0))]
		       (assoc vars v
			      {:depth depth
			       :amp-depth (max (:amp-depth vm) depth)
			       :val (conj (pop (:val vm)) [] var-marker)})))]
      (if good?
	(assoc state :vars (reduce push-var vars vs))
	(fail-state state)))))

(defn- pack-var
  [v]
  (if (= (peek v) var-marker)
    v
    (let [item (peek v)
	  v (pop v)
	  coll (peek v)
	  v (pop v)]
      (conj v (conj coll item) var-marker))))

(defn- do-collect-vars
  [vs]
  (fn [state]
    (let [vars (:vars state)
	  collect-var (fn [vars v]
			(let [vm (vars v)
			      vm (assoc vm
				   :val (pack-var (:val vm)))]
			  (assoc vars v vm)))]
      (assoc state :vars (reduce collect-var vars vs)))))

(defn- do-clean-vars
  [vs]
  (fn [state]
    (let [vars (:vars state)
	  clean-coll (fn [v] (if (= (peek v) var-marker) (pop v) v))
	  clean-var (fn [vars v]
		      (update-in vars [v]
				assoc
				:val (clean-coll (:val (vars v)))
				:depth (dec (:depth (vars v)))))]
      (assoc state :vars (reduce clean-var vars vs)))))

(defn- do-rep
  [cmds]
  (fn [state]
    (if (seq (:input state))
      (let [new-state (exe-commands cmds state)]
	(if (and (:good new-state))
	  (recur new-state)
	  state))
      state)))

(defn- do-push-state
  []
  (fn [state]
    (let [entry {:vars (:vars state)
		 :varm (:varm state)}]
      (assoc state
	:vstack (conj (:vstack state) entry)
	:vars {}
	:varm #{}))))

(defn- do-pop-state
  [variable]
  (fn [state]
    (let [entry {:vars (:vars state)
		 :varm (:varm state)}
	  top (peek (:vstack state))
	  vars (:vars top)
	  val (:val (get vars variable))
	  entry (conj entry {:mega (peek val)})
	  val (conj (pop val) entry)
	  vars (assoc-in vars [variable :val] val)
	  varm (conj (:varm top) variable)]
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

(defn- fail-or
  [states]
  (let [cmp (comparator order-progress)
	best (first (sort-by :progress cmp states))]
    (fail-state best)))

(defn- do-or
  [cmds]
  (fn [state]
    (loop [cmds cmds, res []]
      (if (seq cmds)
	(let [st (exe-commands (first cmds) state)]
	  (if (:good st)
	    st
	    (recur (next cmds) (conj res st))))
	(fail-or res)))))

(defn- do-varclass
  [klass & args]
  (fn [state]
    ((apply klass args) state)))

(defn- dirty-fill
  [template state]
  (let [state (fixup-state state)]
    (binding [tf/*current-match* state]
      (tf/fill-template template state)))) 

(defn- do-pattern
  [cmds template]
  (fn [state]
    (let [input (:input state)
	  progress (:progress state)
	  ft (dirty-fill template state)
	  st (assoc state :input [ft])
	  st (exe-commands cmds st)]
      (assoc st :progress progress :input input))))

(defn- fail-guard
  [state mesg res]
  (let [state (assoc state
		:dstack (conj (:dstack state) [mesg res])
		:progress (late-progress (:progress state)))]
    (fail-state state)))

(defn- do-guard
  [ns pred mesg]
  (fn [state]
    (binding [tf/*current-match* (fixup-state state)
	      *ns* (find-ns ns)]
      (let [params (:params state)
	    vars (keys params)
	    rhss (map (fn [v] `(quote ~v)) (vals params))
	    bv (vec (interleave vars rhss))
	    res (eval `(let ~bv ~pred))]
	(if res
	  (fail-guard state (eval `(let ~bv ~mesg)) res)
	  state)))))

(defn- do-push-params
  [params]
  (fn [state]
    (assoc state
      :params params
      :param-stack (conj (:param-stack state) (:params state)))))

(defn- do-pop-params
  []
  (fn [state]
    (assoc state
      :params (peek (:param-stack state))
      :param-stack (pop (:param-stack state)))))

(defn- compile-variable
  [form]
  [(do-in-progress) (do-store (second form)) (move-forward) (do-out-progress)])

(defn- compile-literal
  [form]
  [(do-in-progress) (do-literal (second form)) (move-forward) (do-out-progress)])

(defn- compile-list
  [form]
  (concat
   [(do-in-progress) (do-copy-describe) (do-nest seq?)]
   (mapcat compile-pattern (rest form))
   [(do-eos) (do-pop-describe) (move-forward) (do-out-progress)]))

(defn- compile-vector
  [form]
  (concat
   [(do-in-progress) (do-copy-describe) (do-nest vector?)]
   (mapcat compile-pattern (rest form))
   [(do-eos) (do-pop-describe) (move-forward) (do-out-progress)]))

(defn- compile-describe
  [form]
  (concat
   [(do-push-describe (second form))]
   (compile-pattern (nth form 2))
   [(do-pop-describe)]))

(defn- compile-pdescribe
  [form]
  [(do-set-describe (second form))])

(defn- compile-amp
  [form]
  (let [vars (second form)
	patterns (nthnext form 2)
	pinstr (mapcat compile-pattern patterns)]
    [(do-push-vars vars) (do-rep (concat pinstr [(do-collect-vars vars)])) (do-clean-vars vars)]))

(defn- compile-options
  [form]
  (let [[_ vars & patterns] form
	pinstrs (map compile-pattern patterns)]
    [(do-rep [(do-or pinstrs)])]))

(defn- compile-head
  [form]
  (mapcat compile-pattern (rest form)))

(defn- compile-only
  [form]
  (concat (mapcat compile-pattern (rest form)) [(do-assert-end)]))

(defn- compile-varclass
  [form]
  (let [[_ variable klass & args] form]
    [(do-store variable) (do-push-state)
     (apply do-varclass klass args)
     (do-pop-state variable)]))

(defn- compile-and
  [form]
  [(do-and (map compile-pattern (rest form)))])

(defn- compile-or
  [form]
  [(do-or (map compile-pattern (rest form)))])

(defn- compile-pattern-form
  [form]
  [(do-pattern (compile-pattern (second form)) (nth form 2))])

(defn- compile-guard
  [form]
  (let [[_ ns code mesg] form]
    [(do-guard ns code mesg)]))

(defn- compile-pattern
  [form]
  ((condp = (first form)
       :variable compile-variable
       :literal compile-literal
       :amp compile-amp
       :options compile-options
       :head compile-head
       :only compile-only
       :pattern compile-pattern-form
       :list compile-list
       :vector compile-vector
       :describe compile-describe
       :pdescribe compile-pdescribe
       :and compile-and
       :or compile-or
       :guard compile-guard
       :varclass compile-varclass)
   form))

(defn- make-state
  [input]
  {:vars {} :input [input] :istack [] :good true
   :dstack ["Bad syntax"] :progress [0]
   :vstack [] :varm #{}
   :params {} :param-stack []})

(defn- exe-commands
  [cmds state]
  (if (:good state)
    (if (seq cmds)
      (recur (next cmds) ((first cmds) state))
      state)
    state))

(defn- last-non-marker
  [v]
  (if (= (peek v) var-marker)
    (recur (pop v))
    (peek v)))

(defn- map-depth
  [f d coll]
  (if (= d 0)
    (f coll)
    (vec (map #(map-depth f (dec d) %) coll))))

(defn- fix-vars
  [state]
  (let [fix (fn [[k v]]
	      (let [v (update-in v [:val] last-non-marker)
		    d (:amp-depth v)
		    v (if ((:varm state) k)
			(update-in v [:val] #(map-depth fix-vars d %))
			v)]
		[k v]))]
    (assoc state
      :vars (into {} (map fix (:vars state))))))

(defn- fixup-state
  [state]
  (let [state (fix-vars state)]
    {:vars (if (:good state) (:vars state) {})
     :varm (if (:good state) (:varm state) #{})
     :good (:good state)
     :describe (peek (:dstack state))
     :progress (:progress state)}))

(defn match
  [pattern form]
  (->> (make-state form)
       (exe-commands (compile-pattern pattern))
       fixup-state))

(defn make-syntax-class
  [vars vals pattern]
  (let [cmds (concat [(do-push-params (zipmap vars vals))]
		     (compile-pattern pattern)
		     [(do-pop-params)])]
    (fn [state]
      (exe-commands cmds state))))
