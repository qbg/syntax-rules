(ns qbg.syntax-rules.pattern.primitives
  (:require [qbg.syntax-rules.core :as core]))

(defn fixup-state
  [state]
  {:vars (if (:good state) (:vars state) {})
   :good (:good state)
   :describe (peek (:dstack state))
   :progress (:progress state)})

(defn fail-state
  [state]
  (let [cause (peek (:dstack state))
	ddepth (peek (:ddstack state))
	depth (:depth state)]
    (if (and (vector? cause) (= 1 (count cause)))
      (let [delta (- depth ddepth)
	    form (if (= delta 0)
		   (first (:input state))
		   (first (nth (:istack state) (dec delta))))
	    mesg (peek (:dstack state))
	    dstack (conj (pop (:dstack state)) (conj mesg form))]
	(assoc state :good false :dstack dstack))
      (assoc state :good false))))

(defn eval-in-context
  [thunk]
  (fn [state]
    (binding [core/*current-match* (fixup-state state)]
      ((thunk) state))))

(defn in-progress
  []
  (fn [state]
    (update-in state [:progress] conj 0)))

(defn out-progress
  []
  (fn [state]
    (let [p (pop (:progress state))
	  v (peek p)
	  p (pop p)
	  p (conj p (inc v))]
      (assoc state :progress p))))

(defn store-variable
  [sym]
  (fn [state]
    (let [item (first (:input state))
	  vm (get (:vars state) sym)]
      (if vm
	(fail-state state)
	(assoc-in state [:vars sym :val] item)))))

(defn assert-pred
  [pred mesg]
  (fn [state]
    (let [item (first (:input state))]
      (if (pred item)
	state
	(let [dstack (:dstack state)
	      dstack (conj dstack [(format "expected %s" mesg) item])]
	  (fail-state (assoc state :dstack dstack)))))))

(defn assert-literal
  [lit]
  (assert-pred #(= % lit) (str lit)))

(defn forward
  []
  (fn [state]
    (if-let [input (seq (:input state))]
      (assoc state :input (next input))
      (fail-state state))))

(defn assert-end
  []
  (fn [state]
    (if (empty? (:input state))
      state
      (fail-state state))))

(defn nest
  []
  (fn [state]
    (let [item (first (:input state))]
      (assoc state
	:input item
	:istack (conj (:istack state) (:input state))
	:depth (inc (:depth state)))
      (fail-state state))))

(defn unnest
  []
  (fn [state]
    (assoc state
      :input (peek (:istack state))
      :istack (pop (:istack state))
      :depth (dec (:depth state)))))

(defn push-describe
  [mesg]
  (fn [state]
    (let [mesg (format "expected %s" mesg)]
      (assoc state
	:ddstack (conj (:ddstack state) (:depth state))
	:dstack (conj (:dstack state) [mesg])))))

(defn phase-out-describe
  []
  (fn [state]
    (assoc state :pop-describe true)))

(defn pop-describe
  []
  (fn [state]
    (if (:pop-describe state)
      (assoc state
	:dstack (pop (:dstack state))
	:ddstack (pop (:ddstack state))
	:pop-describe false)
      state)))

(defn push-vars
  []
  (fn [state]
    (assoc state
      :vars {}
      :vstack (conj (:vstack state) (:vars state)))))

(defn merge-vars
  []
  (fn [state]
    (let [vstack (pop (:vstack state))
	  top-vs (peek (:vstack state))
	  merge-f (fn [prev next]
		    (if (:val prev)
		      (throw (IllegalStateException. "Pattern variable already bound"))
		      (let [prev (assoc prev :ell (or (:ell prev) []))]
			(update-in prev [:ell] conj next))))
	  top-vs (merge-with merge-f top-vs (:vars state))]
      (assoc state
	:vstack (conj vstack top-vs)
	:vars {}))))

(defn pop-vars
  []
  (fn [state]
    (assoc state
      :vars (peek (:vstack state))
      :vstack (pop (:vstack state)))))

(defn pop-nesting-vars
  [sym]
  (fn [state]
    (let [top-vs (peek (:vstack state))]
      (assoc state
	:vars (assoc-in top-vs [sym :vars] (:vars state))
	:vstack (pop (:vstack state))))))

(defn combine
  [& cmds]
  (fn [state]
    (loop [state state, cmds cmds]
      (if (:good state)
	(if (seq cmds)
	  (recur ((first cmds) state) (next cmds))
	  state)
	state))))

(defn perform-and
  [& cmds]
  (fn [state]
    (let [intro (butlast cmds)
	  final (last cmds)
	  input (:input state)
	  progress (:progress state)]
      (loop [state state, intro intro]
	(if (seq intro)
	  (if (:good state)
	    (recur (assoc ((first intro) state)
		     :input input
		     :progress progress)
		   (next intro))
	    state)
	  (if (:good state)
	    (final state)
	    state))))))

(defn- fail-or
  [states]
  (let [cmp (comparator core/order-progress)
	best (first (sort-by :progress cmp states))]
    (fail-state best)))

(defn perform-or
  [& cmds]
  (fn [state]
    (loop [cmds cmds, res []]
      (if (seq cmds)
	(let [st ((first cmds) state)]
	  (if (:good st)
	    st
	    (recur (next cmds) (conj res st))))
	(fail-or res)))))

(defn perform-rep
  [& cmds]
  (let [cmd (apply combine cmds)]
    (fn [state]
      (let [new-state (cmd state)]
	(if (and (:good new-state)
		 (not= (:progress state) (:progress new-state)))
	  (recur new-state)
	  state)))))

(defn- dirty-fill
  [form-d state]
  (let [state (fixup-state state)]
    (binding [core/*current-match* state]
      (form-d))))

(defn aside
  [form-d & cmds]
  (let [cmd (apply combine cmds)]
    (fn [state]
      (let [input (:input state)
	    progress (:progress state)
	    ft (dirty-fill form-d state)
	    st (assoc state :input [ft])
	    st (cmd st)]
	(assoc st :progress progress :input input)))))

(defn- fail-guard
  [state mesg res]
  (let [state (assoc state
		:dstack (conj (:dstack state) [mesg res])
		:progress (conj (:progress state) :late))]
    (fail-state state)))

(defn guard
  [form-d mesg-d]
  (fn [state]
    (binding [core/*current-match* (fixup-state state)]
      (let [res (form-d)]
	(if res
	  (fail-guard state (mesg-d) res)
	  state)))))

(defn make-state
  [input]
  {:vars {} :input [input] :istack ()
   :good true
   :dstack ["Bad syntax"] :ddstack [] :progress [0]
   :depth 0 :vstack ()
   :pop-describe false})
