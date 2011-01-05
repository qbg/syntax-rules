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
  "Evaluate thunk in the context of the current match"
  [thunk]
  (fn [state]
    (binding [core/*current-match* (fixup-state state)]
      ((thunk) state))))

(defn in-progress
  "Adjust the progress as to move into the current item"
  []
  (fn [state]
    (update-in state [:progress] conj 0)))

(defn out-progress
  "Adjust the progress as to move out of the current item and towards the next
one"
  []
  (fn [state]
    (let [p (pop (:progress state))
	  v (peek p)
	  p (pop p)
	  p (conj p (inc v))]
      (assoc state :progress p))))

(defn store-variable
  "Store the current item into sym, failing if it has already been bound"
  [sym]
  (fn [state]
    (let [item (first (:input state))
	  vm (get (:vars state) sym)]
      (if vm
	(fail-state state)
	(assoc-in state [:vars sym :val] item)))))

(defn assert-pred
  "Fail if pred on the current item returns false, where mesg describes the
predicate"
  [pred mesg]
  (fn [state]
    (let [item (first (:input state))]
      (if (pred item)
	state
	(let [dstack (:dstack state)
	      dstack (conj dstack [(format "expected %s" mesg) item])]
	  (fail-state (assoc state :dstack dstack)))))))

(defn assert-literal
  "Fail if the current item is not lit"
  [lit]
  (assert-pred #(= % lit) (str lit)))

(defn forward
  "Move the input forward one item, failing if there is no more input in the
current sequence"
  []
  (fn [state]
    (if-let [input (seq (:input state))]
      (assoc state :input (next input))
      (fail-state state))))

(defn assert-end
  "Fail if there is more input in the current sequence"
  []
  (fn [state]
    (if (empty? (:input state))
      state
      (fail-state state))))

(defn nest
  "Enter the seqable current item"
  []
  (fn [state]
    (let [item (first (:input state))]
      (assoc state
	:input item
	:istack (conj (:istack state) (:input state))
	:depth (inc (:depth state))))))

(defn unnest
  "Exit the current sequence"
  []
  (fn [state]
    (assoc state
      :input (peek (:istack state))
      :istack (pop (:istack state))
      :depth (dec (:depth state)))))

(defn push-describe
  "Push mesg on the description stack"
  [mesg]
  (fn [state]
    (let [mesg (format "expected %s" mesg)]
      (assoc state
	:ddstack (conj (:ddstack state) (:depth state))
	:dstack (conj (:dstack state) [mesg])))))

(defn phase-out-describe
  "Mark the current describe message as being ready to pop"
  []
  (fn [state]
    (assoc state :pop-describe true)))

(defn pop-describe
  "Pop the current describe message if it is ready to be popped"
  []
  (fn [state]
    (if (:pop-describe state)
      (assoc state
	:dstack (pop (:dstack state))
	:ddstack (pop (:ddstack state))
	:pop-describe false)
      state)))

(defn push-vars
  "Push :vars onto :vstack"
  []
  (fn [state]
    (assoc state
      :vars {}
      :vstack (conj (:vstack state) (:vars state)))))

(defn merge-vars
  "Merge and empty :vars into the top of :vstack"
  []
  (fn [state]
    (let [vstack (pop (:vstack state))
	  top-vs (peek (:vstack state))
	  merge-f (fn [prev next]
		    (if (:val prev)
		      (throw (IllegalStateException. "Pattern variable already bound"))
		      (update-in prev [:ell] conj next)))
	  new-keys (remove (set (keys top-vs)) (keys (:vars state)))
	  old-vars (apply dissoc (:vars state) new-keys)
	  new-vs (select-keys (:vars state) new-keys)
	  new-vs (into {} (map (fn [[k v]] [k {:ell [v]}]) new-vs))
	  top-vs (merge-with merge-f top-vs old-vars)
	  top-vs (conj top-vs new-vs)]
      (assoc state
	:vstack (conj vstack top-vs)
	:vars {}))))

(defn pop-vars
  "Pop :vstack to :vars"
  []
  (fn [state]
    (assoc state
      :vars (peek (:vstack state))
      :vstack (pop (:vstack state)))))

(defn pop-nesting-vars
  "Pop :vstack to :vars, nesting the current :vars under sym"
  [sym]
  (fn [state]
    (let [top-vs (peek (:vstack state))]
      (assoc state
	:vars (assoc-in top-vs [sym :vars] (:vars state))
	:vstack (pop (:vstack state))))))

(defn combine
  "Execute cmds in sequence"
  [& cmds]
  (fn [state]
    (loop [state state, cmds cmds]
      (if (:good state)
	(if (seq cmds)
	  (recur ((first cmds) state) (next cmds))
	  state)
	state))))

(defn perform-and
  "Perform cmds in sequence, starting each one with the same progress and input.
Effect of last cmd stays. If any cmds cause a failure, the match fails."
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
  "Like perform-and, only stops with first successful cmd, whose effects stay.
If all cmds fail, the match fails with the state of the most successful cmd."
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
  "Execute cmds with the input temporarily set to the result of (form-d) when
executed with implicit match state being the current match state."
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
  "With the implicit match state being the current match state, check to see if
((form-d)) returns false; if it does not, fail with the blame form being the
result of ((form-d)) and the message being the result of (mesg-d)."
  [form-d mesg-d]
  (fn [state]
    (binding [core/*current-match* (fixup-state state)]
      (let [res (form-d)]
	(if res
	  (fail-guard state (mesg-d) res)
	  state)))))

(defn make-state
  "Create a matching state with input"
  [input]
  {:vars {} :input [input] :istack ()
   :good true
   :dstack ["Bad syntax"] :ddstack [] :progress [0]
   :depth 0 :vstack ()
   :pop-describe false})
