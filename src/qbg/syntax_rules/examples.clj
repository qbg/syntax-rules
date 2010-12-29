(ns qbg.syntax-rules.examples
  (:use qbg.syntax-rules))

(defsyntax-rules ex-dotimes []
  (dotimes [i upper] body ...)
  (let [n (long upper)]
    (loop [i 0]
      (when (< i n)
	body ...
	(recur (unchecked-inc i))))))

(defsyntax-rules ex--> []
  (-> form) form
  (-> form (f args ...)) (f form args ...)
  (-> form f) (f form)
  (-> form f1 fs ...) (-> (-> form f1) fs ...))
  