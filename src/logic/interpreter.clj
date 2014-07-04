(ns logic.interpreter
  (:use [logic.util]
        [logic.term]))

;;
;;  This is the source of all knowledge.
;;  Whatever gets loaded to the program goes here.
;;  The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom { :int [(-->functor :int [:A] [])]}))




(defn match [goal pool]
  (let [possible ((:name goal) @knowledge-base)]
    (if (false? possible)
      false
      (mapv #(unify goal (:head %) pool) possible))))



(defn interpret
  ([query] interpret ((atom query) (atom []) (atom {})))
  ;; >>> STEP 1 <<<
  ([query stack pool]
   ;; >>> STEP 2 <<<
   (while (not-empty @query)
     (let [goal (first @query)
           first (match @pool)]

       ))))


(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))

