(ns logic.interpreter
  (:use [logic.util]
        [logic.term]))

;;
;;  This is the source of all knowledge.
;;  Whatever gets loaded to the program goes here.
;;  The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom { :int [(-->functor :int [:A] [])]}))




(defn match
  "Returns a list of all matches of the goal with clauses from the knowledge-base."
  [goal pool])


(generate-functor (-->functor (-->structure :member [:A [:_ :| :X]])
                              [(-->structure :member [:A :X])])
                  {})

(defn interpret [query])


(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))

