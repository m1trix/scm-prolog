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
  [goal pool]
  (let [possible ((:name goal) @knowledge-base)]
    (if (false? possible)
      false
      (loop [functors []
             clauses possible
             new-pool]

        (if (empty? clauses)
          [functors new-pool]
          (let [top-clase (first clauses)
                new-struct (generate-structure (:head top-clause))
                [matched new-pool] (unify-structure goal new-struct new-pool)]
            (if (false? matched)
              [false pool])))))))


(defn interpret [query])


(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))

