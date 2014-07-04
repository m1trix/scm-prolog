(ns logic.interpreter
  (:use [logic.util]
        [logic.term]))

;;
;;  This is the source of all knowledge.
;;  Whatever gets loaded to the program goes here.
;;  The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom { :member [(-->functor :member [:A [:A :| :_]] [])
                                     (-->functor :member [:A [:_ :| :X]]
                                                 [[:member [:A :X]]])]}))


(defn match
  "Returns a list of all matches of the goal with clauses from the knowledge-base."
  [goal pool]
  (let [all ((:name goal) @knowledge-base)]
    (if (nil? all)
      []
      (loop [result []
             others all]
        (if (empty? others)
          result
          (let [elem (first others)
                [new-functor new-pool] (match-to-functor goal elem pool)]
            (if (false? new-functor)
              (recur result
                     (rest others))
              (recur (conj result [new-functor new-pool])
                     (rest others)))))))))

(match (-->structure :member [1 [1 2]]) {})
(match-to-functor (-->structure :member [1 2])
                  (-->structure :member [:A [:A :| :_]])
                  {})

(defn interpret [query])


(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))

