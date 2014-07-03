(ns logical-programming.interpreter
  (:use [logical-programming.util]
        [logical-programming.terms]))

;;
;; This is the source of all knowledge.
;; Whatever gets loaded to the program goes here.
;; The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom { }))



(defn -: [name args body]
  (-->structure name args))


(-: :member [:A :X [:A :B]] [])
(-: :int [0 [1 2]] [])






(defn interpret[query])


(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))

