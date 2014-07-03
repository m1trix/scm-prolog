(ns logical-programming.interpreter
  (:use [logical-programming.util]
        [logical-programming.terms]))

;;
;;  This is the source of all knowledge.
;;  Whatever gets loaded to the program goes here.
;;  The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom { }))


(defn interpret
  "Recieves a quary of rules and gives the solution."
  ([query] interpret [(atom query) (atom []) (atom {})])
  ;; >>> STEP 1 <<<
  ([query stack pool] ;; All these are atoms.
   ;; >>> STEP 2 <<<
   (while (not-empty @query)
     (let [goal (first @query)]
       goal))))

(interpret [[(-->structure :member [:X :Y])]])

(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))

