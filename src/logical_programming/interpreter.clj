(ns logical-programming.interpreter
  (:use [logical-programming.util]
        [logical-programming.terms]))


(declare unify)

;;
;; This is the source of all knowledge.
;; Whatever gets loaded to the program goes here.
;; The interpreter knows only the things that are included in the knowledge-base.
;;
;; Example knowledge-base:
;; {:member [{:args [:A [:A :_]]
;;            :body []}
;;           {:args [:A [:_ :X]]
;;            :body [[:member [:A :X]]]}]}
;;

(def knowledge-base (atom {:member [{:args [:A {:head [:A] :tail :_}]
                           :body []}
                          {:args [:A {:head [:A] :tail :X}]
                           :body [{:name :member
                                   :args [:A :X]}]}]}))


(defn interpret[query])


(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))

