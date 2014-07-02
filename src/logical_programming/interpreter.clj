(ns logical-programming.interpreter
  (:use [logical-programming.util]))


;;
;; This is the source of all knowledge.
;; Whatever gets loaded to the program goes here.
;; The interpreter knows only the things that are included in the well.
;;
;; Example well:
;; {:member [{:args [:A [:A :_]]
;;            :body []}
;;           {:args [:A [:_ :X]]
;;            :body [[:member [:A :X]]]}]}
;;
;; The definition of member(A, X) is mapped to key "member".
;; This way, the interpreter can use it for queries.
;;

(def well (atom {:member [{:args [:A [:A :_]]
                           :body []}
                          {:args [:A [:_ :X]]
                           :body [{:name :member
                                   :args [:A :X]}]}]}))



(defn args-same-type?
  "Tells is arguments A and B are the same type."
  [A B]
  (if (and (keyword? A) (keyword? B))
    true
    (if (and (vector? A) (vector? B))
      true
      false)))

(defn args-match?
  "Tells if the types of two clauses arguments match."
  [A B]
  (if (different? (count A) (count B))
    false
    true))


(defn match-goal
  "Matches the given goal to a vector of clauses from the well."
  [goal]
  (vec (filter #(args-match?
                 (:args goal)
                 (:args %))
               ((:name goal) @well))))

(defn interpret
  "Recieves a Prolog query and resolutes it."
  [query]
  (if (empty? query)
    true
    (let [goal (first query)
          clauses (match-goal goal)]
      [goal clauses])))

(defn ?-
  "A function that is used by the UI to interpred user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))
