(ns logic.interpreter
  (:use [logic.util]
        [logic.term]))

;;
;;  This is the source of all knowledge.
;;  Whatever gets loaded to the program goes here.
;;  The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom {"member" [(create [:fact "member" ["A" ["A" :| "_"]]])
                                     (create [:rule "member" ["A" ["_" :| "X"]]
                                              [:fact "member" ["A" "X"]]])]}))
(def debug (atom true))


(defprotocol Interpreter
  (conclude [term pool start]))


(defn conclude-fact
  [fact pool start]
  (let [name (:name (:atom fact))
        all (get @knowledge-base name)]
    (if (or (= -1 start)
            (nil? all))
      [false {} []]
      (loop [clauses (subvec all start)
             index start]
        (if (empty? clauses)
          [false {} []]
          (let [[term _] (generate (first clauses) {})
                [new-goals new-pool] (resolve fact term pool @debug)]
            (if (false? new-goals)
              (recur (rest clauses)
                     (inc index))
              (if (< (inc index) (count all))
                [new-goals new-pool [fact pool (inc index)]]
                [new-goals new-pool [fact pool -1]]))))))))


(extend-protocol Interpreter
  logic.term.PrologFact
  (conclude [term pool start] (conclude-fact term pool start)))



(defn consequence
  "Tries to resolve the Term. If the resolution fails it uses backtracking to try another one."
  [query]
  (loop [term query
         pool {}
         stack []]
    (cond
     (false? term) false
     (true? term) true
     :else
       (let [[new-term new-pool _] (conclude query pool 0)]
         (recur new-term new-pool stack)))))


(defn ?-
  "Concludes the query and prints the answer. If there are more than one
  solutions, it waits for the user to stop it or continue it."
  [query]
  (consequence query))

(?- (create [:fact "member" [2 [1 2]]]))
