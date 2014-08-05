 (ns logic.interpreter
  (:use [logic.util]
        [logic.term]))


; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  This is the source of all knowledge.                                           # ;
; #  Whatever gets loaded to the program goes here.                                 # ;
; #  The interpreter knows only the things that are included in the knowledge-base. # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(def knowledge-base
  (atom {"member"
         [(create [:fact "member" ["A" ["A" :| "_"]]])
          (create [:rule "member" ["A" ["_" :| "X"]]
                   [:fact "member" ["A" "X"]]])]

         "insert"
         [(create [:fact "insert" ["A" "X" ["A" :| "X"]]])
          (create [:rule "insert" ["A" ["B" :| "X"] ["B" :| "Y"]]
                   [:fact "insert" ["A" "X" "Y"]]])]

         "perm"
         [(create [:fact "perm" [["A"] ["A"]]])
          (create [:rule "perm" [["A" :| "X"] "Z"]
                   [:conj
                    [:fact "perm" ["X" "Y"]]
                    [:fact "insert" ["A" "Y" "Z"]]]])]}))


(def debug (atom {:trace true
                  :info false
                  :redo false}))


(defmulti prove #(-> %& first type))
(defmulti replace #(-> %& first type))


(defn evaluate-pool
  [pool]
  (reduce #(second (reshape %2 %1)) pool (keys pool)))


(evaluate-pool {(create "X") (create [1 :| "Y"])
                (create "Y") (create [2 3])})


(defn print-var [var work-pool]
  (let [value (get work-pool var)]
    (str (:name var) " = " (output value work-pool))))


(defn print-answer [main-pool work-pool]
  (let [not-nil (reduce (fn [pool var]
                        (if (or (nil? (work-pool var))
                                (set? (work-pool var)))
                          (disj pool var)
                          pool))
                      main-pool main-pool)]

    (if (empty? not-nil)
      (print-green "true ")
      (loop [vars not-nil]
        (when-not (empty? vars)
          (let [out (print-var (first vars) work-pool)]
            (when-not (= "" out)
              (print-blue out)
              (print " ")
              (when (next vars)
                (println ",")))
            (recur (rest vars))))))))


(defmethod prove logic.term.PrologFact
  [fact query pool stack depth start]
  (let [name (-> fact :atom :name)
        all (@knowledge-base name)
        limit (count all)]
    (when (:info @debug)
      (println-gray (str "  INFO: Trying to prove a Fact with start index " start ".")))
    (if (>= start limit)
      [false {} stack]
      (do
        (when (:trace @debug)
          (if (:redo @debug)
            (do
              (print-yellow "  Redo: ")
              (swap! debug assoc :redo false))
            (print-green "  Call: "))
          (print (str "(" depth ")") (output fact pool) "? ")
          (flush)
          (read-line))
      (loop [clauses (subvec all start)
             index start]
        (if (empty? clauses)
          (do
            (when (:info @debug)
              (println-gray "  INFO: Proving failed! No clause mathes the Fact."))
            (when (:trace @debug)
              (print-red "  Fail: ")
              (print (str "(" depth ")") (output fact pool) "? ")
              (flush)
              (read-line))
            [false {} stack])
          (let [[target _] (generate (first clauses) {})

                [status new-term res-pool]
                (resolve fact target pool)

                new-pool (evaluate-pool res-pool)

                new-stack (conj stack [query pool (inc index)])]
            (cond

             (false? status)
             (do
               (when (:info @debug)
                 (println-gray (str "  INFO: Failing to match with index " index ".")))
               (recur (rest clauses) (inc index)))

             (true? status)
             (do
               (when (:info @debug)
                 (println-gray (str "  INFO: Proving successful! Resolved with clause " index ".")))
               (when (:trace @debug)
                 (print-green "  Exit: ")
                 (print (str "(" depth ")") (output fact new-pool) "? ")
                 (flush)
                 (read-line))
             [true new-pool new-stack])

             :else
             (do
               (when (:info @debug)
                 (println-gray (str "  INFO: Proving successful! Resolved with clause " index ".")))
               (let [[re-query re-pool] (replace query new-term new-pool)]
                 (prove new-term re-query re-pool new-stack depth 0)))))))))))


(defmethod replace logic.term.PrologFact
  [fact term pool]
  (reshape term pool))


(defmethod prove logic.term.PrologRule
  [rule query pool stack depth start]
  (when (:info @debug)
    (println-gray "  INFO: Trying to prove a Rule."))
  (let [[answer new-pool new-stack]
        (prove (:body rule) query pool stack (inc depth) start)]
    (if (false? answer)
      (do
        (when (:info @debug)
          (println-gray "  INFO: Failed to prove a Rule."))
        (when (:trace @debug)
          (when-not (:redo @debug)
            (print-red "  Fail: ")
            (print (str "(" depth ")") (output (:head rule) pool) "? ")
            (flush)
            (read-line)))
        [false {} new-stack])
      (do
        (when (:info @debug)
          (println-gray "  INFO: Rule was successfuly proved."))
        (when (:trace @debug)
          (print-green "  Exit: ")
          (print (str "(" depth ")") (output (:head rule) new-pool) "? ")
          (flush)
          (read-line))
        [true new-pool new-stack]))))

(defmethod replace logic.term.PrologRule
  [rule term pool]
  (let [[new-term new-pool] (replace (:body rule) term pool)]
    [(->PrologRule (:head rule) new-term)
     new-pool]))


(defn interpret [term in-pool in-stack depth start]
  (loop [query term
         pool in-pool
         stack in-stack
         index start]
    (let [[answer new-pool new-stack]
          (prove query query pool stack depth index)]
      (if (false? answer)
        (if (empty? stack)
          [false {} []]
          (let [[old-query old-pool old-index] (peek stack)]
            (swap! debug assoc :redo true)
            (recur old-query old-pool (pop stack) old-index)))
        [true new-pool new-stack]))))


(defn ?-
  "Proves the query and prints the answer. If there are more than one
  solutions, it waits for the user to stop it or continue it."
  [input]
  (let [main-pool (get-vars input)]
    (loop [query input
           pool {}
           stack []
           index 0]
      (let [[answer new-pool new-stack] (interpret query pool stack 1 index)]
        (if (false? answer)
          (println-red "false.")
          (do
            (print-answer main-pool new-pool)
            (flush)
            (if (empty? new-stack)
              (println ".")
              (when (= ";" (read-line))
                (let [[old-query old-pool old-index] (peek new-stack)]
                  (swap! debug assoc :redo true)
                  (recur old-query old-pool (pop new-stack) old-index))))))))))
