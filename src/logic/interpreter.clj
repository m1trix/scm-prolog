(ns logic.interpreter
  (:use [logic.util]
        [logic.term]))

;; ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
;; ##                                                                                  ##
;; ##  This is the source of all knowledge.                                            ##
;; ##  Whatever gets loaded to the program goes here.                                  ##
;; ##  The interpreter knows only the things that are included in the knowledge-base.  ##
;; ##                                                                                  ##
;; ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##

(def knowledge-base (atom {"member" [(create [:fact "member" ["A" ["A" :| "_"]]])
                                     (create [:rule "member" ["A" ["_" :| "X"]]
                                              [:fact "member" ["A" "X"]]])]
                           "concat" [(create [:fact "concat" [[] "Y" "Y"]])
                                     (create [:rule "concat" [["A" :| "X"] "Y" ["A" :| "Z"]]
                                              [:fact "concat" ["X" "Y" "Z"]]])]
                           "insert" [(create [:fact "insert" ["A" "X" ["A" :| "X"]]])
                                     (create [:rule "insert" ["A" ["B" :| "X"] ["B" :| "Y"]]
                                              [:fact "insert" ["A" "X" "Y"]]])]
                           "perm" [(create [:fact "perm" [["A"] ["A"]]])
                                   (create [:rule "perm"
                                            [["A" :| "X"] "Z"]
                                            [:conj [:fact "perm" ["X" "Y"]]
                                                   [:fact "insert" ["A" "Y" "Z"]]]])]}))
(def trace (atom true))
(def watch-stack (atom false))
(def watch-pool (atom false))
(def watch-resolve (atom false))
(def debug-info (atom true))


(defprotocol Interpreter
  (replace [query term])
  (prove [term [query pool stack] depth start]))


(defn evaluate-pool
  [pool]
  (->>
   (map create (keys pool))
   (reduce (fn [pool var]
             (let [[new-val new-pool] (substitude-variable var pool)]
               new-pool))
           pool)))


(defn output-pool [pool]
  (let [out (reduce (fn [out name]
                      (str out
                           "\n     "
                           name
                           " = "
                           (output (get pool name) pool)))
                    ""
                    (keys pool))]
    (str "  {" out " }")))

(defn output-stack [stack]
   (reduce #(str %1 "\n    [" (output (first %2) (second %2)) " " (nth %2 2) "]") "" stack))


(defn replace-fact
  "The query is a fact, so we replace it with the new one."
  [fact term]
  term)


(defn prove-fact [fact [query pool stack] depth start]
  (let [name (:name (:atom fact))
        all (get @knowledge-base name)]
    (when @trace
      (print-green "  Call: ")
      (print (str "(" depth ") "))
      (print (output fact pool))
      (print " ? ")
      (flush)
      (read-line))
    (if (nil? all)
      (do
        (when @trace
          (print-red "  Fail: ")
          (print (str "(" depth ") "))
          (print (output fact pool))
          (print " ? ")
          (flush)
          (read-line))
        [false {} []])
      (loop [clauses (subvec all start)
             index start]
        (if (empty? clauses)
          (do
            (when @trace
              (print-red "  Fail: ")
              (print (str "(" depth ") "))
              (print (output fact pool))
              (print " ? ")
              (flush)
              (read-line))
            [false {} stack])
          (let [[term _] (generate (first clauses) {})
                [status new-term resolve-pool] (resolve fact term pool)
                new-pool (evaluate-pool resolve-pool)]
            (when @watch-resolve
              (print "    ")
              (println-gray (output fact pool))
              (println "      ~>")
              (print "    ")
              (println-gray (output term pool))
              (println "      =>")
              (if (false? status)
                (do
                  (when @watch-pool
                    (println-gray "  >>> POOL <<<")
                    (println-gray (output-pool new-pool)))
                  (when @watch-stack
                    (println-gray "\n  >>> STACK <<<")
                    (println-gray (output-stack stack)))
                  (println-red "    false"))
                (println "   " (output new-term new-pool))))
            (if (false? status)
              (recur (rest clauses)
                     (inc index))
              (let [new-stack (if (>= (inc index) (count all))
                                stack
                                (do
                                  (when @debug-info
                                    (println-gray "  INFO: Pushing a choisepoint:")
                                    (println-gray (str "  INFO: [" (output query pool) " " (inc index) "]")))
                                  (conj stack [query pool (inc index)])))]
                (when @watch-pool
                  (println-gray "  >>> POOL <<<")
                  (println-gray (output-pool new-pool)))
                (when @watch-stack
                  (println-gray "\n  >>> STACK <<<")
                  (println-gray (output-stack new-stack)))
                (if (true? status)
                  (do
                    (when @trace
                      (print-green "  Exit: ")
                      (print (str "(" depth ") "))
                      (print (output new-term new-pool))
                      (print " ? ")
                      (flush)
                      (read-line))
                    [true new-pool new-stack])
                  (let [new-query (replace query new-term)]
                    (prove new-term [new-query new-pool new-stack] depth 0)))))))))))


(defn replace-rule [rule term]
  (->PrologRule (:head rule)
                (replace (:body rule) term)))


(defn prove-rule [rule [query pool stack] depth start]
  (let [[status new-pool new-stack]
        (prove (:body rule) [query pool stack] (inc depth) start)]
    (if (false? status)
      (do
        (when @trace
          (print-red "  Fail: ")
          (print (str "(" depth ") "))
          (print (output (:head rule) pool))
          (print " ? ")
          (flush)
          (read-line))
        [false {} stack])
      (do
        (when @trace
          (print-green "  Exit: ")
          (print (str "(" depth ") "))
          (print (output (:head rule) new-pool))
          (print " ? ")
          (flush)
          (read-line))
        [true new-pool new-stack]))))


(defn prove-negation [neg [query pool stack] depth start]
  (let [[answer _ _] (prove (:term neg) [(:term neg) pool []] depth 0)]
    (if (false? answer)
      [true pool stack]
      [false pool stack])))


(defn prove-conjunction [conj [input-query input-pool input-stack] depth input-start]
  (loop [goals (:terms conj)
         query input-query
         pool input-pool
         stack input-stack
         start input-start]
    (if (empty? goals)
      [true pool stack]
      (let [[answer new-pool new-stack]
            (prove (first goals) [query pool stack] depth start)]
        (if (false? answer)
          [false new-pool new-stack]
          (recur (rest goals)
                 (replace query true)
                 new-pool
                 new-stack
                 0))))))


(defn replace-conjunction [conj term]
  (if (true? term)
    (->PrologConjunction (subvec (:terms conj) 1))
    (->PrologConjunction (assoc (:terms conj) 0 term))))


(extend-protocol Interpreter
  logic.term.PrologRule

  (prove [term [query pool stack] depth start] (prove-rule term [query pool stack] depth start))
  (replace [query term] (replace-rule query term))


  logic.term.PrologFact

  (replace [query term] (replace-fact query term))
  (prove [term [query pool stack] depth start] (prove-fact term [query pool stack] depth start))


  logic.term.PrologNegation

  (prove [term [query pool stack] depth start] (prove-negation term [query pool stack] depth start))


  logic.term.PrologConjunction

  (replace [query term] (replace-conjunction query term))
  (prove [term [query pool stack] depth start] (prove-conjunction term [query pool stack] depth start)))





(defn print-var [var work-pool]
  (let [name (:name var)
        value (get work-pool name)]
    (if (nil? value)
      ""
      (str name " = " (output value work-pool)))))


(defn print-answer [main-pool work-pool]
  (loop [vars (reduce (fn [pool var]
                        (if (nil? (get work-pool (:name var)))
                          (disj pool var)
                          pool))
                      main-pool main-pool)]
    (when-not (empty? vars)
      (let [out (print-var (first vars) work-pool)]
        (when-not (= "" out)
          (print-blue out)
          (print " ")
          (when (next vars)
            (println ",")))
        (recur (rest vars))))))


(defn ?-
  "Proves the query and prints the answer. If there are more than one
  solutions, it waits for the user to stop it or continue it."
  [input]
  (let [main-pool (get-variables input)]
    (loop [query input
           work-pool {}
           stack []
           index 0]
      (let [[answer new-pool new-stack] (prove query [query work-pool stack] 1 index)]
        (if (true? answer)
          (do
            (if (empty? main-pool)
              (print-green "true ")
              (print-answer main-pool new-pool))
            (flush)
            (if (empty? new-stack)
              (println ".\n")
              (if (= ";" (read-line))
                (let [[old-query old-pool old-index] (peek new-stack)]
                  (recur old-query old-pool (pop new-stack) old-index)))))
          (if (empty? new-stack)
            (do
              (when @debug-info
                (println-gray "  INFO: Stack empty. No more solutions."))
              (println-red "false.\n"))
            (let [[old-query old-pool old-index] (peek new-stack)]
              (when @debug-info
                println-gray "  INFO: Stack not empty. Redoing top frame.")
              (recur old-query old-pool (pop new-stack) old-index))))))))
