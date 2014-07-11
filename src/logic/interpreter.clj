(ns logic.interpreter
  (:use [logic.util]
        [logic.term]))

;;
;;  This is the source of all knowledge.
;;  Whatever gets loaded to the program goes here.
;;  The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom {:member [(>functor< :member [:A [:A :| :_]]
                                               [:&])
                                    (>functor< :member [:A [:_ :| :X]] [:& [:member [:A :X]]])
                                 ]}))


(defn print-vars
  [main-pool work-pool]
  (loop [vars main-pool
         pool work-pool]
    (if (empty? (rest vars))
      (print-blue (first (output-variable (first vars) pool)))
      (let [[out new-pool] (output-variable (first vars) pool)]
        (print-blue out)
        (print ", ")
        (recur (rest vars)
               new-pool)))))


(def match-term)

(defn match-conjunct
  "Threats the conjunct as a query of goals.Matches the first goal from the Conjunct with a clause from the
   knowledge-base. Then it replaces its place with the body of that clause and evaluates all variables
   (if that is possible). Returns a vector of the 'new quary', new variables pool and the backtrack stack frame."
  [conjunct pool]
  (let [query (:elems conjunct)
        goal (first query)
        [new-goals new-pool stack-frame] (match-term goal pool)
        new-stack-frame (mapv #(vector (->PrologConjunct (concat (first %) (rest query))) (second %)) stack-frame)]
    (if (false? new-goals)
      [false pool []]
      [(->PrologConjunct (concat new-goals (rest query))) new-pool new-stack-frame]
      )))


(defn match-structure
  [struct pool]
  ;; Getting a vector of all functors and atoms with that name.
  (let [matches (->>
                 ;; Generating all new functors.
                 (map #(generate-functor % {})
                      ((:name struct) @knowledge-base))

                 ;; Creating local variable pools for each one.
                 (map #(vector (first %) (make-map (vals (second %)) #{})))

                 ;; Merging the variable pools.
                 (mapv #(vector (first %) (merge pool (second %))))
                 ;; Matching the structure to each one.
                 (map #(match-structure->functor struct (first %) (second %)))
                 ;; Removing those who failed to match.
                 (filter #(different? false (first %)))
                 )]
    (if (empty? matches)
      [false pool []]
      (let [[top-match top-pool] (first matches)]
        [top-match top-pool (rest matches)]))))




(defn match-term
  [term pool]
  (cond
   (prolog-conjunct? term)
     (match-conjunct term pool)
   (prolog-structure? term)
     (match-structure term pool)))



(defn backtrack
  [stack]
  (let [prev-states (peek stack)
        [prev-query prev-pool] (first prev-states)
        next-states (rest prev-states)]
    (if (empty? next-states)
      [prev-query prev-pool (pop stack)]
      [prev-query prev-pool (conj (pop stack) next-states)])))



(defn user-wants-more?
  []
  (let [sym (read-line)]
    (if (= sym ";")
      true
      false)))


(defn ?- [input-query]
  (let [main-pool (get-term-vars input-query)]
    (loop [query input-query
           pool (make-map main-pool #{})
           stack []]

      ;; No more goals
      (if (empty? (:elems query))
        (do
          ;; Printing results.
          (if (empty? main-pool)
            (print-green "true")
            (do
              (print-vars main-pool pool)))
          (flush)
          (if (empty? stack)
            ;; If the stack is empty, there are no more solutions.
            (println ".\n")
            (when (user-wants-more?)
              (let [[prev-query prev-pool new-stack] (backtrack stack)]
                  (recur prev-query
                         prev-pool
                         new-stack)))))
        (do
          (let [[new-query new-pool stack-frame] (match-term query pool)]
            (if (false? new-query)
              (if (empty? stack)
                (print-err "false.\n")
                (let [[prev-query prev-pool new-stack] (backtrack stack)]
                  (recur prev-query
                         prev-pool
                         new-stack)))
              (if (empty? stack-frame)
                (recur new-query new-pool stack)
                (recur new-query new-pool (conj stack stack-frame))))))))))

