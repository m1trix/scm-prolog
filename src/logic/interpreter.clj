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
                                    (>functor< :member [:A [:_ :| :X]]
                                               [:& [:member [:A :X]]])]
                           :concat [(>functor< :concat [[] :Y :Y] [:&])
                                    (>functor< :concat [[:A :| :X] :Y [:A :| :Z]]
                                               [:& [:concat [:X :Y :Z]]])]}))


(defn print-vars
  [main-pool work-pool]
  (loop [vars main-pool
         pool work-pool]
    (when-not (empty? vars)
      (if (empty? (rest vars))
        (print-blue (first (output-variable (first vars) pool)))
        (let [[out new-pool] (output-variable (first vars) pool)]
          (print-blue out)
          (print ", ")
          (recur (rest vars)
                 new-pool))))))


(defn output-pool [input-pool]
  (loop [out "{"
         pool input-pool]
    (if (empty? pool)
      (str out "}")
      (let [elem (first pool)]
        (if (set? (second elem))
          (recur (str out
                      (keyword->string (first elem))
                      "="
                      (second elem)
                      "  ")
                 (rest pool))
          (recur (str out
                      (keyword->string (first elem))
                      "="
                      (output-term (second elem))
                      "  ")

                 (rest pool)))))))


(defn merge-queries
  [query-x query-y]
  (cond
   (same? [] query-x)
     query-y
   (prolog-structure? query-x)
     (cond
      (prolog-structure? query-y)
        (->PrologConjunct [query-x query-y])
      (prolog-conjunct? query-y)
        (->PrologConjunct (concat [query-x] (:elems query-y))))
   (prolog-conjunct? query-x)
     (cond
      (prolog-structure? query-y)
        (->PrologConjunct (conj (:elems query-x)
                                query-y))
      (prolog-conjunct? query-y)
        (->PrologConjunct (concat (:elems query-x)
                                  (:elems query-y))))))


(def match-term)


(defn match-conjunct
  "Threats the conjunct as a query of goals. Matches the first goal from the Conjunct with a clause from the
   knowledge-base. Then it replaces its place with the body of that clause and evaluates all variables
   (if that is possible). Returns a vector of the 'new quary', new variables pool and the backtrack stack frame."
  [conjunct pool start]
  (let [query (:elems conjunct)
        goal (first query)
        [new-goals new-pool [_ _ index]] (match-term goal pool start)]
    (if (false? new-goals)
      [false {} []]
      (let [rem-query (->PrologConjunct (vec (rest query)))
            new-conj (=conjunct= rem-query new-pool)
            final-conj (merge-queries new-goals new-conj)
            new-frame [conjunct pool index]]
        [final-conj new-pool new-frame]))))




(defn match-structure
  "Matches teh strcrute to a functor from the knowledge base."
  [struct pool start]
  (let [name (:name struct)
        all (name @knowledge-base)]
    (if (or (nil? all)
            (> start (count all)))
      [false {} []]
      (loop [clauses (subvec all start)
             index start]
        (if (empty? clauses)
          [false {} []]
          (let [[functor names] (generate-functor (first clauses) {})
                pool+names (merge pool (make-map (vals names) #{}))
                [new-goals new-pool] (match-structure->functor struct functor pool+names)]
            (if (false? new-goals)
              (recur (rest clauses)
                     (inc index))
              [new-goals new-pool [struct pool (inc index)]])))))))


(defn match-term
  ([term pool]
    (match-term term pool 0))
  ([term pool start]
    (cond
     (prolog-conjunct? term)
       (match-conjunct term pool start)
     (prolog-structure? term)
       (match-structure term pool start))))



(defn user-wants-more?
  []
  (let [sym (read-line)]
    (if (= sym ";")
      true
      false)))



(defn =pool=
  [input-pool]
  (loop [vars (mapv >variable< (keys input-pool))
         pool input-pool]
    (if (empty? vars)
      pool
      (let [var (first vars)
            value (=variable= var pool)]
        (recur (rest vars)
               (assoc pool
                 (:name var)
                 value))))))

(defn cut-pool
  [main-pool second-pool]
  (->>
   (filter #(or ((first %) main-pool)
                (set? (second %))) second-pool)
   (reduce #(assoc
              %1
              (first %2)
              (second %2))
           {})))

(defn refactor-pool
  [main-pool second-pool]
  (cut-pool main-pool (=pool= second-pool)))


(defn backtrack
  [input-stack]
  (loop [stack input-stack]
    (if (empty? stack)
      [false {} []]
      (let [[prev-query prev-pool index] (peek stack)
            [new-query new-pool new-frame] (match-term prev-query
                                                       prev-pool
                                                       index)]
        (if (false? new-query)
          (recur (pop stack))
          [new-query new-pool (push (pop stack)
                                    new-frame)])))))

(defn match-query
  [input-query input-pool input-stack main-pool]
  (loop [[query pool frame] (match-term input-query input-pool)
         stack input-stack]
    (if (false? query)
      (let [[prev-query prev-pool prev-stack] (backtrack stack)]
        (if (false? prev-query)
          [false {} []]
          (recur [prev-query prev-pool (peek prev-stack)]
                 (pop prev-stack))))
      (let [[frame-query frame-pool index] frame
            new-frame [(=term= frame-query frame-pool)
                       (refactor-pool main-pool frame-pool)
                       index]]
        [query
         (refactor-pool main-pool pool)
         (push stack new-frame)]))))

(defn ?-
  [input-query]
  (let [main-pool (get-term-vars input-query)]
    (loop [query input-query
           pool (make-map main-pool #{})
           stack []]

      (println "Query: " (output-term query))
      (println "Pool: " (output-pool pool))
      (println "Stack: " (mapv #(vector (output-term (first %))
                                        (output-pool (second %))) stack))
      (println)

      (if (false? query)
        (println-red "false.\n")

        (if (empty? (:elems query))
          (do
            (if (empty? main-pool)
              (print-green "true")
              (print-vars main-pool pool))
            (flush)

            (if (empty? stack)
              (println ".\n")
              (if (user-wants-more?)
                (let [[prev-query prev-pool prev-stack] (backtrack stack)]
                  (recur prev-query
                         prev-pool
                         prev-stack))
                (println))))
          (let [[new-query new-pool new-stack] (match-query query pool stack main-pool)]
            (if (false? new-query)
              (println-red "false.\n")
              (recur new-query
                     new-pool
                     new-stack))))))))
