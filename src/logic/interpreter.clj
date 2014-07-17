(ns logic.interpreter
  (:use [logic.util]
        [logic.term]
        [logic.math]))

;;
;;  This is the source of all knowledge.
;;  Whatever gets loaded to the program goes here.
;;  The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom math-functions))


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
  (let [new-query (cond
                   (or (same? [] query-x)
                       (same? () query-x)
                       (prolog-number? query-x))
                   query-y
                   (prolog-structure? query-x)
                   (cond
                    (prolog-structure? query-y)
                    (->PrologConjunct [query-x query-y])
                    (prolog-conjunct? query-y)
                    (->PrologConjunct (concat [query-x] (:elems query-y)))
                    (prolog-disjunct? query-y)
                    (->PrologDisjunct (concat [query-x] (:elems query-y))))
                   (prolog-conjunct? query-x)
                   (cond
                    (prolog-structure? query-y)
                    (->PrologConjunct (conj (:elems query-x)
                                            query-y))
                    (prolog-conjunct? query-y)
                    (->PrologConjunct (concat (:elems query-x)
                                              (:elems query-y)))
                    (prolog-disjunct? query-y)
                    (->PrologDisjunct (concat [query-x] (:elems query-y))))
                   (prolog-disjunct? query-x)
                   (cond
                    (prolog-structure? query-y)
                    (->PrologDisjunct (conj (:elems query-x) query-y))
                    (prolog-conjunct? query-y)
                    (->PrologConjunct (concat [query-x] (:elems query-y)))
                    (prolog-disjunct? query-y)
                    (->PrologDisjunct (concat (:elems query-x)
                                              (:elems query-y)))))]
    (if (and (or (prolog-conjunct? new-query)
                 (prolog-disjunct? new-query))
             (empty? (:elems new-query)))
      []
      new-query)))



(def match-term)


(defn match-conjunct
  "Threats the conjunct as a query of goals. Matches the first goal from the Conjunct with a clause from the
   knowledge-base. Then it replaces its place with the body of that clause and evaluates all variables
   (if that is possible). Returns a vector of the 'new quary', new variables pool and the backtrack stack frame."
  [conjunct pool start]
  (let [query (:elems conjunct)
        goal (first query)
        [new-goals new-pool [frame-goals frame-pool index]] (match-term goal pool start)]
    (if (false? new-goals)
      [false {} []]
      (let [rem-query (->PrologConjunct (vec (rest query)))
            new-conj (=conjunct= rem-query new-pool)
            final-conj (merge-queries new-goals new-conj)
            new-frame [(merge-queries frame-goals rem-query) frame-pool index]]
        [final-conj new-pool new-frame]))))


(defn match-disjunct
  [disjunct pool start-index]
  (loop [query (:elems disjunct)
         start start-index]
    (if (empty? query)
      [false {} []]
      (if (= start -1)
        (recur (rest query) 0)
        (let [goal (first query)
              [new-goals new-pool [_ _ index]] (match-term goal pool start)]
          (if (false? new-goals)
            (recur (rest query) 0)
            (let [new-frame [(->PrologDisjunct (vec query)) pool index]]
              [new-goals new-pool new-frame])))))))


(defn match-structure
  "Matches teh strcrute to a functor from the knowledge base."
  [struct pool start]
  (let [name (:name struct)
        all (name @knowledge-base)]
    (if (or (nil? all)
            (= start -1))
      [false {} []]
      (loop [clauses (subvec all start)
             index start]
        (if (empty? clauses)
          [false {} []]
          (let [[term names] (generate-term (first clauses) {})
                pool+names (merge pool (make-map (vals names) #{}))
                [new-goals new-pool] (match-structure->term struct term pool+names)]
            (if (false? new-goals)
              (recur (rest clauses)
                     (inc index))
              (if (< (inc index) (count all))
                [new-goals new-pool [struct pool (inc index)]]
                [new-goals new-pool [struct pool -1]]))))))))


(defn match-math
  [math pool index]
  (if (= index -1)
    [false {} []]
    (if (prolog-math? math)
      (let [[new-left pool-left _] (match-math (:left math) pool 0)]
        (if (false? new-left)
          [false {} []]
          (let [[new-right pool-right _] (match-math (:right math) pool-left 0)]
            (if (false? new-right)
              [false {} []]
              (let [method ((:name math) @knowledge-base)]
                (if (nil? method)
                  [false {} []]
                  (let [[new-method names] (generate-method method {})
                        pool+names (merge pool (make-map (vals names) #{}))
                        new-math (->PrologMath (:name math) new-left new-right)
                        [value new-pool] (match-math->method new-math new-method pool+names)]
                    (if (false? value)
                      [false {} []]
                      [value new-pool [[] pool -1]]))))))))
      [math pool []])))


(defn match-term
  ([term pool]
    (match-term term pool 0))
  ([term pool start]
    (cond
     (prolog-conjunct? term)
       (match-conjunct term pool start)
     (prolog-structure? term)
       (match-structure term pool start)
     (prolog-math? term)
       (match-math term pool start)
     (prolog-disjunct? term)
       (match-disjunct term pool start))))



(defn user-wants-more?
  []
  (let [sym (read-line)]
    (if (= sym ";")
      true
      false)))



(defn set-value
  [var pool]
  (let [name (:name var)]
    (if (set? (name pool))
      (name pool)
      (=variable= var pool))))


(defn =pool=
  [input-pool]
  (loop [vars (mapv >variable< (keys input-pool))
         pool input-pool]
    (if (empty? vars)
      pool
      (let [var (first vars)
            value (set-value var pool)]
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
  (->>
   (=pool= second-pool)
   (cut-pool main-pool)))


(defn backtrack
  [input-stack]
  (loop [stack input-stack]
    (if (empty? stack)
      [false {} []]
      (let [[prev-query prev-pool index] (peek stack)
            [new-query new-pool new-frame] (match-term prev-query
                                                       prev-pool
                                                       index)]
        (if (not new-query)
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

;;       (println "Query: " (output-term query))
;;       (println "Pool: " (output-pool pool))
;;       (println "Stack: " (mapv #(vector (output-term (first %))
;;                                         (output-pool (second %))
;;                                         (nth % 2)) stack))
;;       (println)

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
