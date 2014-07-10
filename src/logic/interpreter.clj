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
                                  ;;  (>functor< :member [:A [:_ :| :X]] [:& [:member [:A :X]]])
                                 ]}))


(defn print-vars
  [main-pool work-pool]
  (loop [vals main-pool
         new-pool work-pool]
    (if (empty? (rest vals))
      (let [[out pool] (output-variable
                        (first vals)
                        new-pool)]
        (print-blue out))
      (let [[out pool] (output-variable
                        (first vals)
                        new-pool)]
        (print-blue out)
        (print ", ")
        (recur (disj vals (first vals))
               pool)))))


(def match-term)

(defn match-conjunct
  "Threats the conjunct as a query of goals.Matches the first goal from the Conjunct with a clause from the
   knowledge-base. Then it replaces its place with the body of that clause and evaluates all variables
   (if that is possible). Returns a vector of the 'new quary', new variables pool and the backtrack stack frame."
  [conjunct pool]
  (let [query (:elems conjunct)
        goal (first query)
        [new-goals new-pool stack-frame] (match-term goal pool)]
    (if (false? new-goals)
      [false pool []]
      [(->PrologConjunct (concat new-goals (rest query))) new-pool stack-frame])))


(match-conjunct (>conjunct< [:& [:member [:X [1 2]]]
                                [:member [:X [2 3]]]])
                {:X #{}})

(defn make-map
  "Makes a map from a given vector of keys by putting the same value to all ot them."
  [elems value]
  (reduce #(assoc %1 %2 value) {} elems))


(defn match-structure
  [struct pool]
  ;; Getting a vector of all functors and atoms with that name.
  (let [matches (->>
                 ;; Generating all new functors.
                 (map #(generate-functor % {})
                      ((:name struct) @knowledge-base))
                 ;; Creating local variable pools for each one.
                 (map #(vector (first %) (make-map (vals (second %)) #{})))
                 ;; Matching the structure to each one.
                 (map #(match-structure->functor struct (first %) (second %)))
                 ;; Removing those who failed to match.
                 (filter #(different? false (first %)))
                 ;; Merging the variable pools.
                 (mapv #(vector (first %) (merge pool (second %)))))]
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

(defn interpret [input-query])


(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))



