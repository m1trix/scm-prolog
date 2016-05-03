(ns logic.interpreter
  (:use logic.util
        logic.term
        logic.math
        clojure.java.io
        logic.parser)
  (:refer-clojure :exclude [resolve replace]))


; =================================================================================== ;
; =================================================================================== ;

(def debug (atom {:exit false
                  :trace false
                  :redo false}))


(def user-terms
  (atom {}))


(def built-in-terms (atom built-in-math))


(defn compile-file [[s] pool]
  (with-open [rdr (reader (clojure.string/replace (:string s) #"\"" ""))]
    (doseq [line (line-seq rdr)]
      (let [terms (parse line)]
        (doseq [term terms]
          (let [name (get-name term)
                built-in (@built-in-terms name)
                user-defined (@user-terms name)]
            (cond

             (not (nil? built-in))
             (throw (Exception. (str "Term \"" name "\" already exists.")))

             (nil? user-defined)
             (swap! user-terms assoc name [term])

             :else
             (swap! user-terms assoc name (conj user-defined term))))))))
  [true pool])


(swap! built-in-terms merge
  {"trace" [(create [:form "trace" [] (fn [_ pool] (swap! debug assoc :trace true) [true pool])])]
   "notrace" [(create [:form "notrace" [] (fn [_ pool] (swap! debug assoc :trace false) [true pool])])]
   "halt" [(create [:form "halt" [] (fn [_ pool] (swap! debug assoc :exit true) [true pool])])]
   "compile" [(create [:form "compile" ["Filepath"] compile-file])]})


(reset! user-terms @built-in-terms)


; =================================================================================== ;
; =================================================================================== ;


(defmulti prove #(-> %& first type))
(defmulti replace #(-> %& first type))


(defmethod replace :default
  [what _ pool]
  [what pool])


(defn evaluate-pool
  [pool]
  (reduce #(second (reshape %2 %1)) pool (keys pool)))


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
      (print-green "true")
      (loop [vars not-nil]
        (when-not (empty? vars)
          (let [out (print-var (first vars) work-pool)]
            (when-not (= "" out)
              (print-blue out)
              (when (next vars)
                (println " ,")))
            (recur (rest vars))))))))


(defn output-stack [stack]
  (println-gray "    STACK:")
  (doseq
    [[term pool index] stack]
    (do
      (print "      ")
      (print (str "[" index "] "))
      (println-gray (output term pool)))))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Tracing methods.                                                               # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;



(defn trace-call [term pool depth]
  (if (:redo @debug)
    (do
      (print-yellow "  Redo: ")
      (swap! debug assoc :redo false))
    (print-green "  Call: "))
  (print (str "(" depth ")") (output term pool) "? ")
  (flush)
  (read-line))


(defn trace-fail [term pool depth]
  (print-red "  Fail: ")
  (print (str "(" depth ")") (output term pool) "? ")
  (flush)
  (read-line))


(defn trace-exit [term pool depth]
  (print-green "  Exit: ")
  (print (str "(" depth ")") (output term pool) "? ")
  (flush)
  (read-line))



; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Interpreting a Prolog Atom.                                                    # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;


(defmethod prove logic.term.Atom
  [atom query pool stack depth start]
  (let [[target] (@built-in-terms (:name atom))]
    (if (nil? target)
      (let [fact (create [:fact (:name atom) []])
            [new-query _] (replace query fact pool)]
        (prove fact new-query pool stack depth start))

      (let [[status _ _] (resolve atom target pool)]
        (if (true? status)
          [true pool stack]
          [false {} stack])))))


(defmethod replace logic.term.Atom
  [atom term pool]
  [term pool])



; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Interpreting a Prolog Variable.                                                # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;


(defmethod prove logic.term.Variable
  [var query pool stack depth start]
  (let [val (extract var pool)]
    (if (or (nil? val)
            (set? val))
      (throw (Exception. (str "Cannot prove unbound Variable \"" (:name var) "\".")))
      (let [[new-query new-pool] (replace query val pool)]
        (prove val new-query new-pool stack depth start)))))


(defmethod replace logic.term.Variable
  [var term pool]
  [term pool])


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Interpreting a Prolog Fact.                                                    # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;


(defmethod prove logic.term.PrologFact
  [fact query pool stack depth start]
  (let [name (-> fact :atom :name)
        all (@user-terms name)
        limit (count all)]

    (if (nil? all)
      (throw (Exception. (str "Unknown Term \""  name "\"."))))
    (when (:trace @debug) (trace-call fact pool depth))
    (loop [clauses (subvec all start)
           index start]
      (if (empty? clauses)
        (do
          (when (:trace @debug) (trace-fail fact pool depth))
          [false {} stack])
        (do
          (let [[target _] (obsolete-generate (first clauses) {})
                [status new-term res-pool] (resolve fact target pool)]
            (if (false? status)
              (recur (rest clauses) (inc index))
              (let [new-pool (evaluate-pool res-pool)
                    new-stack (if (>= (inc index) limit)
                                stack
                                (conj stack [query pool (inc index)]))]
                (if (true? status)
                  (do
                    (when (:trace @debug) (trace-exit fact new-pool depth))
                    [true new-pool new-stack])
                  (let [[rep-query rep-pool]
                        (replace query new-term new-pool)]
                    (prove new-term rep-query rep-pool new-stack depth 0)))))))))))


;; (defmethod prove logic.term.PrologFact
;;   [fact query pool stack depth start]
;;   (let [name (-> fact :atom :name)
;;         all (into [] (concat (built-in-terms name)
;;                              (@user-terms name)))
;;         limit (count all)]
;;     (when (:info @debug)
;;       (print-gray "    INFO: Proving ")
;;       (print (output fact pool))
;;       (println-gray "."))
;;     (when (-> @debug :watch :query)
;;       (println-gray (str "    GOAL:  " (output fact pool)))
;;       (println-gray (str "    QUERY: " (output query pool))))
;;     (when (-> @debug :watch :stack)
;;       (output-stack stack))
;;     (if (>= start limit)
;;       [false {} stack]
;;       (do
;;         (when (:trace @debug)
;;           (if (:redo @debug)
;;             (do
;;               (print-yellow "  Redo: ")
;;               (swap! debug assoc :redo false))
;;             (print-green "  Call: "))
;;           (print (str "(" depth ")") (output fact pool) "? ")
;;           (flush)
;;           (read-line))
;;       (loop [clauses (subvec all start)
;;              index start]
;;         (if (empty? clauses)
;;           (do
;;             (when (:trace @debug)
;;               (print-red "  Fail: ")
;;               (print (str "(" depth ")") (output fact pool) "? ")
;;               (flush)
;;               (read-line))
;;             [false {} stack])
;;           (let [[target _] (obsolete-generate (first clauses) {})

;;                 [status new-term res-pool]
;;                 (resolve fact target pool)

;;                 new-pool (evaluate-pool res-pool)

;;                 new-stack (conj stack [query pool (inc index)])]
;;             (cond

;;              (false? status)
;;              (do
;;                (when (:info @debug)
;;                  (print-gray "    INFO: ")
;;                  (print (output fact pool))
;;                  (print-gray "[")
;;                  (print index)
;;                  (print-gray "] is ")
;;                  (print "false")
;;                  (println-gray "."))
;;                (recur (rest clauses) (inc index)))

;;              (true? status)
;;              (do
;;                (when (:info @debug)
;;                  (print-gray "    INFO: ")
;;                  (print (output fact new-pool))
;;                  (print-gray "[")
;;                  (print index)
;;                  (print-gray "] is ")
;;                  (print "true")
;;                  (println-gray ".")
;;                  (println-gray "    INFO: Pushing a Choisepoint:")
;;                  (print-gray "          [query:] ")
;;                  (println (output query pool))
;;                  (print-gray "          [index:] ")
;;                  (println (inc index)))
;;                (when (:trace @debug)
;;                  (print-green "  Exit: ")
;;                  (print (str "(" depth ")") (output fact new-pool) "? ")
;;                  (flush)
;;                  (read-line))
;;              [true new-pool new-stack])

;;              :else
;;              (do
;;                (when (:info @debug)
;;                  (print-gray "    INFO: ")
;;                  (print (output fact new-pool))
;;                  (print-gray "[")
;;                  (print index)
;;                  (print-gray "] is ")
;;                  (print "true")
;;                  (println-gray ".")
;;                  (println-gray "    INFO: Pushing a Choisepoint:")
;;                  (print-gray "          [query:] ")
;;                  (println (output query pool))
;;                  (print-gray "          [index:] ")
;;                  (println (inc index)))
;;                (let [[re-query re-pool] (replace query new-term new-pool)]
;;                  (prove new-term re-query re-pool new-stack depth 0)))))))))))


(defmethod replace logic.term.PrologFact
  [fact term pool]
  (if (true? term)
    [true pool]
    (reshape term pool)))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Interpreting a Prolog Rule.                                                    # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;


(defmethod prove logic.term.PrologRule
  [rule query pool stack depth start]
  (when (:info @debug)
    (println-gray "    INFO: Proving Rule."))
  (let [[answer new-pool new-stack]
        (prove (:body rule) query pool stack (inc depth) start)]
    (if (false? answer)
      (do
        (when (:trace @debug) (trace-fail (:head rule) pool depth))
        [false {} new-stack])
      (do
        (when (:trace @debug) (trace-exit (:head rule) new-pool depth))
        [true new-pool new-stack]))))


(defmethod replace logic.term.PrologRule
  [rule term pool]
  (let [[new-term new-pool] (replace (:body rule) term pool)]
    (if (true? new-term)
      [true new-pool]
      [(->PrologRule (:head rule) new-term)
       new-pool])))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Interpreting a Prolog Conjunction.                                             # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;


(defmethod prove logic.term.PrologConjunction
  [conj in-query in-pool in-stack depth start]
  (loop [goals (:terms conj)
         query in-query
         pool in-pool
         stack in-stack
         index start]

    (if (empty? goals)
      [true pool stack]
      (let [[status new-pool new-stack]
            (prove (first goals) query pool stack depth index)]
        (if (false? status)
          [false {} new-stack]
          (let [[new-query final-pool] (replace query true new-pool)]
            (recur (rest goals)
                   new-query
                   final-pool
                   new-stack
                   0)))))))


(defmethod replace logic.term.PrologConjunction
  [conj term pool]
  (let [elem (-> conj :terms first)
        [new-term new-pool] (replace elem term pool)]
    (if (true? new-term)
      (if (-> conj :terms next)
        (reshape (-> conj :terms (subvec 1) ->PrologConjunction)
                 new-pool)
        [new-term new-pool])
      (reshape (-> conj :terms (assoc 0 new-term) ->PrologConjunction)
               new-pool))))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Interpreting a Prolog Disjunction.                                             # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;



(defmethod prove logic.term.PrologDisjunction
  [disj query pool stack depth start]
  (let [goal (-> disj :terms first)
        rest-terms (-> disj :terms (subvec 1))
        [new-query new-pool] (replace query goal pool)

        [check-query check-pool]
        (if (empty? rest-terms)
          [query pool]
          (replace query (->PrologDisjunction rest-terms) pool))

        new-stack (if (empty? rest-terms)
                    stack
                    (conj stack [check-query check-pool 0]))]
    (prove goal new-query new-pool new-stack depth 0)))


(defmethod replace logic.term.PrologDisjunction
  [disj term pool]
  [term pool])


; =================================================================================== ;
; =================================================================================== ;


(defn interpret
  "Given a term, pool, stack, depth and start index,
   the function proves the term and automaticly backtracks
   if it is necassary."
  [term in-pool in-stack depth start]
  (loop [query term
         pool in-pool
         stack in-stack
         index start]
    (let [[answer new-pool new-stack]
          (prove query query pool stack depth index)]
      (if (false? answer)
        (if (empty? new-stack)
          [false {} []]
          (let [[old-query old-pool old-index] (peek new-stack)]
            (swap! debug assoc :redo true)
            (recur old-query old-pool (pop new-stack) old-index)))
        [true new-pool new-stack]))))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Interpreting a Prolog Negation.                                                # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;



(defmethod prove logic.term.PrologNegation
  [neg query pool stack depth start]
  (let [goal (-> neg :term)
        [answer _ _] (interpret goal pool stack depth 0)]
    (if answer
      [false pool stack]
      [true {} stack])))


(defmethod replace logic.term.PrologNegation
  [neg term pool]
  [term pool])



(defn ?-
  "Proves the query and prints the answer. If there are more than one
  solutions, it waits for force backtracking."
  [input]
  (let [main-pool (get-vars input)]
    (loop [query input
           pool {}
           stack []
           index 0]
      (let [[answer new-pool new-stack] (interpret query pool stack 1 index)]
        (if (false? answer)
          (println-red "false.\n")
          (do
            (print-answer main-pool new-pool)
            (flush)
            (if (empty? new-stack)
              (println ".\n")
              (do
                (print " ")
                (flush)
                (when (= ";" (read-line))
                  (let [[old-query old-pool old-index] (peek new-stack)]
                    (swap! debug assoc :redo true)
                  (recur old-query old-pool (pop new-stack) old-index))))))))))
  (swap! debug assoc :redo false))
