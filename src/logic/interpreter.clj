(ns logic.interpreter
  (:use [logic.util]
        [logic.term]
        [clojure.java.io]
        [logic.parser])
  (:refer-clojure :exclude [resolve replace]))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  This is the source of all knowledge.                                           # ;
; #  Whatever gets loaded to the program goes here.                                 # ;
; #  The interpreter knows only the things that are included in the knowledge-base. # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(def debug (atom {:exit false
                  :trace false
                  :info false
                  :redo false
                  :watch {:query false
                          :stack false}}))


(def knowledge-base
  (atom {"trace" (create [:form "trace" [] (fn [_] (swap! debug assoc :trace true))])
         "notrace" (create [:form "notrace" [] (fn [_] (swap! debug assoc :trace false))])
         "halt" (create [:form "halt" [] (fn [_] (swap! debug assoc :exit true))])}))

(def all-formulas {})


(defn compile-file [[s]]
  (with-open [rdr (reader (clojure.string/replace (:string s) #"\"" ""))]
    (doseq [line (line-seq rdr)]
      (let [terms (parse line)]
        (doseq [term terms]
          (let [name (get-name term)
                all (@knowledge-base name)]
            (cond

             (nil? all)
             (swap! knowledge-base assoc name [term])

             :else
             (swap! knowledge-base assoc name (conj all term)))))))))


(swap! knowledge-base assoc "compile" [(create [:form "compile" ["Filepath"] compile-file])])


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
; #  Interpreting a Prolog Atom.                                                    # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;


(defmethod prove logic.term.PrologAtom
  [atom query pool stack _ _]
  (let [target (@knowledge-base (:name atom))]
    (if (nil? target)
      [false {} stack]
      (let [[answer _ _] (resolve atom target pool)]
        (if (true? answer)
          [true pool stack]
          [false {} stack])))))


(defmethod replace logic.term.PrologAtom
  [atom term pool]
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
        all (@knowledge-base name)
        limit (count all)]
    (when (:info @debug)
      (print-gray "    INFO: Proving ")
      (print (output fact pool))
      (println-gray "."))
    (when (-> @debug :watch :query)
      (println-gray (str "    GOAL:  " (output fact pool)))
      (println-gray (str "    QUERY: " (output query pool))))
    (when (-> @debug :watch :stack)
      (output-stack stack))
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
                 (print-gray "    INFO: ")
                 (print (output fact pool))
                 (print-gray "[")
                 (print index)
                 (print-gray "] is ")
                 (print "false")
                 (println-gray "."))
               (recur (rest clauses) (inc index)))

             (true? status)
             (do
               (when (:info @debug)
                 (print-gray "    INFO: ")
                 (print (output fact new-pool))
                 (print-gray "[")
                 (print index)
                 (print-gray "] is ")
                 (print "true")
                 (println-gray ".")
                 (println-gray "    INFO: Pushing a Choisepoint:")
                 (print-gray "          [query:] ")
                 (println (output query pool))
                 (print-gray "          [index:] ")
                 (println (inc index)))
               (when (:trace @debug)
                 (print-green "  Exit: ")
                 (print (str "(" depth ")") (output fact new-pool) "? ")
                 (flush)
                 (read-line))
             [true new-pool new-stack])

             :else
             (do
               (when (:info @debug)
                 (print-gray "    INFO: ")
                 (print (output fact new-pool))
                 (print-gray "[")
                 (print index)
                 (print-gray "] is ")
                 (print "true")
                 (println-gray ".")
                 (println-gray "    INFO: Pushing a Choisepoint:")
                 (print-gray "          [query:] ")
                 (println (output query pool))
                 (print-gray "          [index:] ")
                 (println (inc index)))
               (let [[re-query re-pool] (replace query new-term new-pool)]
                 (prove new-term re-query re-pool new-stack depth 0)))))))))))


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
        (when (:info @debug)
          (print-gray "    INFO: Rule is ")
          (print "false")
          (println-gray "."))
        (when (:trace @debug)
          (when-not (:redo @debug)
            (print-red "  Fail: ")
            (print (str "(" depth ")") (output (:head rule) pool) "? ")
            (flush)
            (read-line)))
        [false {} new-stack])
      (do
        (when (:info @debug)
          (print-gray "    INFO: Rule is ")
          (print "true")
          (println-gray "."))
        (when (:trace @debug)
          (print-green "  Exit: ")
          (print (str "(" depth ")") (output (:head rule) new-pool) "? ")
          (flush)
          (read-line))
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
    (when (:info @debug)
          (print-gray "    INFO: Proving Conjunction[")
          (print (count goals))
          (println-gray "]."))
    (if (empty? goals)
      (do
        (when (:info @debug)
          (print-gray "    INFO: Conjunction is")
          (print "true")
          (println-gray "."))
        [true pool stack])
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


(defn ?-
  "Proves the query and prints the answer. If there are more than one
  solutions, it waits for forse backtracking."
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
