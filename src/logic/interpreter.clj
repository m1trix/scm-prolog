(ns logic.interpreter
  (:use [logic.util]
        [logic.term]))

;;
;;  This is the source of all knowledge.
;;  Whatever gets loaded to the program goes here.
;;  The interpreter knows only the things that are included in the knowledge-base.
;;
(def knowledge-base (atom { :member [(-->functor :member [:A [:A :| :_]] [])
                                     (-->functor :member [:A [:_ :| :X]] [[:member [:A :X]]])
                                     ]}))


(defn match
  "Returns a list of all matches of the goal with clauses from the knowledge-base."
  [goal pool]
  (let [all ((:name goal) @knowledge-base)]
    (if (nil? all)
      []
      (loop [result []
             others all]
        (if (empty? others)
          result
          (let [elem (first others)
                [new-functor new-pool] (match-to-functor goal elem pool)]
            (if (false? new-functor)
              (recur result
                     (rest others))
              (recur (conj result [new-functor new-pool])
                     (rest others)))))))))


(defn interpret [input-query]
  ;; >>> STEP 1 <<<
  (let [query (atom input-query)
        stack (atom [])
        state (atom :running)]
    ;; >>> STEP 2 <<<
    (while (not-empty @query)
      (let [goal (first @query)
            clauses (atom (match goal {}))]
        ;; >>> STEP 3 <<<
        (while (and (empty? @clauses)
                    (same? :running @state))
          ;; >>> STEP 4 <<<
          (if (empty? @stack)
            (reset! state :fail)
            (let [[old-query old-clauses] (peek @stack)]
              (swap! stack pop)
              (reset! query old-query)
              (reset! clauses old-clauses))))
        (if (same? :fail @state)
          (do
            (print-err "false.\n")
            (reset! query []))
          ;; >>> STEP 5 <<<
          (let [top (first @clauses)]
            (swap! stack conj [@query (rest @clauses)])
            ;; >>> STEP 6 <<<
            (let [body (:body top)]
              (if (empty? body)
                (swap! query rest)
                (do
                  (swap! query assoc 0 body)
                  (swap! query flatten))))

            (if (empty? @query)
              (do
                (print "true. ")
                (flush)
                (if (not-empty @stack)
                  (let [sym (read-line)]
                    (if (same? sym ";")
                      (let [[old-query old-clauses] (peek @stack)]
                        (swap! stack pop)
                        (reset! query old-query)
                        (reset! clauses old-clauses)))))))))))))


(defn ?-
  "A function that is used by the UI to interpret user queries."
  [query]
  (if (empty? query)
    (print-err "Empty query!")
    (interpret query)))

