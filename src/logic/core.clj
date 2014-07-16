(ns logic.core
  (:use [logic.util]
        [logic.term]
        [logic.interpreter]))




(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")


  (let [query (>conjunct< [:&
                   [:concat [[1 2] [3 4] :Z]]
                   [:perm [[1 2 3 4 5 6 7] :H]]
                   [:concat [:Z :Y :H]]])]
    (println "?- " (output-term query))
    (?- query))

  (let [query (>conjunct< [:&
                           [:perm [[1 2.3 1 2] :P]]
                           [:concat [:A :Gosho :P]]
                           [:member [:X :A]]
                           [:member [:X :Gosho]]])]
    (println "?- " (output-term query))
    (?- query))

  (let [query (>disjunct< [:$
                           [:member [:L [1 2]]]
                           [:member [:L [3 4]]]])]
    (println "?- " (output-term query))
    (?- query))

  (let [query (->PrologConjunct [(>disjunct< [:$
                                              [:member [:L [1 2]]]
                                              [:member [:L [3 4]]]])
                                 (>structure< :member [:L [2 3]])])]
    (println "?- " (output-term query))
    (?- query))


  (let [input (atom "")]
    (while (different? @input "halt.")
      (do
        (print "?- \u001b[33m")
        (flush)
        (reset! input (read-line))
        (print "\u001b[0m"))))
  (println "\u001b[33mHope to see you again soon! \u001b[0m \n"))
