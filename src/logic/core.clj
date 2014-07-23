(ns logic.core
  (:use [logic.util]
        [logic.term]
        [logic.interpreter]))




(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")


  (let [query (create [:fact "member" ["L" [1 2 3]]])]
    (println (output query))
    (?- query))


  (let [input (atom "")]
    (while (different? @input "halt.")
      (do
        (print "?- \u001b[33m")
        (flush)
        (reset! input (read-line))
        (print "\u001b[0m"))))
  (println "\u001b[33mHope to see you again soon! \u001b[0m \n"))
