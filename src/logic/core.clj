(ns logic.core
  (:use [logic.util]
        [logic.term]
        [logic.interpreter]))




(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")


  (let [fact (create [:fact "member" ["L" [1 2 3]]])]
    (?- fact))


  (let [input (atom "")]
    (while (different? @input "halt.")
      (do
        (when @trace
          (print "[trace] "))
        (when (or @watch-stack @watch-pool @watch-resolve)
          (print "[debug] "))
        (print " ?- \u001b[33m")
        (flush)
        (reset! input (read-line))
        (print "\u001b[0m"))))
  (println "\u001b[33mHope to see you again soon! \u001b[0m \n"))
