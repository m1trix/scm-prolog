(ns logic.core
  (:use [logic.util]
        [logic.term]
        [logic.interpreter]))




(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")
  (let [[name pool] (unify-variables
                     (>variable< :X)
                     (>variable< :Y)
                     {})]
    (print-blue(output-term name)))

  (let [input (atom "")]
    (while (different? @input "exit")
      (do
        (print "?- \u001b[33m")
        (flush)
        (reset! input (read-line)))))
  (println "\u001b[33mHope to see you again soon! \u001b[0m \n"))
