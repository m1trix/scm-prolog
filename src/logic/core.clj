(use '[clojure.core.match :only (match)])

(ns logical_programming.core
  (:use [logical_programming.terms]))

(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")

  (println (-->structure :member [:A :X [:A :| :X]]))

  (let [input (atom "")]
    (while (different? @input "exit")
      (do
        (print "?- \u001b[33m")
        (flush)
        (reset! input (read-line))
        (print-err "Some error"))))
  (println "\u001b[33mHope to see you again soon! \u001b[0m \n"))

(-->structure :int [0])
