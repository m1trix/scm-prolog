(use '[clojure.core.match :only (match)])

(ns logical-programming.core
  (:use [logical-programming.util]
        [logical-programming.parser]
        [logical-programming.interpreter]))

(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")
  (?- [{:name :member :args [:X :Y]}])
  (let [input (atom "")]
    (while (different? @input "exit")
      (do
        (print "?- \u001b[33m")
        (flush)
        (reset! input (read-line))
        (print-err "Some error"))))
  (println "\u001b[33mHope to see you again soon! \u001b[0m \n"))
