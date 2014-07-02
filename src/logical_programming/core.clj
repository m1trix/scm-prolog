(use '[clojure.core.match :only (match)])

(ns logical-programming.core
  (:gen-class))


(defn -main
  "Entry Point."
  [& args]
  (println "SCM-Prolog started.")
  (let [input (atom "")]
    (while (not (= @input "exit"))
      (do
        (print "?- ")
        (reset! input (read-line))
        ))
    (println "\nHope to see you again soon!\n")))
