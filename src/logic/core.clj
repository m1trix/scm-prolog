(ns logic.core
  (:use [logic.interpreter]
        [logic.parser]
        [logic.util])
  (:refer-clojure :exclude [resolve replace]))




(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")

  (loop []
    (when (:trace @debug)
      (print "[T] " ))
    (print "?- ")
    ;;(print "\u001b[33m")
    (flush)
    (let [input (read-line)]
     ;; (print "\u001b[0m")
      (try
        (let [terms (parse input)]
          (doseq [term terms]
            (?- term)))
        (catch Exception e
          (println-red (str "ERROR: "(.getMessage e)))))
      (when (false? (:exit @debug))
        (recur))))

  (println "\u001b[33mHope to see you again soon! \u001b[0m \n"))
