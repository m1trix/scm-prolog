(ns logic.core
  (:use [logic.interpreter]
        [logic.parser]
        [logic.util])
  (:refer-clojure :exclude [resolve replace]))




(defn -main
  "Entry Point."
  [& args]
  (println-color "Wellcome to SCM-Prolog! Have fun :)\n" color-yellow)

  (loop []
    (print "?- ")
    (flush)
    (let [input (read-line)]
      (try
        (let [terms (parse input)]
          (doseq [term terms]
            (interactive-prove term)))
        (catch Exception e
          (println-color (str "ERROR: " (.getMessage e))
                         color-red)))
      (recur)))

  (println-color "Hope to see you again soon!\n" color-yellow))
