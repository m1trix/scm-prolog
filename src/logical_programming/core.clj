(use '[clojure.core.match :only (match)])

(ns logical-programming.core
  (:gen-class))


(defn parse
  "Parsing the user input and executing it's meaning."
  [input]
  (let [trim-input (clojure.string/trim input)]
    ))


(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")
  (let [input (atom "")]
    (while (not (= @input "exit"))
      (do
        (print "?- \u001b[33m")
        (flush)
        (reset! input (read-line))
        (parse @input)
        (println "\u001b[0m")
        ))
    (println "\u001b[33mHope to see you again soon! \u001b[0m \n")))
