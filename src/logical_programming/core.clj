(use '[clojure.core.match :only (match)])

(ns logical-programming.core
  (:gen-class))

(def stack (atom []))

(defn resolute [[clause & query]]
  (if (false? clause)
    (if (empty? @stack)
	  false
	  (swap! stack ))
	(if (empty? query)
	  true
      (resolute (vec query)))))

(defmacro ?- [clause & query]
  `(resolute [~clause ~@query]))

(defn -main
  "Entry Point."
  [& args]
  (println (?- true)))	
