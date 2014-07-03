;; ============================================
;;  This file contains some utility functions.
;; ============================================

(ns logical-programming.util)


(defn different? [X Y]
  (not (= X Y)))

(defn same? [X Y]
  (= X Y))

(defn arity [A]
  (count (:args A)))


(defn a-z? [key]
  (let [s (->
           (str key)
           (vec)
           (second)
           (str))]
    (same? s (clojure.string/lower-case s))))

(defn print-err
  "Prints the error message in a specific text format"
  [err-msg]
  (println (str "\u001b[31m" err-msg "\u001b[0m")))
