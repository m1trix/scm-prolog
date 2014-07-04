;; ============================================
;;  This file contains some utility functions.
;; ============================================

(ns logic.util)


(defn different? [X Y]
  (not (= X Y)))


(defn same? [X Y]
  (= X Y))


(defn arity
  "Tells the arity of a PL-Struct, even though it uses it as a map."
  [A]
  (count (:args A)))


(defn a-z?
  "Tells if a keyword begins with a small letter."
  [key]
  (let [s (str (second (str key)))]
    (if (< 0 (compare "a" s))
      false
      (if(< 0 (compare s "z"))
        false
        true))))


(defn A-Z? [key]
  "Tells if a keyword begins with a caital letter."
  (let [s (str (second (str key)))]
    (if (< 0 (compare "A" s))
      false
      (if(< 0 (compare s "Z"))
        false
        true))))

(defn print-err
  "Prints the error message in a specific text format"
  [err-msg]
  (println (str "\u001b[31m" err-msg "\u001b[0m")))
