;; ============================================
;;  This file contains some utility functions.
;; ============================================

(ns logic.util)


(defn different? [X Y]
  (not (= X Y)))


(defn same? [X Y]
  (= X Y))


(defn make-map
  "Makes a map from a given vector of keys by putting the same value to all ot them."
  [elems value]
  (reduce #(assoc %1 %2 value) {} elems))


(defn arity
  "Tells the arity of a PL-Struct, even though it uses it as a map."
  [A]
  (count (:args A)))


(defn keyword->string [key]
  (subs (str key) 1))


(defn a-z?
  "Tells if a keyword begins with a small letter."
  [key]
  (let [s (keyword->string key)]
    (cond
     (< 0 (compare "a" s)) false
     (< 0 (compare s "z")) false
     :else true)))


(defn A-Z?
  "Tells if a keyword begins with a small letter."
  [key]
  (let [s (keyword->string key)]
    (cond
     (< 0 (compare "A" s)) false
     (< 0 (compare s "Z")) false
     :else true)))


(defn extract-keys
  [m v]
  (->>
   (map #(vector % v) (keys m))
   (flatten)
   (apply hash-map)))

(defn extract-vals
  [m v]
  (->>
   (map #(vector % v) (vals m))
   (flatten)
   (apply hash-map)))


(defn print-red
  "Prints the error message in a specific text format"
  [err-msg]
  (print (str "\u001b[1;31m" err-msg "\u001b[0m")))


(defn print-blue
  "Prints a string with a blue color."
  [s]
  (print (str "\u001b[1;34m" s "\u001b[0m")))


(defn print-green
  "Prints a string with a green color."
  [s]
  (print (str "\u001b[1;32m" s "\u001b[0m")))


(defn print-yellow
  "Prints a string with a yellow color."
  [s]
  (print (str "\u001b[1;33m" s "\u001b[0m")))


(defn print-gray
  "Prints a string with a gray color."
  [s]
  (print (str "\u001b[38;5;247m" s "\u001b[0m")))



(defn println-blue [s]
  (print-blue s)
  (println))

(defn println-green [s]
  (print-green s)
  (println))

(defn println-yellow [s]
  (print-yellow s)
  (println))

(defn println-red [s]
  (print-red s)
  (println))

(defn println-gray [s]
  (print-gray s)
  (println))
