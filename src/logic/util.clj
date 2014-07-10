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


(defn print-err
  "Prints the error message in a specific text format"
  [err-msg]
  (println (str "\u001b[31m" err-msg "\u001b[0m")))


(defn print-blue
  "Prints a string with a blue color."
  [s]
  (print (str "\u001b[36m" s "\u001b[0m")))

(defn println-blue [s]
  (print-blue s)
  (println))
