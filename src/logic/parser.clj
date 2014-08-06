;; ===========================================================
;;  The Parser is the link between the user input and the
;;  Interpreter. The Parser reads a string from the user,
;;  'understands' it't meaning and creates the PrologFunctors
;;  that it describes.
;;
(ns logic.parser
  [:use [logic.term]])


(defn small-letter?
  [sym]
  (and (>= 0 (compare \a sym))
       (>= 0 (compare sym \z))))


(defn capital-letter?
  [sym]
  (and (>= 0 (compare \A sym))
       (>= 0 (compare sym \Z))))


(defn find-atom-single
  "If a string starts with a single word atom name
  the function returns the name and the rest of the string.
  Otherwise it returns empty name and the input string."
  [input]
  (loop [name ""
         res input]
    (if (small-letter? (first res))
      (recur (str name (first res))
             (subs res 1))
      [name res])))


(defn find-atom-multi
  "If a string starts with a multi word atom name
  the function returns the name and the rest of the string.
  Otherwise it returns empty name and the input string."
  [input]
  (if (= \' (first input))
    (loop [name ""
           res (subs input 1)]
      (if (= \' (first res))
        [(str \' name \')
         (subs res 1)]
        (recur (str name (first res))
               (subs res 1))))
    ["" input]))


(defn find-atom
  "If a string starts with an atom name
  the function returns that name and the rest of the stirng.
  Otherwise it returns empty name and the input stirng."
  [input]
  (let [[single-word result] (find-atom-single input)]
    (if (= "" single-word)
      (find-atom-multi input)
      [single-word result])))
