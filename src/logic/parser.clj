;; ===========================================================
;;  The Parser is the link between the user input and the
;;  Interpreter. The Parser reads a string from the user,
;;  'understands' it't meaning and creates the Prolog Terms
;;  that it describes.
;;
(ns logic.parser
  [:require [logic.term :refer :all]])


(defn find-atom-single
  "If a string starts with a single word atom name
  the function returns the name and the rest of the string.
  Otherwise it returns empty name and the input string."
  [input]
  (if (re-matches (re-pattern #"[a-z]") (subs input 0 1))
    (loop [name (subs input 0 1)
           text (subs input 1)]
      (if (re-matches (re-pattern #"[a-zA-Z0-9_]") (subs text 0 1))
        (recur (str name (first text))
               (subs text 1))
        [name text]))
    ["" input]))


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


(defn find-variable
  "If a string starts with a single word Variable name
  the function returns the name and the rest of the string.
  Otherwise it returns empty name and the input string."
  [input]
  (if (re-matches (re-pattern #"[A-Z_]") (subs input 0 1))
    (loop [name (subs input 0 1)
           text (subs input 1)]
      (if (re-matches (re-pattern #"[a-zA-Z0-9_]") (subs text 0 1))
        (recur (str name (first text))
               (subs text 1))
        [name text]))

    ["" input]))


(defn find-number
  "If a string starts with a number the function
  returns a string of that number and the rest of the input.
  Otherwise it returns an empty string and the same input."
  [input]
  (loop [num ""
        res input]
    (let [sym (first res)]
    (if (re-matches (re-pattern #"[0-9.]") (str sym))
      (recur (str num sym)
             (subs res 1))
      [num res]))))


(defn find-list
  [input]
  (if-not (= \[ (first input))
    ["" input]
    (loop [str-list "["
           res (subs input 1)]
      (let [sym (first res)]
        (cond

         (= \[ sym)
         (let [[new-list new-input] (find-list res)]
           (recur (str str-list new-list)
                  new-input))

         (= \] sym)
         [(str str-list \]) (subs res 1)]

         (or (= \( sym)
             (= \( sym))
         (throw (Exception. "A List can hold only Atoms, Strings, Numbers and Variables."))

         :else
         (recur (str str-list sym)
                (subs res 1)))))))


(defn find-next
  [input]
  (let [[atom res] (find-atom input)]
    (if-not (= "" atom) [atom res])))



(defn extract-atom
  [input]
  (let [[atom result] (find-atom input)]
    (if (= "" atom)
      [nil input]
      [(create atom) result])))


(defn extract-variable
  [input]
  (let [[var result] (find-variable input)]
    (if (= "" var)
      [nil input]
      [(create var) result])))


(defn extract-number
  [input]
  (let [[num res] (find-number input)]
    (if (= "" num)
      [nil num]
      [(-> num read-string create) res])))


(defn extract-arguments
  [input]
  (if-not (= \( (first input))
    [nil input]
    (loop [args []
           text (subs input 1)]
      (cond

       (= \, (first text))
       (recur args (subs text 1))

       (= \) (first text))
       [(create-arguments args) (subs text 1)]

       :else
       (let [[term new-text] (find-next text)]
          (recur (conj args term) new-text))))))

(defn extract-next [text]
  (let [[atom rest-text :as result] (extract-atom text)]
    (if atom
      result)))


(defn parse [input]
  (loop [text input
         ops []
         obs []]
    (cond

     (= "." text)
     (peek obs)

     :else
     (let [[term rest-text] (extract-next text)]
       (if term
         (recur rest-text
                ops
                (conj obs term))
         (throw (Exception. "Invalid input!")))))))
