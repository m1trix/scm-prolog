;; ===========================================================
;;  The Parser is the link between the user input and the
;;  Interpreter. The Parser reads a string from the user,
;;  'understands' it't meaning and creates the Prolog Terms
;;  that it describes.
;;
(ns logic.parser
  [:use [logic.term]])


(defn find-atom-single
  "If a string starts with a single word atom name
  the function returns the name and the rest of the string.
  Otherwise it returns empty name and the input string."
  [input]
  (if (re-matches (re-pattern #"[a-z]") (subs input 0 1))
    (loop [name (-> input first str)
           result (rest input)]
      (if (re-matches (re-pattern #"[a-zA-Z0-9_]") (-> result first str))
        (recur (str name (first result))
               (rest result))
        [name (reduce str result)]))
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
    (loop [name (-> input first str)
           result (rest input)]
      (if (re-matches (re-pattern #"[a-zA-Z0-9_]") (-> result first str))
        (recur (str name (first result))
               (rest result))
        [name (reduce str result)]))

    ["" input]))


(defn find-number
  "If a string starts with a number the function
  returns a string of that number and the rest of the input.
  Otherwise it returns an empty string and the same input."
  [input]
  (loop [num ""
        res input]
    (let [sym (first res)]
    (if (or (digit? sym)
            (= \. sym))
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


(defn find-rule
  [input]
  (let [[atom next-input] (find-atom input)]
    (if (= "" atom)
      ["" input]
      (let [[args result] (find-arguments next-input)]
        (if (= "" args)
          ["" input]
          [(str atom args) result])))))



(defn find-next
  [input]
  (let [[atom res] (find-atom input)]
    (if atom [atom res])))



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
           result (subs input 1)]
      (cond
       (empty? result)
       (throw (Exception. "Illegal Arguments list!"))

       (= \) (first result))
       [(create args) (subs result 1)]

       :else
       (let [[term new-result] (find-next result)]
          (recur (conj args term) new-result))))))


(defn parse-next
  [input]
  (let [[atom res] (extract-atom input)]
    (if atom [atom res]

      (let [[var res] (extract-variable input)]
        (if var [var res]

          (let [[num res] (extract-number input)]
            (if num [num res])

            (let [[args res] (extract-arguments input)]
              (if args [args res]))))))))



(parse-next "(pesho)")
