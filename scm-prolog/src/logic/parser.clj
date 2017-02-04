;; ===========================================================
;;  The Parser is the link between the user input and the
;;  Interpreter. The Parser reads a string from the user,
;;  'understands' it't meaning and creates the Prolog Terms
;;  that it describes.
;;
(ns logic.parser
  (:use [logic.term]
        [logic.operator])
  (:refer-clojure :exclude [resolve]))


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


(defn find-string
  "If a string starts with a inline string
  the function returns that string and the rest of the input.
  Otherwise it returns empty string and the same input."
  [input]
  (if (= \" (first input))
    (loop [s ""
           res (subs input 1)]
      (if (= \" (first res))
        [(str \" s \")
         (subs res 1)]
        (recur (str s (first res))
               (subs res 1))))
    ["" input]))


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
  "If a string starts with a number, the function
  returns a string of that number and the rest of the input.
  Otherwise it returns an empty string and the same input."
  [input]
  (loop [num ""
         text input
         point true]
    (let [sym (first text)]
      (cond

       (re-matches (re-pattern #"[0-9]") (str sym))
       (recur (str num sym)
              (subs text 1)
              point)

       (= \. sym)
       (if (and (true? point)
                (next text)
                (re-matches (re-pattern #"[0-9]") (subs text 1 2)))
         (recur (str num sym) (subs text 1) false)
         [num text])

       :else
       [num text]))))


(defn find-next
  "The function finds the next Prolog Term (if there is one)
  in a stirng and returns ['<term>', <rest_of_string>] vector.
  If there is no Term, [\"\", <input_string>]."
  [input]
  (let [[atom res] (find-atom input)]
    (if-not (= "" atom) [atom res]

      (let [[var res] (find-variable input)]
        (if-not (= "" var) [var res]

          (let [[num res] (find-number input)]
            (if-not (= "" num) [(read-string num) res]

              (let [[s res] (find-string input)]
                (if-not (= "" s) [s res]
                  (throw (Exception. (str "Missing operator before: \"" (-> input (subs 1) find-next first) "\"."))))))))))))



(defn extract-atom
  "Creates a PrologAtom from a string.
  Returns: [<atom>, <rest_of_string>].
  Returns: [nil, <input_string>], if there is no Atom."
  [input]
  (let [[atom result] (find-atom input)]
    (if (= "" atom)
      [nil input]
      [(create atom) result])))


(defn extract-variable
  "Creates a PrologVariable from a string.
  Returns: [<variable>, <rest_of_string>]
  Returns: [nil, <input_string>], if there is no Variable."
  [input]
  (let [[var result] (find-variable input)]
    (if (= "" var)
      [nil input]
      [(create var) result])))


(defn extract-list
  "Creates a vector of ProlotTerms from a string.
  Returns: [[<elements>], <rest_of_string>]
  Returns: [nil, <input_string>], if there is no List."
  [input]
  (if-not (= \[ (first input))
    [nil input]
    (if (= \] (second input))
      [[] (subs input 2)]
      (loop [elems []
             text (subs input 1)
             expecting-elem true]
        (cond

         (= \space (first text))
         (recur elems (subs text 1) expecting-elem)

         (= \, (first text))
         (if expecting-elem
           (if (empty? elems)
             (throw (Exception. "Missing element: \"(\" (here) \",\""))
             (throw (Exception. (str "Missing element after: \"" (last elems) ",\" (here) ."))))
           (recur elems (subs text 1) true))

         (= \[ (first text))
         (if (false? expecting-elem)
           (throw (Exception. (str "Missing ',' after: \"" (last elems) "\" (here) .")))
           (let [[new-list new-text] (extract-list text)]
             (recur (conj elems new-list) new-text false)))

         (= \| (first text))
         (if expecting-elem
           (throw (Exception. "Missing element before '|' ."))
           (recur (conj elems "|")
                  (subs text 1)
                  true))

         (= \] (first text))
         (if expecting-elem
           (throw (Exception. "Missing element before ']'."))
           [elems (subs text 1)])

         :else
         (if (false? expecting-elem)
           (throw (Exception. (str "Missing ',': \"" (last elems) "\" (here) .")))
           (let [[term new-text] (find-next text)]
             (recur (conj elems term) new-text false))))))))


(defn extract-arguments
  "Creates a ProlotArguments object from a string.
  Returns: [<arguments>, <rest_of_string>]
  Returns: [nil, <input_string>], if there are no Arguments."
  [input]
  (if-not (= \( (first input))
    [nil input]
    (loop [args []
           text (subs input 1)
           check-arg true]
      (cond

       (= \space (first text))
       (recur args (subs text 1) check-arg)

       (= \, (first text))
       (recur args (subs text 1) true)

       (= \) (first text))
       (if check-arg
         (throw (Exception. (str "Missing argument: \"" (last args) ", \" (here) .")))
         [(create-arguments args) (subs text 1)])

       (= \[ (first text))
       (if (false? check-arg)
         (throw (Exception. (str "Missing ',': \"" (last args) "\" (here) \"" text "\"." )))
         (let [[list new-text] (extract-list text)]
           (recur (conj args list) new-text false)))

       :else
       (if (false? check-arg)
         (throw (Exception. (str "Missing ',': \"" (last args) "\" (here) \"" text "\"." )))
         (let [[term new-text] (find-next text)]
           (recur (conj args term) new-text false)))))))


(defn extract-next [text]
  "Recognizes and extracts the first Prolog Term from a stirng.
  Returns: [<term>, <rest_of_string>]
  Returns: [nil, <input_string>], if no Term is recognized."
  (let [[atom atom-text :as result] (extract-atom text)]
    (if atom
      (let [[args fact-text] (extract-arguments atom-text)]
        (if args
          [(->PrologFact atom args) fact-text]
          result))

      (let [[parsed-list text-after-list] (extract-list text)]
        (if (= nil parsed-list)
          (let [[term term-text] (find-next text)]
            [(create term) term-text])
          [(create parsed-list) text-after-list])))))



(defn find-word-operator
  "Finds and extracts the name of an Operator,
  made by a single word.
  Returns: [<name> <rest_of_text>].
  Returns: [\"\" <input_string>] if the name is
  not recognized as a Prolog Operator."
  [input]
  (loop [name ""
         text input]
    (cond


     (empty? text)
     [name ""]

     (re-matches (re-pattern "[a-zA-Z0-9_]")
                 (subs text 0 1))
     (recur (str name (first text))
             (subs text 1))

     :else
     [name text])))


(defn find-symbol-operator
  "Finds and extracts the name of an Operator,
  made by a group of symbols.
  Returns: [<name> <rest_of_text>].
  Returns: ['' <input_string>] if the name is
  not recognized as a Prolog Operator."
  [input]
  (loop [name ""
         text input]
    (cond


     (empty? text)
     [name ""]

     (re-matches (re-pattern "[a-zA-Z0-9_ \t()]")
                 (subs text 0 1))
     [name text]

     (= \space (first text))
     [name text]

     :else
     (recur (str name (first text))
            (subs text 1)))))



(defn find-operator [input]
  "Finds and extracts the name of an Operator
  contained in the begining of the stirng.
  Returns: [<name> <rest_of_text>].
  Returns: [\"\" <input_string>] if the name is
  not recognized as a Prolog Operator."
  (let [[name rest-text]
        (if (re-matches (re-pattern "[a-zA-Z0-9_]")
                        (subs input 0 1))
          (find-word-operator input)
          (find-symbol-operator input))]
    (if (and (not (= "" name))
             (prolog-operator? name))
      [name rest-text]
      ["" input])))


(defn execute [op obs]
  "Executes an operation by using Terms
  from a stack as operands.
  The result is pushed on the top of the stack."
  (let [index (- (count obs) (:arity op))
        terms (subvec obs index)
        rest-obs (subvec obs 0 index)]

;;     DEBUG:
;;       (println "Executing:" (-> op :op :name))

    (cond

     (= "," (-> op :op :name))
     (conj rest-obs (->PrologConjunction terms))

     (= ";" (-> op :op :name))
     (conj rest-obs (->PrologDisjunction terms))

     (= "not" (-> op :op :name))
     (conj rest-obs (->PrologNegation (first terms)))

     (= ":-" (-> op :op :name))
     (conj rest-obs (->PrologRule (first terms)
                                  (second terms)))

     :else (conj rest-obs (make-fact (:op op) terms)))))


(defn execute-all [in-ops in-obs]
  "Executes all operations from a given stack by
  using objects from another stack as operands.
  The result of each operation is pushed on the
  top of the second stack."
  (loop [ops in-ops
         obs in-obs]
    (if (empty? ops)
      (do
;;         DEBUG:
;;           (println "All operations executed.")
        [[] obs])
      (recur (pop ops)
             (execute (peek ops) obs)))))


(defn operators-clash
  "Throws an Exception, containg a message about
  two Prolog Operators which precendeces clash."
  [op-left op-right]
  (throw
   (Exception.
    (str "Operators clash: \""
         (:name op-left)
         "\" and \""
         (:name op-right)
         "\"."))))


(defn add-operation
  "Adds an Operator to a stack of Operators
  by using the Shunting-Yard algorithm to pop
  and execute previous Operations from the stack."
  [operations objects new-op]

;;   DEBUG:
;;     (println "Adding operation:" (:name new-op))

  (loop [ops operations
         obs objects]
    (if (or (empty? ops) (= "(" (peek ops)))
      [(conj ops {:op new-op :arity (operator-arity new-op)}) obs]
      (let [top (peek ops)]

        (cond

         (and (= (:name new-op) ",")
              (= (-> top :op :name) ","))
         [(conj (pop ops) (assoc top :arity (-> top :arity inc)))
          obs]

         (< (-> top :op :prec) (:prec new-op))
         (recur (pop ops) (execute top obs))

         (= (-> top :op :prec) (:prec new-op))
         (cond


          (or (and (= (-> top :op :right) :same) (= (:left new-op) :less))
              (and (= (-> top :op :right) :same) (= (:left new-op) :none)))
          [(conj ops {:op new-op :arity (operator-arity new-op)}) obs]


          (or (and (= (-> top :op :right) :less) (= (:left new-op) :same))
              (and (= (-> top :op :right) :none) (= (:left new-op) :same)))
          (recur (pop ops) (execute top obs))


          :else
          (operators-clash (:op top) new-op))


         :else
         [(conj ops {:op new-op :arity (operator-arity new-op)}) obs])))))


(defn parse
  "Parses a string and returns a vector
  of Prolog Terms recognized in the string."
  [input]
  (loop [text input
         ops []
         obs []
         result []
         expecting-unary true]

;;   DEBUG:
;;     (print ops " ")
;;     (print "[ ")
;;     (doseq [x obs] (print (output x {}) ""))
;;     (println "]")
;;     (println "Text:" text)

    (cond

     (or (empty? text)
         (= \% (first text)))
     (if (empty? obs)
       (do
;;          (doseq [x result] (println (output x {})))
         result)
       (throw (Exception. "Missing '.'")))

     (= \space (first text))
     (recur(subs text 1) ops obs result expecting-unary)

     (= \. (first text))
     (let [[new-ops new-obs] (execute-all ops obs)]
       (if (or (not (empty? new-ops))
               (next new-obs))
         (throw (Exception. (str "Missing operator before " (output (peek new-obs) {}) ".")))
         (recur (subs text 1) [] [] (conj result (peek new-obs)) true)))

     (= \( (first text))
     (recur (subs text 1) (conj ops "(") obs result true)

     (= \) (first text))
     (if (= "(" (peek ops))
       (throw (Exception. str ("Missing operator before: \"" text "\".")))
       (let [[new-ops new-obs]
             (loop [new-ops ops
                    new-obs obs]
               (cond

                (empty? new-ops)
                (throw (Exception. (str "Missing '(' before: \"" text "\".")))

                (= "(" (peek new-ops))
                [new-ops new-obs]

                :else
                (recur (pop new-ops) (execute (peek new-ops) new-obs))))]
         (recur (subs text 1) (pop new-ops) new-obs result false)))

     :else
     (let [[name rest-text] (find-operator text)]
       (if (= "" name)
         (let [[term rest-text] (extract-next text)]
           (recur rest-text ops (conj obs term) result false))
         (let [operator (if expecting-unary
                          (get-unary name)
                          (get-binary name))
               [new-ops new-obs] (add-operation ops obs operator)]
           (recur rest-text new-ops new-obs result true)))))))