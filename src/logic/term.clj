;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logic.term
  [:require [logic.util :refer :all]
            [clojure.set]])


(defprotocol TermInterface
  (create [input])
  (unify [x y pool])
  (output [term])
  (generate [term names])
  (get-variables [term]))



;; ============================================================================
;;  Prolog Variable: X. Whatever.
;;
;;  A Prolog Variable always starts with a capital letter.
;;  It does not have a value when it is created, but it can be evaluated to
;;  any Prolog Term. Once a value is given to a Variable, it cannot be changed.
;;
;;  The process of unifying two Variables is a process of binding them together.
;;  That means that whenever one of them gets evaluated, the other is also
;;  evaluated.
;;
(defrecord PrologVariable [name])


(defn create-variable [var]
  (if (re-matches (re-pattern #"[_A-Z][A-Za-z_]*") var)
    (PrologVariable. var)
    (throw (Exception. (str var " is invalid PrologVariable name!")))))


(defn prolog-variable? [var]
  (= (type var)
     logic.term.PrologVariable))


(defn generate-variable [var names]
  (if (= (:name var) "_")
    (let [new-name (str (gensym))]
      [(PrologVariable. new-name)
       (assoc
         names
         new-name
         new-name)])
    (if (nil? (get names (:name var)))

      (let [new-name (str (gensym))]
        [(PrologVariable. new-name)
         (assoc
           names
           (:name var)
           new-name)])

      [(PrologVariable. (get names
                             (:name var)))
       names])))


(defn get-variable-variables
  "A PrologVariable holds only itself, but random variables are not important."
  [var]
  (if (= (:name var) "_")
    #{}
    #{var}))


(defn output-variable [var]
  (:name var))


(defn evaluate-variable [var term pool]
  (let [name (:name var)
        val (get pool name)]
    (if (nil? val)
      [term (assoc pool name term)]
      (unify val term pool))))


(defn bind-variables
  [var-x var-y pool]
  (if (same? var-x var-y)
    [var-x pool]
    (let [name-x (:name var-x)
          name-y (:name var-y)
          val-x (get pool name-x)
          val-y (get pool name-y)]
      (if (nil? val-x)
        (if (nil? val-y)
          [var-x (assoc pool name-y var-x)]
          (evaluate-variable var-x val-y pool))
        (if (nil? val-y)
          (evaluate-variable var-y val-x pool)
          (unify val-x val-y pool))))))


(extend-protocol TermInterface
  logic.term.PrologVariable

  (unify
   [x y pool]
   (if (prolog-variable? y)
     (bind-variables x y pool)
     (evaluate-variable x y pool)))

  (generate
   [var names]
   (generate-variable var names))

  (get-variables
   [var]
   (get-variable-variables var))

  (output
   [var]
   (output-variable var)))



;; ===========================================================================
;;  Prolog Atoms: cat, dOG, cat_dog, 'some atom+wh@tever s&mbols'
;;
;;  It's a general-purpose name with no inherent meaning.
;;
;;  Atom "atom" can be unified with atom "'atom'".
;;
(defrecord PrologAtom [name])


(defn create-atom [name]
  (if (re-matches (re-pattern #"[a-z][a-zA-Z_]*|'[^']+'") name)
    (PrologAtom. name)
    (throw (Exception. (str name " is invalid PrologAtom name!")))))


(defn prolog-atom? [atom]
  (same? (type atom) logic.term.PrologAtom))


(defn unify-atoms
  "Two atoms unify if they have exactly the same names, or one is the same placed in ' '."
  [x y pool]
  ;; Removing the ' ' and checking if the results are the same.
  (let [name-x (re-find (re-pattern #"[^']+") (:name x))
        name-y (re-find (re-pattern #"[^']+") (:name y))]
    (if (same? name-x name-y)
      (if (re-matches (re-pattern #"[a-zA-Z_]+") name-x)
        [(create-atom name-x) pool]
        [(create-atom (str \' name-x \')) pool])
      [false pool])))


(defn generate-atom
  "An Atom holds no variables, so it remains the same."
  [atom names]
  [atom names])


(defn output-atom
  "An Atom is printed as it's name."
  [atom]
  (:name atom))


(extend-protocol TermInterface
  logic.term.PrologAtom

  (unify
   [atom-x atom-y pool]
   (cond
    (prolog-atom? atom-y)
      (unify-atoms atom-x atom-y pool)
    (prolog-variable? atom-y)
      (evaluate-variable atom-y atom-x pool)
    :else
     [false pool]))

  (generate
   [atom names]
   (generate-atom atom names))

  (output
   [atom]
   (output-atom atom))

  (get-variables
   [atom]
   ;; An Atom holds no Variables.
   #{}))



;; ===========================================================================
;;  Prolog Numbers: 1, 5.5.
;;  They are just numbers.
;;
;;  Numbers can be used in mathematical expressions.
;;
(defrecord PrologNumber [value])


(defn create-number [n]
  (if (number? n)
    (PrologNumber. n)
    (throw (Exception. (str n " is illegal PrologNumber value!")))))


(defn prolog-number? [number]
  (same? (type number)
         logic.term.PrologNumber))


(defn unify-numbers
  "Two Numbers unify if their values are equal."
  [x y pool]
  (if (= (:value x)
         (:value y))
    [x pool]
    [false pool]))


(defn output-number
  "A Number is printed as it's value."
  [num]
  (str (:value num)))


(defn generate-number
  "A Number holds no Variables, so it remains exactily the same."
  [num names]
  [num names])


(extend-protocol TermInterface
  logic.term.PrologNumber

  (unify
   [x y pool]
   (cond
    (prolog-number? y)
      (unify-numbers x y pool)
    (prolog-variable? y)
      (evaluate-variable y x pool)

    :else
      [false pool]))

  (output
   [num]
   (output-number num))

  (generate
   [num names]
   (generate-number num names))

  (get-variables
   ;; A Number holds no variables.
   [num]
   #{}))



;; ============================================================================
;;  Prolog String: "string", "Tom is a cat!"
;;
(defrecord PrologString [string])


(defn create-string [s]
  (if (string? s)
    (PrologString. s)
    (throw (Exception. (str s " is illegal PrologString value.")))))


(defn prolog-string? [string]
  (same? (type string)
         logic.term.PrologString))


(defn unify-strings [x y pool]
  (if (= (:string x)
         (:string y))
    [x pool]
    [false pool]))


(defn generate-string
  "A String holds no variables, so it remains the same"
  [str pool]
  [str pool])


(defn output-string
  "A Prolog String is printed as the string it represents."
  [str]
  (:string str))


(extend-protocol TermInterface
  logic.term.PrologString

  (unify [x y pool]
    (cond
     (prolog-string? y)
       (unify-strings x y pool)
     (prolog-variable? y)
       (evaluate-variable y x pool)
     :else
       [false pool]))

  (generate
   [s names]
   (generate-string s names))

  (output
   [s]
   (str "\"" (:string s) "\""))

  (get-variables
   [s]
   ;; A String holds no Variables.
   #{}))



;; ===========================================================================
;;  PrologArguments: (Var, atom, [1,2 | [3]]).
;;
;;  It's a list of Prolog Terms.
;;
(defrecord PrologArguments [args])


(defn create-arguments [args]
  (PrologArguments. (mapv create args)))


(defn prolog-arguments? [term]
  (= (type term)
     logic.term.PrologArguments))


(defn unify-arguments
  "Two Arguments lists unify if each twoplet of elements unify."
  [args-x args-y pool]
  (if (different? (count (:args args-x))
                    (count (:args args-y)))
      [false pool]
      (->>
       (mapv vector
             (:args args-x)
             (:args args-y))
       (reduce (fn [[args pool] [x y]]
                 (if (false? args)
                   [false pool]
                   (let [[z new-pool] (unify x y pool)]
                     (if (false? z)
                       [false pool]
                       [(conj args z)
                        new-pool]))))
               [[] pool])
       (#(if (false? (first %))
           [false pool]
           [(PrologArguments. (first %))
            (second %)])))))


(defn generate-arguments
  "Returns a new Prolog Arguments list with new names generated for the unique Variables."
  [args names]
    (let [[new-args new-names]
        (reduce
         (fn [[res names] term]
           (let [[new-term new-names]
                 (generate term names)]
             [(conj res new-term) new-names]))
         [[] names] (:args args))]
    [(PrologArguments. new-args) new-names]))


(defn output-arguments
  "Returns a string with the output form of the given Arguments list."
  [args]
  (str "("
       (when-not (empty? (:args args))
         (reduce #(str %1 ", " (output %2))
                 (output (first (:args args)))
                 (rest (:args args))))
       ")"))


(defn get-arguments-variables
  "Returns a set of all Variables holded by the Arguments list."
  [args]
  (reduce clojure.set/union
          #{}
          (get-variables (:args args))))


(extend-protocol TermInterface
  logic.term.PrologArguments

  (unify
   [args-x args-y pool]
     (if (prolog-arguments? args-y)
       (unify-arguments args-x args-y pool)
       [false pool]))

  (generate
   [args names]
   (generate-arguments args names))

  (output
   [args]
   (output-arguments args))

  (get-variables
   [args]
   (get-arguments-variables args)))



;; ============================================================================
;;  Prolog List: [A, B, C]. [A | X]. [_ | 3]. [1 | _].
;;  List of Prolog Temrs - the elementss can be anything.
;;
;;  They have a head [1, 2 and a tail | X], where the head
;;  contains elemens and the tail is another list (could be and empty one),
;;  or a variable.
;;
(defrecord PrologList [head tail])


(defn create-list [elems]
  (loop [new-head []
         old-head elems]
    (if (empty? old-head)
      (PrologList. new-head [])
      (let [elem (first old-head)]
        (if (same? elem :|)
          (let [last (second old-head)]
            (if (= second [])
              (PrologList. new-head [])
              (PrologList. new-head (create last))))
          (recur (conj new-head (create elem))
                 (rest old-head)))))))


(defn prolog-list? [list]
  (same? (type list)
         logic.term.PrologList))


(defn reconstruct-list
  "Recreates the same List with a simpler structure (if it's possible)."
  [list]
  (if (= [] (:head list))
    ;; Empty head -> only the tail remains.
    (if (= [] (:tail list))
      (PrologList. [] [])
      (:tail list))

    (if (prolog-list? (:tail list))
      (let [new-tail (reconstruct-list (:tail list))]
        (PrologList. (vec (concat (:head list)
                                  (:head new-tail)))
                     (:tail new-tail)))
      ;; If the tail is not another list, the structure cannot be changed.
      list)))


(defn unify-lists
  "Two lists unify if their initial elements unify, and the lists which remain after removing both initial elements unify."
  [list-x list-y pool]
  (cond
   (and (= [] (:head list-x))
        (= [] (:head list-y)))
     [list-x pool]
   (or (= [] (:head list-x))
       (= [] (:head list-y)))
     [false pool]

   :else
     (let [size (min (count (:head list-x))
                     (count (:head list-y)))
           [new-head head-pool] (->>
                                 (vector (subvec (:head list-x) 0 size)
                                         (subvec (:head list-y) 0 size))
                                 (#(unify (PrologArguments. (first %))
                                          (PrologArguments. (second %))
                                          pool)))]
       (if (false? new-head)
         [false pool]
         (let [rest-x (subvec (:head list-x) size)
               rest-y (subvec (:head list-y) size)
               [new-tail tail-pool] (unify (reconstruct-list
                                            (PrologList. rest-x
                                                         (:tail list-x)))
                                           (reconstruct-list
                                            (PrologList. rest-y
                                                         (:tail list-y)))
                                            head-pool)]
           (if (false? new-tail)
             [false pool]
             [(reconstruct-list
               (PrologList. (:args new-head) new-tail))
              tail-pool]))))))


(defn get-list-variables [list]
  (reduce #(clojure.set/union %1 (get-variables %2))
          (if (= [] (:tail list))
            #{}
            (get-variables (:tail list)))
          (:head list)))


(defn generate-list [list names]
  (let [[new-head head-names] (->
                              (PrologArguments. (:head list))
                              (generate names)
                              (#(vector (:args (first %))
                                        (second %))))]
    (if (= [] (:tail list))
      [(PrologList. new-head [])
       head-names]
      (let [[new-tail tail-names] (generate (:tail list)
                                            head-names)]
        [(PrologList. new-head new-tail)
         tail-names]))))


(defn output-list [list]
  (str "["
       (when-not (= [] (:head list))
         (reduce #(str %1 ", " (output %2))
                 (output (first (:head list)))
                 (rest (:head list))))
       (when-not (= [] (:tail list))
         (str " | "
              (output (:tail list))))
       "]"))


(extend-protocol TermInterface
  logic.term.PrologList

  (unify [x y pool]
    (cond
     (prolog-list? y)
       (unify-lists x y pool)
     (prolog-variable? y)
       (evaluate-variable y x pool)
     :else
       [false pool]))

  (get-variables
   [list]
   (get-list-variables list))

  (generate
   [list names]
   (generate-list list names))

  (output [list]
    (output-list list)))

;; ===========================================================================
;;  Prolog Fact: cat(tom). member(X, Y).
;;  Each fact has a name and arguments.
;;  The name is a Prolog Atom.
;;
(defrecord PrologFact [atom args])


(defn prolog-fact? [term]
  (same? (type term)
         logic.term.PrologFact))


(defn create-fact [[sign atom args]]
  (if (= sign :fact)
    (PrologFact. (create-atom atom)
                 (create-arguments args))
    (throw (Exception. "To create a PrologFact from a vector it must start with :fact."))))


(defn unify-facts [fact-x fact-y pool]
  (if (false? (first (unify-atoms (:atom fact-x)
                                  (:atom fact-y)
                                  pool)))
    [false pool]
    (unify-arguments (:args fact-x)
                     (:args fact-y)
                     pool)))


(defn generate-fact [fact names]
  (let [[new-args new-names] (generate-arguments
                              (:args fact)
                              names)]
    [(PrologFact. (:atom fact)
                 new-args)
     new-names]))


(defn get-fact-variables [fact]
  (get-arguments-variables (:args fact)))


(defn output-fact [fact]
  (str (output (:atom fact))
       (output (:args fact))))


(extend-protocol TermInterface
  logic.term.PrologFact

  (unify
   [x y pool]
   (cond
    (prolog-fact? y)
      (unify-facts x y pool)
    (prolog-variable? y)
      (evaluate-variable y x pool)
    :else
      [false pool]))

  (generate [fact names] (generate-fact fact names))
  (get-variables [fact] (get-fact-variables fact))
  (output [fact] (output-fact fact)))


(defn resolve-facts [fact-x fact-y pool]
  (let [[new-fact new-pool]
        (unify-facts fact-x fact-y pool)]
    (if (false? new-fact)
      [false pool]
      [true new-pool])))



;; ===============================================================================
;;  Prolog Disjunction: member(L, [1,2]); member(L, [2,3]).
;;
(defrecord PrologDisjunction [terms])


(defn create-disjunction [[sign & terms]]
  (if (= sign :disj)
    (PrologDisjunction. (mapv create terms))
    (throw (Exception. "To create a PrologDisjunction from a vector it must start with :disj."))))


(defn prolog-disjunction? [term]
  (same? (type term)
         logic.term.PrologDisjunction))


(defn get-disjunction-variables [disj]
  (reduce #(clojure.set/union %1 (get-variables %2)) #{} (:terms disj)))


(defn generate-disjunction [disj names]
  (let [[new-terms new-names]
        (generate-arguments (PrologArguments. (:terms disj))
                            names)]
    [(PrologDisjunction. (:args new-terms))
     new-names]))


(defn output-disjunction [disj]
  (if (empty? (:terms disj))
    ""
    (subs
     (reduce #(str %1 "; " %2)
             ""
             (:terms disj))
     2)))


(extend-protocol TermInterface
  logic.term.PrologDisjunction
  (generate [disj names] (generate-disjunction disj names))
  (get-variables [disj] (get-disjunction-variables disj))
  (output [disj] (output-disjunction disj)))


;; ===============================================================================
;;  Prolog Conjunction: member(L, [1,2]), member(L, [2,3])
;;
(defrecord PrologConjunction [terms])


(defn create-conjunction [[sign & terms]]
  (if (= sign :conj)
    (PrologConjunction. (mapv create terms))
    (throw (Exception. "To create a PrologConjunction from a vector it must start with :conj."))))


(defn prolog-conjunction? [term]
  (same? (type term)
         logic.term.PrologConjunction))


(defn get-conjunction-variables [conj]
  (reduce #(clojure.set/union %1
                              (get-variables %2))
          #{}
          (:terms conj)))


(defn generate-conjunction [conj names]
  (let [[new-terms new-names]
        (generate-arguments (PrologArguments. (:terms conj))
                            names)]
    [(PrologConjunction. (:args new-terms))
     new-names]))



(defn output-conjunction [conj]
  (if (empty? (:terms conj))
    ""
    (subs
     (reduce (fn [out term]
               (if (prolog-disjunction? term)
                 (str out
                      ", ("
                      (output term)
                      ")")

                 (str out
                      ", "
                      (output term))))
             ""
             (:terms conj))
     2)))


(extend-protocol TermInterface
  logic.term.PrologConjunction
  (get-variables [conj] (get-conjunction-variables conj))
  (generate [conj names] (generate-conjunction conj names))
  (output [conj] (output-conjunction conj)))



;; ===============================================================================
;;  Prolog Negation: not(member(1, [2,3])).
;;
(defrecord PrologNegation [term])


(defn create-negation [[sign term]]
  (if (= sign :not)
    (PrologNegation. (create term))
    (throw (Exception. "To create a PrologNegation from a vector it must start with :not."))))


(defn prolog-negation? [term]
  (same? (type term)
         logic.term.PrologNegation))


(defn get-negation-variables [neg]
  (get-variables (:term neg)))


(defn generate-negation [neg names]
  (let [[new-term new-names]
        (generate (:term neg) names)]
    [(PrologNegation. new-term)
     new-names]))


(defn output-negation [neg]
  (str "not("
       (output (:term neg))
       ")"))


(extend-protocol TermInterface
  logic.term.PrologNegation
  (get-variables [neg] (get-negation-variables neg))
  (generate [neg names] (generate-negation neg names))
  (output [neg] (output-negation neg)))



;; ===============================================================================
;;  Prolog Expression: 2 + 2, X is 3 * 8.
;;
;;                                           is
;;                                          /  \
;;  Actualy, they look like this:   +   ,  X    *
;;                                 / \         / \
;;                                2   2       3   5
;;
;;  They are secial type of Prolog Facts.
;;  They have exactly two arguments and are used only for mathematical expressions.
;;
(defrecord PrologExpression [name left right])


(defn create-expression [[sign left name right]]
  (if (= sign :expr)
    (PrologExpression. name (create left) (create right))
    (throw (Exception. "To create a PrologExpression from a vector it must start with :expr"))))


(defn prolog-expression? [term]
  (same? (type term)
         logic.term.PrologExpression))


(defn generate-expression [expr names]
  (let [[new-left names-left] (generate (:left expr) names)
        [new-right new-names] (generate (:right expr) names-left)]
    [(PrologExpression. (:name expr) new-left new-right)
     new-names]))


(defn get-expression-variables [expr]
  (clojure.set/union (get-variables (:left expr))
                     (get-variables (:right expr))))


(defn output-expression [expr]
  (str "("
       (output (:left expr))
       " "
       (:name expr)
       " "
       (output (:right expr))
       ")"))


(extend-protocol TermInterface
  logic.term.PrologExpression
  (generate [expr names] (generate-expression expr names))
  (get-variavles [expr] (get-expression-variables expr))
  (output [expr] (output-expression expr)))



;; ==================================================================================
;;  Prolog Formula: random(Base, Max, Number).
;;
;;  A Prolog Formula is a Prolog Fact that executes a function when it gets resolved.
;;  User cannot create Formulas, they are only built-in.
;;
(defrecord PrologFormula [fact func])


(defn create-formula [[sign name args func]]
  (if (= sign :form)
    (PrologFormula. (create-fact [:fact name args])
                    func)
    (throw (Exception. "To create a Prolog Formula from a vector it must start with :form."))))


(defn prolog-formula? [term]
  (same? (type term)
         logic.term.PrologFormula))


(defn generate-formula [form names]
  (let [[new-fact new-names] (generate-fact (:fact form) names)]
    [(PrologFormula. new-fact (:func form))
     new-names]))


(extend-protocol TermInterface
  logic.term.PrologFormula
  (generate [form names] (generate-formula form names)))


;; ==================================================================================
;;  Prolog Rule: member(A, [_ | X]) :- member(A, X).
;;
;;  A Prolog Rule has a head and a body.
;;  The head is a Prolog Structure. A Rule is defined by it's head.
;;  The body is a Term: Fact, Conjunction or Disjunction.
;;  The symbol :- is called neck. It means "body => head",
;;  or "if the body is true, then the head is true."
;;

(defrecord PrologRule [head body])


(defn create-rule [[sign name args body]]
  (if (same? sign :rule)
    (let [new-head (create-fact [:fact name args])
          new-body (create body)]
      (PrologRule. new-head new-body))
    (throw
     (Exception. "To create a PrologRule from a vector, it must start with :rule keyword."))))


(defn prolog-rule? [rule]
  (same? (type rule)
         logic.term.PrologRule))


(defn match-fact->rule
  [fact rule pool]
  (let [[new-head new-pool]
        (unify-facts fact (:head rule) pool)]
    (if (false? new-head)
      [false pool]
      ;; TODO, it must get evaluated here.
      [(PrologRule. new-head new-pool) new-pool])))


(defn generate-rule
  [rule names]
  (let [[new-head head-names]
        (generate-fact (:head rule) names)

        [new-body new-names]
        (generate (:body rule) head-names)]
    [(PrologRule. new-head new-body)
     new-names]))


(defn output-rule [rule]
  (str (output-fact (:head rule))
       ":-"
       (output(:body rule))))


(extend-protocol TermInterface
  logic.term.PrologRule

  (generate
   [rule names]
   (generate-rule rule names))

  (output
   [rule]
   (output-rule rule)))



;; =========================================================
;;  TermInterface create function.


(extend-protocol TermInterface
  String

  (create
   [inp]
   (cond
    (re-matches (re-pattern #"\"[^\"]+\"") inp)
      (create-string inp)
    (re-matches (re-pattern #"[_A-Z][A-Za-z_]*") inp)
      (create-variable inp)
    :else
      (create-atom inp)))

  Number
  (create [inp]
    (create-number inp))

  clojure.lang.PersistentVector
  (create [inp]
    (cond
     (= :fact (first inp))
       (create-fact inp)
     (= :rule (first inp))
       (create-rule inp)
     (= :conj (first inp))
       (create-conjunction inp)
     (= :disj (first inp))
       (create-disjunction inp)
     (= :not (first inp))
       (create-negation inp)
     (= :expr (first inp))
       (create-expression inp)
     (= :form (first inp))
       (create-formula inp)
     :else
       (create-list inp))))

(defn resolve [fact term pool]
  (cond
   (prolog-fact? term)
     (resolve-facts fact term pool)))
