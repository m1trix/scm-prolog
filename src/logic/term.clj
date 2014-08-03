(ns logic.term
  [:require [logic.util :refer :all]
            [clojure.set]])


(defprotocol TermInterface
  (create [term])
  (substitude [term pool])
  (generate [term names])
  (get-variables [term]))

(defmulti output #(type %1))
(defmulti unify (subvec #(mapv type %&) 0 2))
(defmulti generate #(type %1))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Variable: X. Whatever.                                                  # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  A Prolog Variable always starts with a capital letter.                         # ;
; #  It does not have a value when it is created, but it can be evaluated to        # ;
; #  any Prolog Term. Once a value is given to a Variable, it cannot be changed.    # ;
; #                                                                                 # ;
; #  The process of unifying two Variables is a process of binding them together.   # ;
; #  That means that whenever one of them gets evaluated, the other is also         # ;
; #  evaluated.                                                                     # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologVariable [name])


(defn create-variable [var]
  (if (re-matches (re-pattern #"[_A-Z][0-9A-Za-z_]*") var)
    (PrologVariable. var)
    (throw (Exception. (str var " is invalid PrologVariable name!")))))


(defn prolog-variable? [var]
  (= (type var)
     logic.term.PrologVariable))


(defn extract
  "Extracts the real value of the Variable from the pool."
  [var pool]
  (let [val (pool var)]
    (if (or (nil? val)
            (set? val))
      var
      (if (prolog-variable? val)
        (extract val pool)
        val))))


(defn bounds
  "Returns a set of all Variables bound together with the given one."
  [var pool]
  (let [val (pool var)]
    (cond
     (set? val) val

     (prolog-variable? val)
       (bounds val pool)

     :else #{})))



;; TODO:
(defn generate [var names]
  (let [new-name (str (gensym '_G))
        map-name (get ())]

(defn get-variable-variables
  "A PrologVariable holds only itself, but random variables are not important."
  [var]
  (if (= (:name var) "_")
    #{}
    #{var}))


(defmethod output PrologVariable
  [var pool]
  (let [val (get pool var)]
    (if (or (nil? val)
            (set? val))
      (:name var)
      (output val pool))))


(defn evaluate-variable [var term pool]
  (let [name (:name var)
        val (get pool name)]
    (if (nil? val)
      [term (assoc pool name term)]
      (unify val term pool))))


(defn bind [var-x var-y pool]
  (let [bounds-x (bounds var-x pool)
        bounds-y (bounds var-y pool)
        new-bounds (clojure.set/union bounds-x bounds-y)]
    [var-x
     (assoc pool
      var-x (conj new-bounds var-y)
      var-y var-x)]))


(defmethod unify [PrologVariable PrologVariable]
  ;; Two variables unify by agreeing to bind to each together.
  ;; That means that whenever one of them is evaluated,
  ;; the other one is also evaluated.
  [var-x var-y pool]
  (let [val-x (extract var-x pool)
        val-y (extract var-y pool)]
    (cond

     (and (prolog-variable? val-x)
          (prolog-variable? val-y))
     (bind val-x val-y pool)

     (prolog-variable? val-x)
     (unify val-x val-y pool)

     (prolog-variable? val-y)
     (unify val-y val-x pool)

     :else
     (unify val-x val-y pool))))


(defn substitude-variable [var pool]
  (let [name (:name var)
        val (get pool name)]
    (if (nil? val)
      [var pool]
      (let [[new-val new-pool] (substitude val pool)]
        [new-val (assoc new-pool name new-val)]))))



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
  [atom pool]
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

  (generate [atom names] (generate-atom atom names))
  (output [atom pool] (output-atom atom pool))
  (get-variables [atom] #{})
  (substitude [atom pool] [atom pool]))



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
  [num pool]
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

  (output [num pool] (output-number num pool))
  (generate [num names] (generate-number num names))
  (get-variables [num]  #{})
  (substitude [num pool] [num pool]))



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
  [str pool]
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

  (generate [s names] (generate-string s names))
  (output [s pool] (str "\"" (:string s) "\""))
  (get-variables [s] #{})
  (substitude [s pool] [s pool]))



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
  [args pool]
  (str "("
       (when-not (empty? (:args args))
         (reduce #(str %1 ", " (output %2 pool))
                 (output (first (:args args)) pool)
                 (rest (:args args))))
       ")"))


(defn get-arguments-variables
  "Returns a set of all Variables holded by the Arguments list."
  [args]
  (reduce #(clojure.set/union %1 (get-variables %2))
          #{}
          (:args args)))


(defn substitude-arguments [args pool]
  (let [[new-args new-pool]
        (reduce (fn [[res pool] term]
                  (let [[new-term new-pool] (substitude term pool)]
                    [(conj res new-term) new-pool]))
                [[] pool]
                (:args args))]
    [(PrologArguments. new-args)
     new-pool]))


(extend-protocol TermInterface
  logic.term.PrologArguments

  (unify
   [args-x args-y pool]
     (if (prolog-arguments? args-y)
       (unify-arguments args-x args-y pool)
       [false pool]))

  (generate [args names] (generate-arguments args names))
  (output [args pool] (output-arguments args pool))
  (get-variables [args] (get-arguments-variables args))
  (substitude [args pool] (substitude-arguments args pool)))



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


(defn output-list [list pool]
  (str "["
       (when-not (= [] (:head list))
         (reduce #(str %1 ", " (output %2 pool))
                 (output (first (:head list)) pool)
                 (rest (:head list))))
       (when-not (= [] (:tail list))
         (str " | "
              (output (:tail list) pool)))
       "]"))


(defn substitude-list [list pool]
  (let [[new-head head-pool] (substitude (PrologArguments. (:head list))
                                       pool)]
    (if (= [] (:tail list))
      [(PrologList. (:args new-head) [])
       head-pool]
      (let [[new-tail tail-pool] (substitude (:tail list) head-pool)]
        [(reconstruct-list (PrologList. (:args new-head) new-tail))
         tail-pool]))))


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

  (get-variables [list] (get-list-variables list))
  (generate [list names] (generate-list list names))
  (output [list pool] (output-list list pool))
  (substitude [list pool] (substitude-list list pool)))

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
    (let [[new-args new-pool] (unify-arguments (:args fact-x)
                                               (:args fact-y)
                                               pool)]
      (if (false? new-args)
        [false pool]
        [(PrologFact. (:atom fact-x)
                      new-args)
         new-pool]))))


(defn generate-fact [fact names]
  (let [[new-args new-names] (generate-arguments
                              (:args fact)
                              names)]
    [(PrologFact. (:atom fact)
                 new-args)
     new-names]))


(defn get-fact-variables [fact]
  (get-arguments-variables (:args fact)))


(defn output-fact [fact pool]
  (str (output-atom (:atom fact) pool)
       (output-arguments (:args fact) pool)))


(defn substitude-fact [fact pool]
  (let [[new-args new-pool] (substitude-arguments (:args fact) pool)]
    [(PrologFact. (:atom fact) new-args)
     new-pool]))


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
  (output [fact pool] (output-fact fact pool))
  (substitude [fact pool] (substitude-fact fact pool)))


(defn resolve-facts [fact-x fact-y pool]
  (let [[new-fact new-pool]
        (unify-facts fact-x fact-y pool)]
    (if (false? new-fact)
      [false false pool]
      [true new-fact new-pool])))


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


(defn output-disjunction [disj pool]
  (if (empty? (:terms disj))
    ""
    (subs
     (reduce #(str %1 "; " (output %2 pool))
             ""
             (:terms disj))
     2)))


(extend-protocol TermInterface
  logic.term.PrologDisjunction
  (generate [disj names] (generate-disjunction disj names))
  (get-variables [disj] (get-disjunction-variables disj))
  (output [disj pool] (output-disjunction disj pool)))


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



(defn output-conjunction [conj pool]
  (if (empty? (:terms conj))
    ""
    (str "("
         (subs
          (reduce #(str %1 ", " (output %2 pool))
                  ""
                  (:terms conj))
          2)
         ")")))


(defn substitude-conjunction [conjunct pool]
  (let [[res new-pool] (reduce (fn [[res pool] term]
                                 (let [[new-term new-pool] (substitude term pool)]
                                   [(conj res new-term)
                                    new-pool]))
                               [[] pool]
                               (:terms conjunct))]
    [(PrologConjunction. res)
     new-pool]))


(extend-protocol TermInterface
  logic.term.PrologConjunction
  (get-variables [conj] (get-conjunction-variables conj))
  (generate [conj names] (generate-conjunction conj names))
  (output [conj pool] (output-conjunction conj pool))
  (substitude [conj pool] (substitude-conjunction conj pool)))



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


(defn output-negation [neg pool]
  (str "not("
       (output (:term neg) pool)
       ")"))


(extend-protocol TermInterface
  logic.term.PrologNegation
  (get-variables [neg] (get-negation-variables neg))
  (generate [neg names] (generate-negation neg names))
  (output [neg pool] (output-negation neg pool)))



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


(defn output-expression [expr pool]
  (str "("
       (output (:left expr) pool)
       " "
       (:name expr)
       " "
       (output (:right expr) pool)
       ")"))


(extend-protocol TermInterface
  logic.term.PrologExpression
  (generate [expr names] (generate-expression expr names))
  (get-variavles [expr] (get-expression-variables expr))
  (output [expr pool] (output-expression expr pool)))



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


(defn generate-rule
  [rule names]
  (let [[new-head head-names]
        (generate-fact (:head rule) names)

        [new-body new-names]
        (generate (:body rule) head-names)]
    [(PrologRule. new-head new-body)
     new-names]))


(defn substitude-rule [rule pool]
  (let [[new-head head-pool] (substitude-fact (:head rule) pool)
        [new-body body-pool] (substitude (:body rule) head-pool)]
    [(PrologRule. new-head new-body)
     body-pool]))


(defn output-rule [rule pool]
  (str (output (:head rule) pool)
       " :- "
       (output (:body rule) pool)))


(extend-protocol TermInterface
  logic.term.PrologRule

  (generate [rule names] (generate-rule rule names))
  (output [rule pool] (output-rule rule pool))
  (substitude [rule pool] (substitude-rule [rule pool])))


(defn resolve-fact->rule [fact rule pool]
  (let [[status new-fact new-pool]
        (resolve-facts fact (:head rule) pool)
        [new-body final-pool] (substitude (:body rule) new-pool)]
    (if (false? status)
      [false false pool]
      [:unresolved
       (PrologRule. new-fact new-body)
       final-pool])))



;; =========================================================
;;  TermInterface create function.


(extend-protocol TermInterface
  String

  (create
   [inp]
   (cond
    (re-matches (re-pattern #"\"[^\"]+\"") inp)
      (create-string inp)
    (re-matches (re-pattern #"[_A-Z][0-9A-Za-z_]*") inp)
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


;;  TODO:
;; (extend-protocol TermInterface
;;   java.lang.Boolean

;;   (output [inp] (str inp)))


(defn resolve [fact term pool]
  (cond
   (prolog-fact? term)
     (resolve-facts fact term pool)
   (prolog-rule? term)
     (resolve-fact->rule fact term pool)))
