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
  (if (= sign :%fact%)
    (PrologFact. (create-atom atom)
                 (create-arguments args))
    (throw (Exception. "To create a Fact from a vector it must start with :%fact%."))))


(defn unify-facts [fact-x fact-y pool]
  (if (false? (first (unify-atoms (:atom fact-x)
                                  (:atom fact-y))))
    [false pool]
    (unify-arguments (:args fact-x)
                     (:args fact-y)
                     pool)))


(defn generate-fact [fact names]
  (let [[new-args new-names] (generate-arguments
                              (:args fact)
                              names)]
    [(PrologFact. (:atom fact)
                 new-names)
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

  (generate
   [fact names]
     (generate-fact fact names))

  (get-variables
   [fact]
     (get-fact-variables fact))

  (output
   [fact]
     (output-fact fact)))


;; ===============================================================================
;;  Prolog Rule: member(A, [_ | X]) :- member(A, X).
;;
;;  A Prolog Rule has a head and a body.
;;  The head is a Prolog Structure. A Rule is defined by it's head.
;;  The body is a Term: Fact, Conjunction or Disjunction.
;;  The symbol :- is called neck. It means "body => head",
;;  or "if the body is true, then the head is true."
;;

(defrecord PrologRule [head body])


(defn >rule< [[sign name args body]]
  (if (same? sign :%rule%)
    (let [new-head
          (>fact< [:%fact% name args])
          new-body (>term< body)]
      (PrologRule.
       new-head
       new-body))
    (throw (Exception. "To create a PrologFunctor from a vector, it must start with :%fact% keyword."))))


(defn prolog-rule? [rule]
  (same? (type rule)
         logic.term.PrologRule))


(defn =rule=
  "It's a specific evaluation. It evaluates and returns only the body of the rule."
  [rule pool]
  (=term= (:body rule) pool))


(defn unify-fact->rule
  [fact rule pool]
  (let [[new-head new-pool]
        (unify-facts
         (:head rule)
         fact
         pool)]
    (if (false? new-head)
      [false pool]
      [(=rule= new-head new-pool) new-pool])))


(defn generate-rule
  [rule names]
  (let [[new-head head-names] (generate-fact (:head rule)
                                             names)
        [new-body new-names] (generate-term (:body rule)
                                            head-names)]
    [(PrologRule. new-head new-body)
     new-names]))


(extend-protocol TermInterface
  logic.term.PrologRule

  (output [x]
    (str (output (:head x))
         " :- "
         (output (:body x)))))



;; ============================================================================
;;  PrologMath: 1 + 2, X is 5 + 3.
;;
;;                                 +       is
;;                                / \     /  \
;;  Actualy, it looks more like: 1   2 , X    +
;;                                           / \
;;                                          5   3
;;
(defrecord PrologMath [name left right])


(defn >math< [[sign name left right]]
  (if (same? sign :#math)
    (PrologMath. name
                 (>term< left)
                 (>term< right))
    (throw (Exception. "To create a PrologMath from a vector, it must start with the :#math keyword."))))


(defn prolog-math? [math]
  (same? (type math)
         logic.term.PrologMath))


(defn =math= [math pool]
  (PrologMath. (:name math)
               (=term= (:left math) pool)
               (=term= (:right math) pool)))


(defn generate-math [math names]
  (let [[new-left names-left] (generate-term (:left math) names)
        [new-right names-right] (generate-term (:right math) names-left)]
    [(PrologMath. (:name math)
                  new-left
                  new-right)
     names-right]))


(defn get-math-variables [math]
  (clojure.set/union (get-term-variables (:left math))
                     (get-term-variables (:right math))))


(defn output-math [math]
  (let [out-left (output-term (:left math))
        out-right (output-term (:right math))]
    (str "("
         out-left
         " "
         (keyword->string (:name math))
         " "
         out-right
         ")")))



;; =============================================================================
;;  PrologMethod: A + B, X is 42.01 - 0.01.
;;
;;
(defrecord PrologMethod [name args func])


(defn >method< [[sign name args func]]
  (if (same? sign :#met)
    (let [new-args (mapv >term< args)]
      (PrologMethod. name new-args func))
    (throw (Exception. "To create PrologMethod from a vector, it must start with :#meth keyword."))))


(defn prolog-method? [method]
  (same? (type method)
         logic.term.PrologMethod))


(defn =method= [method pool]
  (PrologMethod. (:name method)
                 (mapv #(=term= % pool) (:args method))
                 (:func method)))


(defn execute [method pool]
  ((:func method) (:args method) pool))


(defn generate-method [method names]
  (let [[new-args new-pool] (generate-vector (:args method) names)]
    [(PrologMethod. (:name method)
                    new-args
                    (:func method))
     new-pool]))

(defn match-math->method
  [math method pool]
  (if (or (different? (:name math)
                      (:name method))
          (different? 2 (count (:args method))))
    [false pool]
    (let [[new-left pool-left] (unify-terms (:left math)
                                            (first (:args method))
                                            pool)]
      (if (false? new-left)
        [false [pool]]
        (let [[new-right pool-right] (unify-terms (:right math)
                                                  (second (:args method))
                                                  pool-left)]
          (if (false? new-right)
            [false pool]
            (execute (PrologMethod. (:name method)
                                    [new-left new-right]
                                    (:func method))
                     pool)))))))


(defn unify-methods [method-x method-y pool])


(defn get-method-variables
  [method]
  (reduce #(clojure.set/union %1 (get-term-variables %2)) #{} (:args method)))


(defn output-method [method]
  (str (keyword->string (:name method))
       (output-arguments (:args method))))











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
     (= :%fact% (first inp))
       (create-fact inp)
     :else
       (create-list inp))))
