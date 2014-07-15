;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logic.term
  [:require [logic.util :refer :all]
            [clojure.set]])


(def unify-terms)
(def generate-term)
(def output-term)
(def =term=)
(def >term<)
(def get-term-vars)


(defn generate-vector
  [elems pool]
  (loop [new-elems []
        old-elems elems
        new-pool pool]
    (if (empty? old-elems)
      [new-elems new-pool]
      (let [elem (first old-elems)
            [new-elem newer-pool] (generate-term elem new-pool)]
        (recur (conj new-elems new-elem)
               (rest old-elems)
               newer-pool)))))



;; ===========================================================================
;;  Prolog Atoms: cat. dog.
;;  They are more like facts - Prolog treats them as a TRUE values.
;;  An Atom name always starts with a small letter.
;;
(defrecord PrologAtom [name])

(defn >atom< [name]
  (if (a-z? name)
    (PrologAtom. name)
    (throw
     (Exception.
      (str
       "Illegal PrologAtom name: \""
       (keyword->string name)
       "\"")))))


(defn prolog-atom? [atom]
  (same? (type atom) logic.term.PrologAtom))


(defn unify-atoms
  "Two atoms unify if they are the same."
  [atom-x atom-y pool]
  (if (same?
       (:name atom-x)
       (:name atom-y))
    [atom-x pool]
    [false pool]))


(defn output-atom
  "Prints the atom to the screen in a specific format."
  [atom]
  (let [text (keyword->string (:name atom))]
    text))




;; ===========================================================================
;;  Prolog Numbers: 1,   5.5.
;;  They are just numbers.
;;
(defrecord PrologNumber [value])


(defn >number< [n]
  (if (number? n)
    (PrologNumber. n)
    (throw (Exception. (str "Illegal PrologNumber value: \"" n "\"")))))


(defn prolog-number? [number]
  (same? (type number) logic.term.PrologNumber))


(defn unify-numbers
  "Two numbers unify if they have the same value."
  [number-x number-y pool]
  (if (same? (:value number-x)
             (:value number-y))
    [number-x pool]
    [false pool]))


(defn output-number
  "Prints the number to the screen in a specific format."
  [number]
  (:value number))





;; ============================================================================
;;  Prolog String: 'str1' 'Yes! It is a string!'
;;  tom. and 'tom'. can be unified.
;;
(defrecord PrologString [string])


(defn >string< [s]
  (if (string? s)
    (PrologString. s)
    (throw (Exception. (str "Illegal PrologString value: \"" s "\"")))))


(defn prolog-string? [string]
  (same? (type string) logic.term.PrologString))


(defn unify-strings
  "Two strings unify if and only if they have precisely the same characters in them."
  [string-x string-y pool]
  (if (= (:string string-x)
         (:string string-y))
    [string-x pool]
    [false pool]))


(defn output-string
  "Prints the string to the screen in a specific format."
  [s]
  (str \' (:string s) \'))




;; ============================================================================
;;  Prolog Variable: X. Whatever.
;;  They do not have a value at their creation.
;;  Once they are evaluated, they cannot be changed.
;;  A Variable name always starts with a capital letter.
;;  The uni-list is a set, that holds all other variables, that are bound to the current one.
;;
;;  A Variable can be bound to another (or many others) variable. That means that, whenever
;;  this Variable gets evaluated, all other Variables that are bound to it, also are evaluated
;;  with the same value.
;;
(defrecord PrologVariable [name])


(defn >variable<
  [name]
  (if (A-Z? name)
    (PrologVariable. name)
    (throw (Exception.
            (str "Invalid PrologVariable name: \""
                 name
                 "\"")))))


(defn prolog-variable? [var]
  (same? (type var)
         logic.term.PrologVariable))


;;
;;  PrologVariable evaluation.
;;
;;  Case 1: The Variable is unbound.
;;    It is evaluated with the value.
;;
;;  Case 2: The Variable is already evaluated.
;;    The two values are unified.
;;    The Variable is evaluated to the result, if it's not false.
;;

(defn =variable=
  ([var pool]
   (let [name (:name var)]
     (if (or (set? (name pool))
             (same? name (:name (name pool))))
       var
       (=term= (name pool) pool))))
  ([var term pool]
   (let [name (:name var)]
     (if (set? (name pool))
       (loop [bounds (name pool)
              new-pool (assoc pool name term)]
         (if (empty? bounds)
           [term new-pool]
           (let [elem-name (first bounds)
                 [_ newest-pool] (=variable=
                                  (>variable< elem-name)
                                  term
                                  new-pool)]
             (recur (rest bounds)
                    newest-pool))))
       (unify-terms (name pool) term pool)))))


;;
;;  PrologVariables unification.
;;
;;  Case 1: Both are not evaluated.
;;    In this case, the two variables are bound to each other -
;;    whenever one of them get evaluated, the other is evaluated also.
;;
;;  Case 2: Y is evaluated - then X is evaluated with the value of Y.
;;  Case 3: X is evaluated - then Y is evaluated with the value of X.
;;  Case 4: Both are evaluated - their values are unifued.
;;
(defn unify-variables
  "If both are not evaluated, it binds them together.
  If one is evaluated, the other gets it's value."
  [var-x var-y pool]
  (let [name-x (:name var-x)
        name-y (:name var-y)]
    (cond
     (same? name-x name-y)
       [name-x pool]
     ;; CASE 1:
     (and (set? (name-x pool))
          (set? (name-y pool)))
       (let [binds-x (conj (name-x pool) name-y)
             binds-y (conj (name-y pool) name-x)]
         [var-x (assoc pool
                  name-x binds-x
                  name-y binds-y)])

     ;; CASE 2:
     (set? (name-x pool))
       (=variable= var-x (name-y pool) pool)

     ;; CASE 3:
     (set? (name-y pool))
       (=variable= var-y (name-x pool) pool)

     ;; CASE 4:
     :else
       (unify-terms (name-x pool)
                    (name-y pool)
                    pool))))


(defn generate-variable
  [var names]
  (if (nil? ((:name var) names))
    (let [new-name (keyword (gensym))]
      [(>variable< new-name) (assoc names
                               (:name var)
                               new-name)])
    [(>variable< ((:name var) names)) names]))



(defn output-variable
  ([var]
   (keyword->string (:name var)))
  ([name pool]
   (if (set? (name pool))
     [(str (keyword->string name)) pool]
     [(str (keyword->string name)
           " = "
           (output-term (name pool))) pool])))



;; ============================================================================
;;  Prolog List: [A, B, C]. [A | X]. [_ | 3]. [1 | _].
;;  List of Prolog Temrs - the elementss can be anything.
;;
;;  They have a head [1, 2 and a tail | X], where the head
;;  contains elemens and the tail is another list (could be and empty one),
;;  or a variable.
;;
(defrecord PrologList [head tail])


(defn >list< [elems]
  (loop [new-head []
         old-head elems]
    (if (empty? old-head)
      (PrologList. new-head [])
      (let [elem (first old-head)]
        (if (same? elem :|)
          (let [last (second old-head)]
            (if (= second [])
              (PrologList. new-head [])
              (PrologList. new-head (>term< last))))
          (recur (conj new-head (>term< elem))
                 (rest old-head)))))))


(defn prolog-list? [list]
  (same? (type list)
         logic.term.PrologList))


(defn unify-tails
  "Unifies the tails of two PrologLists"
  [tail-x tail-y pool]
  (cond

   (and (same? tail-x [])
        (same? tail-y []))
     [[] pool]

   (same? tail-x [])
     (if (prolog-variable? tail-y)
       (unify-terms tail-y (>list< []) pool)
       [false pool])

   (same? tail-y [])
     (if (prolog-variable? tail-x)
       (unify-terms tail-x (>list< []) pool))

   :else
     (unify-terms tail-x tail-y pool)))

(defn get-list-vars
  [list]
  (let [head-vars (reduce #(clojure.set/union %1 (get-term-vars %2)) #{} (:head list))]
    (if (same? [] (:tail list))
      head-vars
      (clojure.set/union head-vars (get-term-vars (:tail list))))))


(defn unify-lists
  "Two lists unify if their initial elements unify, and the lists which remain after removing both initial elements unify."
  [list-x list-y pool]
  (loop [new-head []
         head-x (:head list-x)
         head-y (:head list-y)
         new-pool pool]
    (cond

     ;; Both lists heads are empty => unifying the tails.
     (and (empty? head-x)
          (empty? head-y))
       (let [[new-tail final-pool]
             (unify-tails
              (:tail list-x)
              (:tail list-y)
              new-pool)]
         (if (false? new-tail)
           [false pool]
           [(PrologList. new-head new-tail) final-pool]))

     ;; List-x head is empty => matching it's tail with the rest of list-y.
     (empty? head-x)
       (let [rest-y (PrologList. head-y (:tail list-y))
             [new-tail final-pool] (unify-tails (:tail list-x)
                                                rest-y
                                                new-pool)]
         (if (false? new-tail)
           [false pool]
           [(PrologList. new-head new-tail) final-pool]))

     ;; List-y head is empty => matching it's tail with the rest of list-x.
     (empty? head-y)
       (let [rest-x (PrologList. head-x (:tail list-x))
             [new-tail final-pool] (unify-tails (:tail list-y)
                                                 rest-x
                                                 new-pool)]
         (if (false? new-tail)
           [false pool]
           [(PrologList. new-head new-tail) final-pool]))

     :else
       (let [[new-elem newer-pool] (unify-terms
                                     (first head-x)
                                     (first head-y)
                                     new-pool)]
         (if (false? new-elem)
           [false pool]
           (recur (conj new-head new-elem)
                  (vec (rest head-x))
                  (vec (rest head-y))
                  newer-pool))))))

(defn =list=
  "Replaces all of the list's variables with thieir values from the pool."
  [list pool]
  (let [new-head (mapv #(=term= % pool) (:head list))]
    (if (same? [] (:tail list))
      (PrologList. new-head [])
      (PrologList. new-head (=term= (:tail list)
                                    pool)))))


(defn generate-list
  [list names]
  (loop [new-head []
         old-head (:head list)
         new-names names]
    (if (empty? old-head)
      (if (same? [] (:tail list))
        [(PrologList. new-head []) new-names]
        (let [[new-tail final-names] (generate-term (:tail list) new-names)]
          [(PrologList. new-head new-tail) final-names]))
      (let [old-elem (first old-head)
            [new-elem newest-names] (generate-term old-elem
                                                   new-names)]
        (recur (conj new-head new-elem)
               (rest old-head)
               newest-names)))))


(defn output-list
  "Makes a string, that represents the PrologList in it's output format."
  [list]
  (let [out-tail (if (empty? (:tail list))
                   ""
                   (str "| "
                        (output-term (:tail list))))]
    (loop [out-head "["
           elems (:head list)]
      (cond
       (empty? elems)
         (str out-head out-tail "]")
       (empty? (rest elems))
         (str out-head
              (output-term (first elems))
              out-tail
              "]")
       :else
         (recur (str out-head
                     (output-term (first elems))
                     ",")
                (rest elems))))))



;; ===========================================================================
;;  Prolog Structures: cat(tom). member(X, Y).
;;  Each structure has a name and arguments.
;;
(defrecord PrologStructure [name args])


(defn >structure-args< [args]
  (loop [new-args []
         old-args args]
    (if (empty? old-args)
      new-args
      (let [elem (first old-args)
            new-elem (>term< elem)]
        (recur (conj new-args new-elem)
               (rest old-args))))))


(defn >structure< [name args]
  (if(not (a-z? name))
    (throw (Exception. (str
                        "Invalid PrologStructure name: \""
                        (keyword->string name)
                        "\"")))
    (PrologStructure. name (>structure-args< args))))


(defn prolog-structure? [struct]
  (same? (type struct)
         logic.term.PrologStructure))


(defn unify-args [args-x args-y pool]
  (loop [new-args []
         rest-x args-x
         rest-y args-y
         new-pool pool]
    (if (empty? rest-x)
      [new-args new-pool]
      (let [elem-x (first rest-x)
            elem-y (first rest-y)
            [new-elem newest-pool] (unify-terms elem-x elem-y new-pool)]
        (if (false? new-elem)
          [false pool]
          (recur (conj new-args new-elem)
                 (rest rest-x)
                 (rest rest-y)
                 newest-pool))))))


(defn unify-structures
  [struct-x struct-y pool]
  (cond

   (different? (:name struct-x)
               (:name struct-y))
     [false pool]

   (different? (count (:args struct-x))
               (count (:args struct-y)))
     [false pool]

   :else
     (let [[new-args new-pool] (unify-args (:args struct-x)
                                           (:args struct-y)
                                           pool)]
       (if (false? new-args)
         [false pool]
         [(PrologStructure. (:name struct-x)
                            new-args)
          new-pool]))))


(defn generate-args
  [args names]
  (loop [new-args []
         old-args args
         new-names names]
    (if (empty? old-args)
      [new-args new-names]
      (let [old-elem (first old-args)
            [new-elem newest-names] (generate-term old-elem
                                                   new-names)]
        (recur (conj new-args new-elem)
               (rest old-args)
               newest-names)))))


(defn generate-structure
  [struct names]
  (let [[new-args new-names] (generate-args (:args struct) names)]
    [(PrologStructure. (:name struct)
                      new-args)
     new-names]))


(defn =structure=
  "Replaces all variables of the structure with their values from the pool."
  [struct pool]
  (loop [new-args (mapv #(=term= % pool) (:args struct))]
    (PrologStructure. (:name struct)
                       new-args)))


(defn output-structure
  [struct]
  (loop [out (str (keyword->string (:name struct))
                  "(")
         elems (:args struct)]
    (if (empty? (rest elems))
      (str out
           (output-term (first elems))
           ")")
      (let [out-elem (output-term (first elems))]
        (recur (str out out-elem ", ")
               (rest elems))))))


(defn get-structure-vars
  [struct]
  (reduce #(clojure.set/union %1 (get-term-vars %2)) #{} (:args struct)))


;; ================================================================================
;;  Prolog Conjunct: member(X, [1, 2]), member(X, [2, 3]).
;;  Prolog Disjunct: member(X, [1, 2]); member(X, [2, 3]).
;;
;;  A Prolog Conjunct is a vector of Prolog Structures or Prolog Disjuncts.
;;  No conjunct has a name.
;;  A conjunct is true if all of it's elements are true.
;;
;;  A Prolog Disjunct is a vector or Prolog Structures or Prolog Conjuncts.
;;  No disjunct has a name.
;;  A disjunct is true if atleast one of it's elements is true.
;;
(defrecord PrologConjunct [elems])
(defrecord PrologDisjunct [elems])


(defn >conjunct< [list]
  (if (same? :& (first list))
    (PrologConjunct. (mapv #(>structure< (first %) (second %)) (rest list)))
    (throw (Exception. "To create a PrologConjunct from a vector, it must start with a :& keyword."))))

(defn >disjunct< [list]
  (if (same? :$ (first list))
    (PrologDisjunct. (mapv #(>structure< (first %) (second %)) (rest list)))
    (throw (Exception. "To create a PrologDisjunct from a vector, it must start with a :$ keyword."))))



(defn prolog-conjunct? [conjunct]
  (same? (type conjunct)
         logic.term.PrologConjunct))

(defn prolog-disjunct? [disjunct]
  (same? (type disjunct)
         logic.term.PrologDisjunct))


(defn =conjunct=
  [conjunct pool]
  (PrologConjunct. (mapv #(=term= % pool) (:elems conjunct))))

(defn =disjunct=
  [disjunct pool]
  (PrologDisjunct. (mapv #(=term= % pool) (:elems disjunct))))


(defn generate-conjunct
  [conjunct names]
  (let [[new-elems new-names] (generate-vector (:elems conjunct)
                                               names)]
    [(PrologConjunct. new-elems) new-names]))

(defn generate-disjunct
  [disjunct names]
  (let [[new-elems new-names] (generate-vector (:elems disjunct)
                                               names)]
    [(PrologDisjunct. new-elems) new-names]))


(defn get-conjunct-vars
  [conjunct]
  (reduce #(clojure.set/union %1 (get-term-vars %2)) #{} (:elems conjunct)))

(defn get-disjunct-vars
  [disjunct]
  (reduce #(clojure.set/union %1 (get-term-vars %2)) #{} (:elems disjunct)))


(defn output-conjunct
  [conjunct]
  (loop [out "("
         elems (:elems conjunct)]
    (if (empty? (rest elems))
      (str out
           (output-term (first elems))
           ")")
      (let [out-elem (output-term (first elems))]
        (recur (str out out-elem ", ")
               (rest elems))))))


;; ===============================================================================
;;  Prolog Functor: member(A, [_ | X]) :- member(A, X).
;;
;;  A Prolog Functor has a head and a body.
;;  The head is a Prolog Structure. A functor is defined by it's head.
;;  The body is a Prolog Conjunct or a Prolog Disjunct.
;;  The symbol :- is called neck. It means "body => head",
;;  or "if the body is true, then the head is true."
;;

(defrecord PrologFunctor [head body])


(defn >functor< [name args body]
  (let [new-head
        (>structure<
         name
         args)
        new-body (>term< body)]
    (PrologFunctor.
     new-head
     new-body)))


(defn prolog-functor? [func]
  (same? (type func)
         logic.term.PrologFunctor))


(defn =functor=
  "It's a specific evaluation. It evaluates and returns only the body of the functor."
  [func pool]
  (=term= (:body func) pool))


(defn match-structure->functor
  [struct func pool]
  (let [[new-head new-pool]
        (unify-structures
         (:head func)
         struct
         pool)]
    (if (false? new-head)
      [false pool]
      [(=functor= func new-pool) new-pool])))


(defn generate-functor
  [func names]
  (let [[new-head new-names]
          (generate-structure
           (:head func)
           names)
        [new-body final-names]
          (generate-term
           (:body func)
           new-names)]
    [(PrologFunctor.
       new-head
       new-body)
     final-names]))


;; ============================================================================
;;  Common PrologTerm functions.
;;  They create a "polymorphism" between PrologTerms.
;;


(defn =term=
  "Recognizes the type of a term and evaluates it according to it's specific needs."
  [term pool]
  (cond

   (prolog-list? term)
     (=list= term pool)

   (prolog-structure? term)
     (=structure= term pool)

   (prolog-variable? term)
     (=variable= term pool)

   (prolog-conjunct? term)
     (=conjunct= term pool)

   (prolog-disjunct? term)
     (=disjunct= term pool)

   :else
     term))


(defn generate-term
  [term names]
  (cond
   (prolog-variable? term)
     (generate-variable term names)
   (prolog-list? term)
     (generate-list term names)
   (prolog-structure? term)
     (generate-structure term names)
   (prolog-conjunct? term)
     (generate-conjunct term names)
   (prolog-disjunct? term)
     (generate-disjunct term names)
   :else
     [term names]))


(defn unify-terms
  [term-x term-y pool]
  (cond

   (and (prolog-variable? term-x)
        (prolog-variable? term-y))
     (unify-variables term-x term-y pool)
   (prolog-variable? term-x)
     (=variable= term-x term-y pool)
   (prolog-variable? term-y)
     (=variable= term-y term-x pool)
   (different? (type term-x)
               (type term-y))
     [false pool]
   (prolog-atom? term-x)
     (unify-atoms term-x term-y pool)
   (prolog-number? term-x)
     (unify-numbers term-x term-y pool)
   (prolog-string? term-x)
     (unify-strings term-x term-y pool)
   (prolog-list? term-x)
     (unify-lists term-x term-y pool)
   (prolog-structure? term-x)
     (unify-structures term-x term-y pool)))


(defn >term<
  "Creates a term from the input."
  [inp]
  (cond
   (nil? inp)
     (throw (Exception. "One does not simply create a PrologTerm from a nil!"))
   (number? inp)
     (>number< inp)
   (vector? inp)
     (cond
      (same? :& (first inp))
        (>conjunct< inp)
      (same? :$ (first inp))
        (>disjunct< inp)
      :else
        (>list< inp))
   (string? inp)
     (>string< inp)
   (same? :_ inp)
     (PrologVariable. (keyword (gensym)))
   (a-z? inp)
     (>atom< inp)
   (A-Z? inp)
     (>variable< inp)))


(defn output-term
  [term]
  (cond
   (prolog-variable? term)
     (output-variable term)
   (prolog-atom? term)
     (output-atom term)
   (prolog-number? term)
     (output-number term)
   (prolog-string? term)
     (output-string term)
   (prolog-list? term)
     (output-list term)
   (prolog-structure? term)
     (output-structure term)
   (prolog-conjunct? term)
     (output-conjunct term)
   :else
     (str "\"" term "\"")))


(defn get-term-vars
  [term]
  (cond
   (prolog-variable? term)
     #{(:name term)}
   (prolog-list? term)
     (get-list-vars term)
   (prolog-structure? term)
     (get-structure-vars term)
   (prolog-conjunct? term)
     (get-conjunct-vars term)
   (prolog-disjunct? term)
     (get-disjunct-vars term)))
