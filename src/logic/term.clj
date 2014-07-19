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
(def get-term-variables)

(defprotocol TermInterface
  (unify [x y pool])
  (output [term])
  (generate [term names])
  (get-variables [term]))


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
;;  Prolog Atoms: cat, dOG, cat_dog, 'some atom+wh@tever s&mbols'
;;
;;  It's a general-purpose name with no inherent meaning.
;;
;;  Atom "atom" can be unified with atom "'atom'".
;;
(defrecord PrologAtom [name])


(defn >atom< [name]
  (if (re-matches (re-pattern #"[a-z][a-zA-Z_]*|'[^']+'") name)
    (PrologAtom. name)
    (throw (Exception. (str name " is invalid PrologAtom name!")))))


(defn prolog-atom? [atom]
  (same? (type atom) logic.term.PrologAtom))


(extend-protocol TermInterface
  logic.term.PrologAtom

  (unify
   [x y pool]
   (if (prolog-atom? y)
     (let [name-x (re-find (re-pattern #"[^']+") (:name x))
           name-y (re-find (re-pattern #"[^']+") (:name y))]
       (if (same? name-x name-y)
         [(>atom< name-x) pool]
         [false pool]))
     [false pool]))

  (generate [atom names]
    [atom names])

  (output [atom]
    (:name atom))

  (get-variables [atom] #{}))



;; ===========================================================================
;;  Prolog Numbers: 1, 5.5.
;;  They are just numbers.
;;
(defrecord PrologNumber [value])


(defn >number< [n]
  (if (number? n)
    (PrologNumber. n)
    (throw (Exception. (str n " is illegal PrologNumber value!")))))


(defn prolog-number? [number]
  (same? (type number) logic.term.PrologNumber))


(extend-protocol TermInterface
  logic.term.PrologNumber

  (unify
   [x y pool]
   (if (and (prolog-number? y)
            (same? (:value x) (:value y)))
     [x pool]
     [false pool]))

  (generate [num names]
    [num names])

  (output [num]
    (str (:value num)))

  (get-variables [num] #{}))



;; ============================================================================
;;  Prolog String: "string", "Tom is a cat!"
;;
(defrecord PrologString [string])


(defn >string< [s]
  (if (string? s)
    (PrologString. s)
    (throw (Exception. (str s " is illegal PrologString value.")))))


(defn prolog-string? [string]
  (same? (type string) logic.term.PrologString))

(extend-protocol TermInterface
  logic.term.PrologString

  (unify [x y pool]
    (if (and (prolog-string? y)
             (same? (:string x)
                    (:string y)))
      [x pool]
      [false pool]))

  (generate [s names]
    [s names])

  (output [s]
    (str "\"" (:string s) "\""))

  (get-variables [s] #{}))



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




;; ===========================================================================
;;  PrologArguments: (Var, atom, [1,2 | [3]]).
;;
(defrecord PrologArguments [args])


(defn >arguments< [args]
  (PrologArguments. (mapv >term< args)))


(defn =arguments= [args pool]
  (PrologArguments. (mapv #(=term= % pool)
                          (:args args))))


(defn generate-arguments [args names]
  (let [[new-args new-names]
        (reduce
         (fn [[res names] term]
           (let [[new-term new-names]
                 (generate-term term names)]
             [(conj res new-term) new-names]))
         [[] names] (:args args))]
    [(PrologArguments. new-args) new-names]))


(defn unify-arguments [args-x args-y pool]
  (let [elems-x (:args args-x)
        elems-y (:args args-y)]
    (if (different? (count elems-x)
                    (count elems-y))
      [false pool]
      (let [[new-args new-pool]
             (->>
              (mapv #(vector %1 %2) elems-x elems-y)
              (reduce
               (fn [[res pool] [term-x term-y]]
                 (if (false? res)
                   [false pool]
                   (let [[new-term new-pool] (unify-terms term-x term-y pool)]
                     (if (false? new-term)
                       [false pool]
                       [(conj res new-term)
                        new-pool]))))
               [[] pool]))]
        [(PrologArguments. new-args)
         new-pool]))))


(extend-protocol TermInterface
  logic.term.PrologArguments

  (unify [args-x args-y pool]
    (if (different? (count (:args args-x))
                    (count (:args args-x)))
      [false pool]
      (->>
       (mapv vector
             (:args args-x)
             (:args args-y))
       ))))

(extend-protocol TermInterface
  logic.term.PrologArguments

  (output [x]
    (str "("
         (when-not (empty? (:args x))
           (reduce #(str %1 ", " (output %2))
                   (output (first (:args x)))
                   (rest (:args x))))
         ")"))

  (get-variables [args]
    (reduce #(clojure.set/union %1
              (get-variables %2))
            #{}
            (:args args))))



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

(defn refactor-list
  [list]
  (if (prolog-list? (:tail list))
    (let [new-list (refactor-list (:tail list))]
      (PrologList. (vec (concat (:head list)
                                (:head new-list)))
                   (:tail new-list)))
    list))


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
     (->>
      (min (count (:head list-x))
           (count (:head list-y)))
      (#(vector (subvec (:head list-x) 0 %)
                (subvec (:head list-y) 0 %)))
      (#(unify (PrologArguments. (first %1))
               (PrologArguments. (second %1))
               pool))
      (#([(PrologList. (first %) []) (second %)])))))


(defn =list=
  "Replaces all of the list's variables with thieir values from the pool."
  [list pool]
  (let [new-head (mapv #(=term= % pool) (:head list))]
    (if (same? [] (:tail list))
      (refactor-list (PrologList. new-head []))
      (refactor-list (PrologList. new-head (=term= (:tail list)
                                    pool))))))


(extend-protocol TermInterface
  logic.term.PrologList

  (unify [x y pool]
    (let [size (min (count (:head x))
                    (count (:head y)))

          [new-head head-pool]
          (->>
           (mapv #(vector %1 %2)
                 (subvec (:head x) 0 size)
                 (subvec (:head y) 0 size))
           (reduce (fn [[head pool] [elem-x elem-y]]
                     (if (false? head)
                       [head pool]
                       (let [[new-elem new-pool]
                             (unify elem-x elem-y pool)]
                         (if (false? new-elem)
                           [false pool]
                           [(conj head new-elem)
                            new-pool]))))
                  [[] pool]))]
      (if (false? new-head)
        [false pool]
        (let [[new-tail tail-pool]
              (unify (PrologList.
                      (subvec (:head x)
                              size)
                      (:tail x))

                     (PrologList.
                      (subvec (:head y)
                              size)
                      (:tail y))

                     head-pool)]
          (if (false? new-tail)
            [false pool]
            [(refactor-list
              (PrologList. new-head
                           new-tail))
             tail-pool]))))))


(extend-protocol TermInterface
  logic.term.PrologList

  (output [list]
    (let [head (:head list)
          tail (:tail list)]
      (str "["
           (when-not (empty? head)
             (reduce #(str %1 ", " (output %2))
                     (output (first head))
                     (rest head)))
           (when-not (empty? tail)
             (str " | "(output tail)))
           "]"))))


(extend-protocol TermInterface
  logic.term.PrologList

  (generate [list names]
    (let [[new-head head-names]
          (reduce (fn [[res names] elem]
                    (let [[new-elem new-names]
                          (generate elem names)]
                      [(conj res new-elem)
                       new-names]))
                  [[] names]
                  (:head list))]
      (if (= [] (:tail list))
        [(PrologList. new-head [])
         head-names]
        (let [[new-tail tail-names]
              (generate (:tail list)
                        head-names)]
          [(PrologList. new-head new-tail)
           tail-names])))))


(extend-protocol TermInterface
  logic.term.PrologList

  (get-variables [list]
    (reduce #(clojure.set/union
              %1
              (get-variables %2))
            (if (= [] (:tail list))
              #{}
              (get-variables (:tail list)))
            (:head list))))



;; ===========================================================================
;;  Prolog Fact: cat(tom). member(X, Y).
;;  Each fact has a name and arguments.
;;  The name is a Prolog Atom.
;;
(defrecord PrologFact [atom args])


(defn >fact< [[sign atom args]]
  (if (same? sign :%fact%)
    (PrologFact. (>atom< atom)
                 (>arguments< args))
    (throw (Exception. "To create a PrologFact from a vector, it must start with a :%fact%"))))


(defn prolog-fact? [term]
  (same? (type term)
         logic.term.PrologFact))


(defn =fact= [fact pool]
  (PrologFact. (:atom fact)
               (=arguments= (:args fact)
                            pool)))

(defn generate-fact [fact names]
  (generate-arguments (:args fact)
                      names))


(defn unify-facts [fact-x fact-y pool]
  (let [[new-name _] (unify-atoms (:atom fact-x)
                                  (:atom fact-y)
                                  pool)]
    (if (false? new-name)
      [false pool]
      (let [[new-args new-pool] (unify-arguments (:args fact-x)
                                                 (:args fact-y)
                                                 pool)]
        (if (false? new-args)
          [false pool]
          [(PrologFact. new-name new-args)
           new-pool])))))


(defn get-fact-variables
  [fact]
  (get-arguments-variables (:args fact)))


(extend-protocol TermInterface
  logic.term.PrologFact


  (output [x]
    (str (output-atom (:atom x))
         (output-arguments (:args x)))))



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

   (prolog-fact? term)
     (=fact= term pool)

   (prolog-variable? term)
     (=variable= term pool)

   (prolog-conjunct? term)
     (=conjunct= term pool)

   (prolog-disjunct? term)
     (=disjunct= term pool)

   (prolog-method? term)
     (=method= term pool)

   (prolog-math? term)
     (=math= term pool)

   :else
     term))


(defn generate-term
  [term names]
  (cond

   (prolog-variable? term)
     (generate-variable term names)

   (prolog-list? term)
     (generate-list term names)

   (prolog-fact? term)
     (generate-fact term names)

   (prolog-rule? term)
     (generate-rule term names)

   (prolog-conjunct? term)
     (generate-conjunct term names)

   (prolog-disjunct? term)
     (generate-disjunct term names)

   (prolog-method? term)
     (generate-method term names)

   (prolog-math? term)
     (generate-math term names)
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
   (prolog-fact? term-x)
     (unify-facts term-x term-y pool)
   (prolog-method? term-x)
     (unify-methods term-x term-y pool)))


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
      (same? :#con (first inp))
        (>conjunct< inp)
      (same? :#dis (first inp))
        (>disjunct< inp)
      (same? :%fact% (first inp))
        (>fact< inp)
      (same? :%rule% (first inp))
        (>rule< inp)
      (same? :#met (first inp))
        (>method< inp)
      (same? :#math (first inp))
        (>math< inp)
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

   (prolog-fact? term)
     (output-fact term)

   (prolog-conjunct? term)
     (output-conjunct term)

   (prolog-disjunct? term)
     (output-disjunct term)

   (prolog-method? term)
     (output-method term)

   (prolog-math? term)
     (output-math term)

   :else
     term))


(defn get-term-variables
  "Returns a set of the variables, used by the given term.
  If the term uses no variables, it's logical to return an empty set."
  [term]
  (cond

   (prolog-variable? term)
     #{(:name term)}

   (prolog-list? term)
     (get-list-variables term)

   (prolog-fact? term)
     (get-fact-variables term)

   (prolog-conjunct? term)
     (get-conjunct-variables term)

   (prolog-disjunct? term)
     (get-disjunct-variables term)

   (prolog-method? term)
     (get-method-variables term)

   (prolog-math? term)
     (get-math-variables term)

   :else
     #{}))
