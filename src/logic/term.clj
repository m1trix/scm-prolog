(ns logic.term
  [:require [logic.util :refer :all]
            [clojure.set]])


(defmulti output (fn [term _] (type term)))
(defmulti unify (fn [x y _] [(type x) (type y)]))
(defmulti generate (fn [term _] (type term)))
(defmulti get-vars type)
(defmulti reshape (fn [term _] (type term)))
(defmulti create type)


(defmethod reshape :default
  [term pool]
  [term pool])


(defmethod generate :default
  [term names]
  [term names])


(defmethod get-vars :default
  [_]
  #{})


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



;; TODO
(defn prolog-variable? [var]
  (= (type var)
     logic.term.PrologVariable))


(defn extract
  "Extracts the actual value of the Variable from the pool."
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


(defmethod generate PrologVariable
  [var names]
  (let [name (:name var)
        new-name (-> '_G gensym str)
        map-name (names name)]
    (cond

     (= name "_")
     [(PrologVariable. new-name) names]

     (nil? map-name)
     [(PrologVariable. new-name)
      (assoc names name new-name)]

     :else
     [(PrologVariable. map-name) names])))


(defmethod get-vars PrologVariable
  ;; A PrologVariable holds only itself,
  ;; but random variables are not important.
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


(defn evaluate
  "Evaluates the Variable with value Term, no matter if it's already evaluated."
  [var term pool]
  (let [real-var (extract var pool)]
    (if (= term real-var)
      [var pool]
      (->>
       (bounds real-var pool)
       (reduce #(assoc %1 %2 term) (dissoc pool real-var))
       (#(assoc % (extract var pool) term))
       (vector term)))))


(defn bind
  "Binds the second Variable to the First one. All Variables previously bound to the
  second one are now directly bound to the first."
  [var-x var-y pool]
  (if (= var-x var-y)
    [var-x pool]
    (let [bounds-x (bounds var-x pool)
          bounds-y (bounds var-y pool)
          new-bounds (clojure.set/union bounds-x bounds-y)
          [_ new-pool] (evaluate var-y var-x pool)]
      [var-x (assoc pool
               var-x (conj new-bounds var-y)
               var-y var-x)])))


(defmethod unify [PrologVariable PrologVariable]
  ;; Two variables unify by agreeing to bind to each together.
  ;; That means that whenever one of them is evaluated,
  ;; the other one is also evaluated.
  [var-x var-y pool]
  (let [val-x (extract var-x pool)
        val-y (extract var-y pool)]
    (cond

     (and (prolog-variable? val-x)    ;; Both are not evaluated, so
          (prolog-variable? val-y))   ;; they bind together.
     (bind val-x val-y pool)

     (prolog-variable? val-x)
     (evaluate val-x val-y pool)

     (prolog-variable? val-y)
     (evaluate val-y val-x pool)

     :else
     (unify val-x val-y pool))))


(defmethod reshape PrologVariable
  [var pool]
  (let [val (pool var)]
    (if (or (set? val)
            (nil? val))
      [var pool]
      (let [[new-val new-pool] (reshape val pool)]
        [new-val (assoc new-pool var new-val)]))))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Atom: cat, dOG, cat_dog, 'some atom+wh@tever s&mbols//'                 # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #   It's a general-purpose name with no inherent meaning.                         # ;
; #                                                                                 # ;
; #  Atom "atom" can be unified with atom "'atom'".                                 # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologAtom [name])


;; TODO
(defn prolog-atom? [atom]
  (same? (type atom) logic.term.PrologAtom))


(defmethod unify [PrologAtom PrologAtom]
  ;; Two atoms unify if they have exactly the same names,
  ;; or one is the same placed in ' '.
  [x y pool]
  ;; Removing the ' ' and checking if the results are the same.
  (let [name-x (re-find (re-pattern #"[^']+") (:name x))
        name-y (re-find (re-pattern #"[^']+") (:name y))]
    (if (same? name-x name-y)
      (if (re-matches (re-pattern #"[a-z][a-zA-Z_]*") name-x)
        [(create-atom name-x) pool]
        [(create-atom (str \' name-x \')) pool])
      [false pool])))

(defmethod unify [PrologAtom PrologVariable]
  [atom var pool]
  (evaluate var atom pool))

(defmethod unify [PrologVariable PrologAtom]
  [var atom pool]
  (evaluate var atom pool))


(defmethod output PrologAtom
  ;; An Atom is printed as it's name.
  [atom pool]
  (:name atom))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #   Prolog Number: 1, 5.5.                                                        # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  They are just numbers.                                                         # ;
; #                                                                                 # ;
; #  Numbers can be used in mathematical expressions.                               # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologNumber [value])


(defmethod create Number
  [n]
  (PrologNumber. n))


;; TODO
(defn prolog-number? [number]
  (same? (type number)
         logic.term.PrologNumber))


(defmethod unify [PrologNumber PrologNumber]
  ;; Two Numbers unify if their values are equal.
  [x y pool]
  (if (= (:value x)
         (:value y))
    [x pool]
    [false pool]))

(defmethod unify [PrologNumber PrologVariable]
  [num var pool]
  (evaluate var num pool))

(defmethod unify [PrologVariable PrologNumber]
  [var num pool]
  (evaluate var num pool))


(defmethod output PrologNumber
  ;; A Number is printed as it's value.
  [num _]
  (str (:value num)))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog String: "string", "Tom is a cat!"                                       # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologString [string])


;; TODO
(defn prolog-string? [string]
  (same? (type string)
         logic.term.PrologString))


(defmethod unify [PrologString PrologString]
  ;; Two Strings unify if their characters are exactly the same.
  [x y pool]
  (if (= (:string x)
         (:string y))
    [x pool]
    [false pool]))

(defmethod unify [PrologString PrologVariable]
  [str var pool]
  (evaluate var str pool))

(defmethod unify [PrologVariable PrologString]
  [var str pool]
  (evaluate var str pool))


(defmethod output PrologString
  [str pool]
  (:string str))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  PrologArguments: (Var, atom, [1,2 | [3]]).                                     # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  It's a list of Prolog Terms.                                                   # ;
; #  Two Arguments lists unify if they hold the same number of Terms and each two   # ;
; #  arguments unify.                                                               # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologArguments [args])


(defn create-arguments [args]
  (PrologArguments. (mapv create args)))


;; TODO
(defn prolog-arguments? [term]
  (= (type term)
     logic.term.PrologArguments))


(defmethod unify [PrologArguments PrologArguments]
  [args-x args-y in-pool]
  (if (different? (count (:args args-x))
                  (count (:args args-y)))
    [false in-pool]

    (loop [args []
           all (mapv vector (:args args-x) (:args args-y))
           pool in-pool]
      (if (empty? all)
        [(PrologArguments. args) pool]
        (let [[term-x term-y] (first all)
              [new-term new-pool] (unify term-x term-y pool)]
          (if (false? new-term)
            [false pool]
            (recur (conj args new-term)
                   new-pool
                   (rest all))))))))


(defmethod generate PrologArguments
  ;; Returns a new Prolog Arguments list
  ;; with new names generated for the unique Variables.
  [args names]
    (let [[new-args new-names]
        (reduce
         (fn [[res names] term]
           (let [[new-term new-names]
                 (generate term names)]
             [(conj res new-term) new-names]))
         [[] names] (:args args))]
    [(PrologArguments. new-args) new-names]))


(defmethod output PrologArguments
  ;; Returns a string with the output form of the given Arguments list.
  [args pool]
  (str "("
       (when-not (empty? (:args args))
         (reduce #(str %1 ", " (output %2 pool))
                 (output (first (:args args)) pool)
                 (rest (:args args))))
       ")"))


(defmethod get-vars PrologArguments
  [args]
  (reduce #(clojure.set/union %1 (get-vars %2))
          #{}
          (:args args)))


(defmethod reshape PrologArguments
  [in-args in-pool]
  (loop [args []
         all (:args in-args)
         pool in-pool]
    (if (empty? all)
      [(PrologArguments. args) pool]
    (let [[new-term new-pool] (reshape (first all) pool)]
      (recur (conj args new-term)
             (rest all)
             new-pool)))))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog List: [A, B, C]. [A | X]. [_, A | 3]. [1 | _].                          # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  List of Prolog Temrs - the elementss can be anything.                          # ;
; #                                                                                 # ;
; #  They have a head [1, 2 and a tail | X], where the head contains elemens and    # ;
; #  the tail is another list (could be an empty one) or a variable.                # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologList [head tail])


(defmethod create clojure.lang.PersistentVector
  [elems]
  (loop [head []
         all elems]
    (if (empty? all)
      (PrologList. head [])
      (let [elem (first all)]
        (if (= :| elem)
          (PrologList. head (-> all second create))
          (recur (conj head (create elem))
                 (rest all)))))))


;; TODO
(defn prolog-list? [list]
  (same? (type list)
         logic.term.PrologList))


(defn reconstruct-list
  "Recreates the same List with a simpler structure (if possible)."
  [list pool]
  (if (empty? (:head list))

    (if (= [] (:tail list))
      (PrologList. [] [])
      (:tail list))

    (cond

     (= PrologList (-> list :tail type))
     (->
      (reconstruct-list (:tail list) pool)
      (#(PrologList. (vec (concat (:head list)
                                  (:head %)))
                     (:tail %))))

     (= PrologVariable (-> list :tail type))
     (let [new-tail (extract (:tail list) pool)]
       (if (= PrologVariable (type new-tail))
         list
         (->
          (PrologList. (:head list) new-tail)
          (reconstruct-list pool))))

     :else list)))


(defmethod unify [PrologList PrologList]
  ;; Two lists unify if their initial elements unify
  ;; and the lists which remain after removing both initial elements unify.
  [list-x list-y in-pool]
  (cond

   (and (= [] (:head list-x))
        (= [] (:head list-y)))
   [list-x in-pool]

   (or (= [] (:head list-x))
       (= [] (:head list-y)))
   [false in-pool]

   :else
   (loop [head-x (:head list-x)
          head-y (:head list-y)
          new-head []
          pool in-pool]
     (if (or (empty? head-x)
             (empty? head-y))
       (let [[new-tail new-pool]
             (unify
              (reconstruct-list
               (PrologList. head-x (:tail list-x))
               pool)
              (reconstruct-list
               (PrologList. head-y (:tail list-y))
               pool)
              pool)]
         (if (false? new-tail)
           [false in-pool]
           [(reconstruct-list (PrologList. new-head new-tail) new-pool)
            new-pool]))
       (let [[new-term new-pool] (unify (first head-x) (first head-y) pool)]
         (if (false? new-term)
           [false in-pool]
           (recur (rest head-x)
                  (rest head-y)
                  (conj new-head new-term)
                  new-pool)))))))

(defmethod unify [PrologList PrologVariable]
  [list var pool]
  (evaluate var list pool))

(defmethod unify [PrologVariable PrologList]
  [var list pool]
  (evaluate var list pool))


(defmethod get-vars PrologList
  [list]
  (reduce #(clojure.set/union %1 (get-vars %2))
          (if (= [] (:tail list))
            #{}
            (get-vars (:tail list)))
          (:head list)))


(defmethod generate PrologList
  [list names]
  (let [[new-head head-names]
        (->
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


(defmethod output PrologList
  [list pool]
  (str "["
       (when-not (= [] (:head list))
         (reduce #(str %1 ", " (output %2 pool))
                 (output (first (:head list)) pool)
                 (rest (:head list))))
       (when-not (= [] (:tail list))
         (str " | "
              (output (:tail list) pool)))
       "]"))


(defmethod reshape PrologList
  [list pool]
  (let [[new-head head-pool]
        (reshape (-> list :head PrologArguments.)
                 pool)]
    (if (= [] (:tail list))
      [(PrologList. (:args new-head) [])
       head-pool]
      (let [[new-tail tail-pool]
            (-> list :tail (reshape head-pool))]
        [(reconstruct-list
          (-> new-head :args (PrologList. new-tail))
          tail-pool)
         tail-pool]))))


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
;;


(defmethod create String
  [inp]
  (cond

   (re-matches (re-pattern #"\"[^\"]+\"") inp)
   (PrologString. inp)

   (re-matches (re-pattern #"[A-Z][0-9A-Za-z_]*|[_]") inp)
   (PrologVariable. inp)

   (re-matches (re-pattern #"[a-z][a-zA-Z_]*|'[^']+'") inp)
   (PrologAtom. inp)

   :else
   (throw (Exception. (str "Unable to create a PrologTerm from " inp "!")))))
