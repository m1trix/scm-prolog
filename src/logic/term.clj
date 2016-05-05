(ns logic.term
  (:use logic.util
        logic.env)
  (:require [clojure.set])
  (:refer-clojure :exclude [resolve]))


(defprotocol ITerm
  (to-string [this env])
  (generate [this pool])
  (unify [this other env]))


(declare create)


(load "term/variable")
(load "term/atom")
(load "term/tuple")


(defmulti output (fn [term _] (type term)))
(defmulti obsolete-unify (fn [x y _] [(type x) (type y)]))
(defmulti obsolete-generate (fn [term _] (type term)))
(defmulti get-vars type)
(defmulti reshape (fn [term _] (type term)))
(defmulti create (fn [inp & rest] (type inp)))
(defmulti resolve (fn [x y _] [(type x) (type y)]))
(defmulti get-name (fn [term] (type term)))


(defmethod obsolete-unify :default
  [_ _ pool]
  [false pool])


(defmethod reshape :default
  [term pool]
  [term pool])


(defmethod obsolete-generate :default
  [term names]
  [term names])


(defmethod get-vars :default
  [_]
  #{})


(defmethod output :default
  [what _]
  (str what))


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
(defn extract
  "Extracts the actual value of the Variable from the pool."
  [var pool]
  (let [val (pool var)]
    (if (or (nil? val) (set? val))
      var
      (if (variable? val)
        (extract val pool)
        val))))


(defn bounds
  "Returns a set of all Variables bound together with the given one."
  [var pool]
  (let [val (pool var)]
    (cond
      (set? val) val
      (variable? val) (bounds val pool)
      :else #{})))


(defmethod obsolete-generate Variable
  [var names]
  (let [name (:name var)
        new-name (-> '_G gensym str)
        map-name (names name)]
    (cond

     (= name "_")
     [(Variable. new-name) names]

     (nil? map-name)
     [(Variable. new-name)
      (assoc names name new-name)]

     :else
     [(Variable. map-name) names])))


(defmethod get-vars Variable
  ;; A Variable holds only itself,
  ;; but random variables are not important.
  [var]
  (if (= (:name var) "_")
    #{}
    #{var}))


(defmethod output Variable
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
      (if (= Variable (type real-var))
        (->>
         (bounds real-var pool)
         (reduce #(assoc %1 %2 term) (dissoc pool real-var))
         (#(assoc % (extract var pool) term))
         (vector term))
        (obsolete-unify real-var term pool)))))


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


(defmethod obsolete-unify [Variable Variable]
  ;; Two variables unify by agreeing to bind to each together.
  ;; That means that whenever one of them is evaluated,
  ;; the other one is also evaluated.
  [var-x var-y pool]
  (let [val-x (extract var-x pool)
        val-y (extract var-y pool)]
    (cond

     (and (variable? val-x)    ;; Both are not evaluated, so
          (variable? val-y))   ;; they bind together.
     (bind val-x val-y pool)

     (variable? val-x)
     (evaluate val-x val-y pool)

     (variable? val-y)
     (evaluate val-y val-x pool)

     :else
     (obsolete-unify val-x val-y pool))))


(defmethod reshape Variable
  [var pool]
  (let [val (pool var)]
    (if (or (set? val)
            (nil? val))
      [var pool]
      (let [[new-val new-pool] (reshape val pool)]
        [new-val (assoc new-pool var new-val)]))))


; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Atom: cat, dOG, cat_dog, 'some atom+wh@tever s&mbols//'                 # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;

(defmethod obsolete-unify [Atom Atom]
  [x y pool]
  (.unify x y pool))

(defmethod obsolete-unify [Atom Variable]
  [atom var pool]
  (evaluate var atom pool))

(defmethod obsolete-unify [Variable Atom]
  [var atom pool]
  (evaluate var atom pool))

(defmethod output Atom
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


(defmethod obsolete-unify [PrologNumber PrologNumber]
  ;; Two Numbers unify if their values are equal.
  [x y pool]
  (if (= (:value x)
         (:value y))
    [x pool]
    [false pool]))

(defmethod obsolete-unify [PrologNumber Variable]
  [num var pool]
  (evaluate var num pool))

(defmethod obsolete-unify [Variable PrologNumber]
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


(defmethod obsolete-unify [PrologString PrologString]
  ;; Two Strings unify if their characters are exactly the same.
  [x y pool]
  (if (= (:string x)
         (:string y))
    [x pool]
    [false pool]))

(defmethod obsolete-unify [PrologString Variable]
  [str var pool]
  (evaluate var str pool))

(defmethod obsolete-unify [Variable PrologString]
  [var str pool]
  (evaluate var str pool))


(defmethod output PrologString
  [str pool]
  (:string str))


; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                         # ;
; #  Tuple: (Var, atom, [1,2 | [3]]).                                       # ;
; #                                                                         # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;

(defmethod obsolete-unify [Tuple Tuple]
  [args-x args-y in-pool]
  (if (different? (count (:terms args-x))
                  (count (:terms args-y)))
    [false in-pool]

    (loop [args []
           all (mapv vector (:terms args-x) (:terms args-y))
           pool in-pool]
      (if (empty? all)
        [(Tuple. args) pool]
        (let [[term-x term-y] (first all)
              [new-term new-pool] (obsolete-unify term-x term-y pool)]
          (if (false? new-term)
            [false pool]
            (recur (conj args new-term)
                   (rest all)
                   new-pool)))))))


(defmethod obsolete-generate Tuple
  ;; Returns a new Prolog Arguments list
  ;; with new names obsolete-generated for the unique Variables.
  [args names]
    (let [[new-args new-names]
        (reduce
         (fn [[res names] term]
           (let [[new-term new-names]
                 (obsolete-generate term names)]
             [(conj res new-term) new-names]))
         [[] names] (:terms args))]
    [(Tuple. new-args) new-names]))


(defmethod output Tuple
  ;; Returns a string with the output form of the given Arguments list.
  [args pool]
  (str "("
       (when-not (empty? (:terms args))
         (reduce #(str %1 ", " (output %2 pool))
                 (output (first (:terms args)) pool)
                 (rest (:terms args))))
       ")"))


(defmethod get-vars Tuple
  [args]
  (reduce #(clojure.set/union %1 (get-vars %2))
          #{}
          (:terms args)))


(defmethod reshape Tuple
  [in-args in-pool]
  (loop [args []
         all (:terms in-args)
         pool in-pool]
    (if (empty? all)
      [(Tuple. args) pool]
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


(defn create-list [elems]
  (loop [head []
         all elems]
    (if (empty? all)
      (PrologList. head [])
      (let [elem (first all)]
        (if (= "|" elem)
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

     (= Variable (-> list :tail type))
     (let [new-tail (extract (:tail list) pool)]
       (if (= Variable (type new-tail))
         list
         (->
          (PrologList. (:head list) new-tail)
          (reconstruct-list pool))))

     :else list)))


(defmethod obsolete-unify [PrologList PrologList]
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
             (obsolete-unify
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
       (let [[new-term new-pool] (obsolete-unify (first head-x) (first head-y) pool)]
         (if (false? new-term)
           [false in-pool]
           (recur (rest head-x)
                  (rest head-y)
                  (conj new-head new-term)
                  new-pool)))))))

(defmethod obsolete-unify [PrologList Variable]
  [list var pool]
  (evaluate var list pool))

(defmethod obsolete-unify [Variable PrologList]
  [var list pool]
  (evaluate var list pool))


(defmethod get-vars PrologList
  [list]
  (reduce #(clojure.set/union %1 (get-vars %2))
          (if (= [] (:tail list))
            #{}
            (get-vars (:tail list)))
          (:head list)))


(defmethod obsolete-generate PrologList
  [list names]
  (let [[new-head head-names]
        (->
         (Tuple. (:head list))
         (obsolete-generate names)
         (#(vector (:terms (first %))
                   (second %))))]
    (if (= [] (:tail list))
      [(PrologList. new-head [])
       head-names]
      (let [[new-tail tail-names] (obsolete-generate (:tail list)
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
        (reshape (-> list :head Tuple.)
                 pool)]
    (if (= [] (:tail list))
      [(PrologList. (:terms new-head) [])
       head-pool]
      (let [[new-tail tail-pool]
            (-> list :tail (reshape head-pool))]
        [(reconstruct-list
          (-> new-head :terms (PrologList. new-tail))
          tail-pool)
         tail-pool]))))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Fact: cat(tom). member(X, Y).                                           # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Each fact has a name and arguments.                                            # ;
; #  The name is a Prolog Atom.                                                     # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologFact [atom args])


(defn create-fact
  [[atom args]]
  (PrologFact. (create atom)
               (create-tuple args)))

(defmethod get-name PrologFact
  [fact]
  (-> fact :atom :name))


(defmethod obsolete-unify [PrologFact PrologFact]
  [fact-x fact-y pool]
  (if (-> (obsolete-unify (:atom fact-x) (:atom fact-y) pool)
          (first)
          (false?))
    [false pool]
    (let [[new-args new-pool]
          (obsolete-unify (:args fact-x) (:args fact-y) pool)]

      (if (false? new-args)
        [false pool]
        [(PrologFact. (:atom fact-x) new-args)
         new-pool]))))

(defmethod obsolete-unify [PrologFact Variable]
  [fact var pool]
  (evaluate var fact pool))

(defmethod obsolete-unify [Variable PrologFact]
  [var fact pool]
  (evaluate var fact pool))


(defmethod obsolete-generate PrologFact
  [fact names]
  (-> (obsolete-generate (:args fact) names)
      (#(assoc % 0 (PrologFact. (:atom fact) (first %))))))


(defmethod get-vars PrologFact
  [fact]
  (get-vars (:args fact)))


(defmethod output PrologFact
  [fact pool]
  (str (output (:atom fact) pool)
       (output (:args fact) pool)))


(defmethod reshape PrologFact
  [fact pool]
  (-> (reshape (:args fact) pool)
      (#(assoc % 0 (PrologFact. (:atom fact) (first %))))))


(defmethod resolve [PrologFact PrologFact]
  [fact-x fact-y pool]
  (let [[new-fact new-pool]
        (obsolete-unify fact-x fact-y pool)]

    (if (false? new-fact)
      [false false pool]
      [true new-fact new-pool])))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Disjunction: member(L, [1,2]); member(L, [2,3]).                        # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  This is logical "or". A Disjunction is proved false if and only if all its     # ;
; #  Terms are false.                                                               # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologDisjunction [terms])


(defn create-disjunction [terms]
  (PrologDisjunction. (mapv create terms)))



;; TODO
(defn prolog-disjunction? [term]
  (same? (type term)
         logic.term.PrologDisjunction))


(defmethod get-vars PrologDisjunction
  [disj]
  (reduce #(clojure.set/union %1 (get-vars %2))
          #{}
          (:terms disj)))


(defmethod obsolete-generate PrologDisjunction
  [disj names]
  (-> (obsolete-generate (Tuple. (:terms disj)) names)
      (#(assoc % 0 (PrologDisjunction. (-> % first :terms))))))


(defmethod output PrologDisjunction
  [disj pool]
  (str "("
       (->
        (reduce #(str %1 "; " (output %2 pool))
                ""
                (:terms disj))
        (subs 2))
       ")"))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Conjunction: member(L, [1,2]); member(L, [2,3]).                        # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  This is logical "and". A Conjunction is proved true if and only if all its     # ;
; #  Terms are true.                                                                # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologConjunction [terms])


(defn create-conjunction [terms]
  (PrologConjunction. (mapv create terms)))


(defmethod get-vars PrologConjunction
  [conj]
  (reduce #(clojure.set/union %1 (get-vars %2))
          #{}
          (:terms conj)))


(defmethod obsolete-generate PrologConjunction
  [conj names]
  (-> (obsolete-generate (Tuple. (:terms conj)) names)
      (#(assoc % 0 (PrologConjunction. (-> % first :terms))))))


(defmethod reshape PrologConjunction
  [conj pool]
  (-> (:terms conj)
      (Tuple.)
      (reshape pool)
      (#(assoc % 0 (PrologConjunction. (-> % first :terms))))))


(defmethod output PrologConjunction
  [conj pool]
  (str "("
       (->
        (reduce #(str %1 ", " (output %2 pool))
                ""
                (:terms conj))
        (subs 2))
       ")"))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Negation: not(member(1, [2,3])).                                        # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  This is logical "not". A Negation is proved true if and only if its Term is    # ;
; #  proved false (and the other way around).                                       # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologNegation [term])


(defn create-negation [[term]]
  (PrologNegation. (create term)))


(defmethod get-vars PrologNegation
  [neg]
  (-> neg :term get-vars))


(defmethod obsolete-generate PrologNegation
  [neg names]
  (-> neg :term (obsolete-generate names)))


(defmethod output PrologNegation
  [neg pool]
  (-> (output (:term neg) pool)
      (#(str "not(" % ")"))))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Formula: random(Base, Max, Number).                                     # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  A Prolog Formula is a special type of Prolog Fact, that executes               # ;
; #  a function when it is resolved.                                                # ;
; #  The user cannot create Formulas, they are only built-in.                       # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologFormula [fact func])


(defn create-formula [[name args func]]
  (PrologFormula. (create-fact [name args])
                  func))


(defmethod obsolete-generate PrologFormula
  [form names]
  (-> (obsolete-generate (:fact form) names)
      (#(assoc % 0 (PrologFormula. (first %) (:func form))))))


(defmethod resolve [Atom PrologFormula]
  [atom form pool]
  (if (and (= 0 (-> form :fact :args :terms count))
           (obsolete-unify atom (-> form :fact :atom) pool))
    (let [[new-term new-pool] ((:func form) (-> form :fact :args) pool)]
      [true new-term new-pool])
    [false false pool]))


(defmethod resolve [PrologFact PrologFormula]
  [fact form pool]
  (let [[status new-fact new-pool] (resolve fact (:fact form) pool)]
    (if (true? status)
      (let [[new-term new-pool] ((:func form) (-> new-fact :args :terms) pool)]
        (if (false? new-term)
          [false false pool]
          [true new-term new-pool]))
      [false false pool])))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Rule: member(A, [_ | X]) :- member(A, X).                               # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  A Prolog Rule has a head and a body.                                           # ;
; #  The head is a Prolog Structure. A Rule is defined by it's head.                # ;
; #  The body is a Term: Fact, Conjunction or Disjunction.                          # ;
; #  The symbol :- is called neck. It means "body => head",                         # ;
; #  or "if the body is true, then the head is true."                               # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologRule [head body])


(defn create-rule [[name args body]]
  (let [new-head (create-fact [name args])
        new-body (create body)]
    (PrologRule. new-head new-body)))


(defmethod get-name PrologRule
  [rule]
  (-> rule :head :atom :name))


(defmethod obsolete-generate PrologRule
  [rule names]
  (let [[new-head head-names]
        (obsolete-generate (:head rule) names)

        [new-body new-names]
        (obsolete-generate (:body rule) head-names)]

    [(PrologRule. new-head new-body)
     new-names]))


(defmethod reshape PrologRule
  [rule pool]
  (let [[new-head head-pool]
        (reshape (:head rule) pool)

        [new-body body-pool]
        (reshape (:body rule) head-pool)]

    [(PrologRule. new-head new-body)
     body-pool]))


(defmethod output PrologRule
  [rule pool]
  (str (output (:head rule) pool)
       " :- "
       (output (:body rule) pool)))


(defmethod resolve [PrologFact PrologRule]
  [fact rule pool]
  (let [[status new-fact new-pool]
        (resolve fact (:head rule) pool)

        [new-body final-pool]
        (reshape (:body rule) new-pool)]

    (if (false? status)
      [false false pool]
      [:unresolved
       (PrologRule. new-fact new-body)
       final-pool])))


; =================================================================================== ;
; =================================================================================== ;


(defmethod create String
  [inp]
  (cond

   (re-matches (re-pattern #"\"[^\"]+\"") inp)
   (PrologString. inp)

   (re-matches (re-pattern #"[A-Z][0-9A-Za-z_]*|[_]") inp)
   (create-var inp)

   (re-matches (re-pattern #"[a-z][a-zA-Z_]*|'[^']+'") inp)
   (create-atom inp)

   :else
   (throw (Exception. (str "Unable to create a PrologTerm from " inp "!")))))


(defmethod create clojure.lang.PersistentVector
  [[key & rest :as all]]
  (cond

   (= key :fact)
   (create-fact rest)

   (= key :disj)
   (create-disjunction rest)

   (= key :conj)
   (create-conjunction rest)

   (= key :not)
   (create-negation rest)

   (= key :form)
   (create-formula rest)

   (= key :rule)
   (create-rule rest)

   :else
   (create-list all)))
