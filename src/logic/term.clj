(ns logic.term
  [:require [logic.util :refer :all]
            [clojure.set]])


(defmulti output (fn [term _] (type term)))
(defmulti unify (fn [x y _] [(type x) (type y)]))
(defmulti generate (fn [term _] (type term)))
(defmulti get-vars type)
(defmulti reshape (fn [term _] (type term)))
(defmulti create (fn [inp & rest] (type inp)))
(defmulti resolve (fn [x y _] [(type x) (type y)]))


(defmethod unify :default
  [_ _ pool]
  [false pool])


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
    (if (or (nil? val) (set? val))
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
      (if (= PrologVariable (type real-var))
        (->>
         (bounds real-var pool)
         (reduce #(assoc %1 %2 term) (dissoc pool real-var))
         (#(assoc % (extract var pool) term))
         (vector term))
        (unify real-var term pool)))))


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
        [(create name-x) pool]
        [(create (str \' name-x \')) pool])
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
                   (rest all)
                   new-pool)))))))


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


(defn create-list [elems]
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
               (create-arguments args)))


(defmethod unify [PrologFact PrologFact]
  [fact-x fact-y pool]
  (if (-> (unify (:atom fact-x) (:atom fact-y) pool)
          (first)
          (false?))
    [false pool]
    (let [[new-args new-pool]
          (unify (:args fact-x) (:args fact-y) pool)]

      (if (false? new-args)
        [false pool]
        [(PrologFact. (:atom fact-x) new-args)
         new-pool]))))

(defmethod unify [PrologFact PrologVariable]
  [fact var pool]
  (evaluate var fact pool))

(defmethod unify [PrologVariable PrologFact]
  [var fact pool]
  (evaluate var fact pool))


(defmethod generate PrologFact
  [fact names]
  (-> (generate (:args fact) names)
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
        (unify fact-x fact-y pool)]

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


(defmethod generate PrologDisjunction
  [disj names]
  (-> (generate (PrologArguments. (:terms disj)) names)
      (#(assoc % 0 (PrologDisjunction. (-> % first :args))))))


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


(defmethod generate PrologConjunction
  [conj names]
  (-> (generate (PrologArguments. (:terms conj)) names)
      (#(assoc % 0 (PrologConjunction. (-> % first :args))))))


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


(defmethod generate PrologNegation
  [neg names]
  (-> neg :term (generate names)))


(defmethod output PrologNegation
  [neg pool]
  (-> (output (:term neg) pool)
      (#(str "not(" % ")"))))


; =================================================================================== ;
; =================================================================================== ;

; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #  Prolog Expression: 2 + 2, X is 3 * 8.                                          # ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
; #                                                                                 # ;
; #                                          is                                     # ;
; #                                         /  \                                    # ;
; #  Actualy, they look like this:  +   ,  X    *                                   # ;
; #                                / \         / \                                  # ;
; #                               2   2       3   5                                 # ;
; #                                                                                 # ;
; #  They are secial type of Prolog Facts.                                          # ;
; #  They have exactly two arguments and are used only for mathematical expressions.# ;
; #                                                                                 # ;
; # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # ;
(defrecord PrologExpression [name left right])


(defn create-expression [[left name right]]
  (PrologExpression. name (create left) (create right)))


(defmethod generate PrologExpression
  [expr names]
  (let [[new-left names-left]
        (generate (:left expr) names)

        [new-right new-names]
        (generate (:right expr) names-left)]

    [(PrologExpression. (:name expr) new-left new-right)
     new-names]))


(defmethod get-vars PrologExpression
  [expr]
  (clojure.set/union
   (get-vars (:left expr))
   (get-vars (:right expr))))


(defmethod output PrologExpression
  [expr pool]
  (str "("
       (output (:left expr) pool)
       " "
       (:name expr)
       " "
       (output (:right expr) pool)
       ")"))


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


(defmethod generate PrologFormula
  [form names]
  (-> (generate (:fact form) names)
      (#(assoc % 0 (PrologFormula. (first %) (:func form))))))


(defmethod resolve [PrologAtom PrologFormula]
  [atom form pool]
  (if (and (= 0 (-> form :fact :args :args count))
           (unify atom (-> form :fact :atom) pool))
    (do
      ((:func form) (:args form))
      [true true pool])
    [false false pool]))


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


(defmethod generate PrologRule
  [rule names]
  (let [[new-head head-names]
        (generate (:head rule) names)

        [new-body new-names]
        (generate (:body rule) head-names)]

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
   (PrologVariable. inp)

   (re-matches (re-pattern #"[a-z][a-zA-Z_]*|'[^']+'") inp)
   (PrologAtom. inp)

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

   (= key :expr)
   (create-expression rest)

   (= key :form)
   (create-formula rest)

   (= key :rule)
   (create-rule rest)

   :else
   (create-list all)))
