;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logic.term-old
  [:require [logic.util :refer :all]])


(def unify)
(def generate)
(def print-term)

;; ===========================================================================
;;  Prolog Atoms: cat. dog.
;;  They are more like facts - Prolog treats them as a TRUE value.
;;  An Atom name always starts with a small letter.
;;
(defrecord PL-Atom [name])

(defn pl-atom? [x]
  (= (type x) logic.term.PL-Atom))

(defn -->atom [key]
  (PL-Atom. key))

(defn unify-atoms
  "Two atoms unify if they are the same."
  [x y pool]
  (if (same? (:name x) (:name y))
    [x pool]
    [false pool]))

(defn print-atom
  "Prints an PL-Atom"
  [atom]
  (let [output-atom (->
                     (str (:name atom))
                     (subs 1))]
    (print-blue output-atom)))


;; ===========================================================================
;;  Prolog Numbers: 1. 5.5.
;;  They are just numbers.
;;

(defrecord PL-Number [value])

(defn -->number [n]
  (PL-Number. n))

(defn pl-number? [x]
  (= (type x) logic.term.PL-Number))

(defn unify-numbers
  "Two numbers unify if they have the same value."
  [x y pool]
  (if (same? (:value x) (:value y))
    [x pool]
    [false pool]))

;; ===========================================================================
;;  Prolog Structures: cat(tom). member(X, Y).
;;  Each structure has a name and arguments.
;;
(defrecord PL-Structure [name args])

(defn pl-structure? [x]
  (= (type x) logic.term.PL-Structure))

(declare -->arg)

(defn -->args [arg-list]
  (mapv -->arg arg-list))

(defn -->structure [name args]
  (let [args-new (-->args args)]
    (PL-Structure. name args-new)))

(defn unify-args
  [x-args y-args pool]
  (loop [res []
         old-x x-args
         old-y y-args
         new-pool pool]
    (if (empty? old-x)
      [res new-pool]
      (let [[new-elem newer-pool] (unify (first old-x) (first old-y) new-pool)]
        (if (false? new-elem)
          [false pool]
          (recur (conj res new-elem)
                 (rest old-x)
                 (rest old-y)
                 newer-pool))))))


(defn unify-structures
  "Two structures unify if they have the same name and arity, and each pair of respective arguments unify."
  [x y pool]
  (if (different? (:name x) (:name y))
    [false pool]
    (if (different? (arity x) (arity y))
      [false pool]
      (let [[new-args new-pool] (unify-args (:args x)
                                            (:args y)
                                            pool)]
        (if (false? new-args)
          false
          [(PL-Structure. (:name x) new-args) new-pool])))))


(defn generate-structure
  "Generates a structure with random variable names, based on the original names."
  [struct old-names]
  (loop [new-args []
         old-args (:args struct)
         names old-names]
    (if (empty? old-args)
      [(PL-Structure. (:name struct) new-args) names]
      (let [[new-elem new-names] (generate (first old-args) names)]
        (recur (conj new-args new-elem)
               (rest old-args)
               new-names)))))

;; ============================================================================
;;  Prolog Variable: X. Whatever.
;;  They do not have a value at their creation.
;;  Once they are evaluated, they cannot be changed.
;;  A Variable name always starts with a capital letter.
;;  The uni-list is a set, that holds all other variables, that are bound to the current one.
;;
(defrecord PL-Variable [name value binds])

(defn -->variable
  ([name]
   (-->variable name nil []))
  ([name val]
   (-->variable name val []))
  ([name val binds]
    (PL-Variable. name val binds)))

(defn generate-variable [var all-names]
  (let [mapped-name (get all-names (:name var))]
    (if (same? mapped-name nil)
      (let [new-name (keyword (gensym))]
        [(-->variable new-name) (assoc all-names (:name var) new-name)])
      [(-->variable mapped-name) all-names])))

(defn pl-variable? [x]
  (= (type x) logic.term.PL-Variable))



;; ============================================================================
;;  Prolog String: 'str1'
;;  tom. and 'tom'. can be unified.
;;
(defrecord PL-String [word])

(defn -->string [s]
  (PL-String. s))

(defn pl-string? [x]
  (= (type x) logic.term.PL-String))

(defn unify-strings
  "Two strings unify if and only if they have precisely the same characters in them."
  [x y]
  (if (= (:word x) (:word y))
    x
    false))



;; ============================================================================
;;  Prolog List: [A, B, C]. [A | X]. [_ | 3]. [1 | _].
;;  List of Prolog Temrs - the elementss can be anything.
;;  They have a head [1, 2 and a tail | X], where the head
;;    contains elemens and the tail is another list (could be and empty one).
;;
(defrecord PL-List [head tail])

(defn -->head [vec]
  (loop [res [] v vec]
    (if (empty? v)
      [res v]
      (let [x (first v)]
        (if (same? x :|)
          [res (rest v)]
          (recur (conj res (-->arg x))
                 (rest v)))))))

(defn -->tail [vec]
  (if (empty? vec)
    nil
    (-->arg (first vec))))

(defn -->list [vec]
  (let [[head remain] (-->head vec)]
    (PL-List. head (-->tail remain))))

(defn unify-lists
  "Two lists unify if their initial elements unify, and the lists which remain after removing both initial elements unify."
  [x y pool]
  (loop [new-head []
         head-x (:head x)
         head-y (:head y)
         new-pool pool]
    (cond
     (and (empty? head-x) (empty? head-y))
       (cond
         (and (nil? (:tail x)) (nil? (:tail y)))
           [(PL-List. new-head nil) new-pool]
         (and (nil? (:tail x)) (pl-variable? (:tail y)))
           (let [[new-y newer-pool] (unify (:tail y) (-->list []) new-pool) ]
             [(PL-List. new-head new-y) newer-pool])
         (and (nil? (:tail y)) (pl-variable? (:tail x)))
           (let [[new-x newer-pool] (unify (:tail x) (-->list []) new-pool) ]
             [(PL-List. new-head new-x) newer-pool])
         (or (nil? (:tail y)) (nil? (:tail x)))
           [false pool]
         :else
           (let [[new-tail newer-pool] (unify (:tail x) (:tail y) new-pool)]
             (if (false? new-tail)
               [false pool]
               [(PL-List. new-head new-tail) newer-pool])))

     (empty? head-x)
       (if (nil? (:tail x))
         [false pool]
         (let [new-y (PL-List. head-y (:tail y))
               [new-tail newer-pool] (unify (:tail x) new-y new-pool)]
           (if (false? new-tail)
             [false pool]
             [(PL-List. new-head new-tail) newer-pool])))

     (empty? head-y)
       (if (nil? (:tail y))
         [false pool]
         (let [new-x (PL-List. head-x (:tail x))
               [new-tail newer-pool] (unify (:tail y) new-x new-pool)]
           [(PL-List. new-head new-tail) newer-pool]))

     :else
       (let [[new-elem newer-pool] (unify (first head-x)
                                          (first head-y)
                                          new-pool)]
         (if (false? new-elem)
           [false pool]
           (recur (conj new-head new-elem)
                  (rest head-x)
                  (rest head-y)
                  newer-pool))))))


(defn generate-list [list all-names]
  (loop [new-head []
         old-head (:head list)
         new-names all-names]
    (if (empty? old-head)
      (let [[new-tail final-names] (generate (:tail list) new-names)]
        [(PL-List. new-head new-tail) final-names])

      (let [[new-elem newer-names] (generate (first old-head) new-names)]
        (recur (conj new-head new-elem)
               (rest old-head)
               newer-names)))))



(defn pl-list? [x]
  (= (type x) logic.term.PL-List))


;; =============================================================================
;;  The common unifier.
;;  It automaticly recognizes objects types and unifies them.
;;
;;  The pool is the map of all variables in the current stack frame of the interpreter.
;;


(defn extract-var [var pool]
  (let [pool-var ((:name var) pool)]
    (if (nil? pool-var)
      var
      pool-var)))

(defn bind
  "Binds PL-Variables x and y together."
  [x y pool]
  (let [new-x (extract-var x pool)
        new-y (extract-var y pool)]
    (cond
     (and (pl-variable? new-x) (pl-variable? new-y))
       (let [res-x (-->variable (:name x) nil (:binds new-x))
             res-y (-->variable (:name y) nil (conj (:binds new-y) (:name x)))]
         [res-y (assoc pool (:name x) res-x (:name y) res-y)])
     :else
       (unify new-x new-y pool))))

(declare evaluate)

(defn evaluate-many
  ;; evaluates all variables with value y in the pool
  [vars y pool]
  (loop [others vars
         new-pool pool]
    (if (empty? others)
      new-pool
      (let [name (first others)
            [_ newer-pool] (evaluate (name new-pool) y new-pool)]
        (recur (rest others)
               newer-pool)))))

(defn evaluate
  "Evaluates PL-Variable x with whatever y is"
  [x y pool]
  (let [new-x (extract-var x pool)]
    (if (pl-variable? new-x)
      (let [new-pool (evaluate-many (:binds new-x) y pool)]
        [y (assoc new-pool (:name new-x) y)])
      (unify new-x y pool))))


(defn unify [x y pool]
  (cond
   (and (pl-variable? x) (pl-variable? y))
     (bind x y pool)
   (pl-variable? x)
     (evaluate x y pool)
   (pl-variable? y)
     (evaluate y x pool)
   (different? (type x) (type y))
     false
   (pl-atom? x)
     (unify-atoms x y pool)
   (pl-number? x)
     (unify-numbers x y pool)
   (pl-structure? x)
     (unify-structures x y pool)
   (pl-list? x)
     (unify-lists x y pool)))


(defn generate [elem names]
  (cond
   (pl-variable? elem)
     (generate-variable elem names)
   (pl-list? elem)
     (generate-list elem names)
   :else
     [elem names]))


(defn -->arg [a]
  (if (keyword? a)
    (if (a-z? a)
      (-->atom a)
      (if (A-Z? a)
        (-->variable a)
        (if (same? a :_)
          (-->variable (keyword (gensym)))
          (throw (Exception. (str "Argument " a " has illigal name."))))))
    (if (number? a)
      (-->number a)
      (if (vector? a)
        (-->list a)))))



;; ============================================================================
;;  Prolog Functor: member(A, [_|X]) :- member(A, X).
;;  It's place is not among the Terms (I think), but so be it.
;;
;;  The :- sign is called "neck". It means "if body, then haed".
;;  The head is the structure of the functor - name and arguments list.
;;  The body is a list of structures.
;;
(defrecord Functor [head body])

(defn -->functor [title args body]
  (let [head (-->structure title args)
        new-body (mapv #(-->structure (first %1) (second %1)) body)]
    (Functor. head new-body)))


(defn generate-functor [functor names]
  (let [[new-head head-names] (generate-structure (:head functor) names)]
    (loop [new-body []
           old-body (:body functor)
           new-names head-names]
      (if (empty? old-body)
        [(Functor. new-head new-body) new-names]
        (let [elem (first old-body)
              [new-elem newer-names] (generate-structure elem new-names)]
          (recur (conj new-body new-elem)
                 (rest old-body)
                 newer-names))))))

(def set-values)


(defn set-values-var
  "Sets a variable to it's coresponding value from a pool."
  [var pool]
  (extract-var var pool))

(defn set-values-list
  "Gives to a list the values from a pool."
  [list pool]
  (let [new-tail (set-values (:tail list) pool)]
    (loop [new-head []
           old-head (:head list)]
      (if (empty? old-head)
        (PL-List. new-head new-tail)
        (let [new-elem (set-values (first old-head)
                                    pool)]
          (recur (conj new-head new-elem)
                 (rest old-head)))))))

(defn set-values-struct
  "Gives a structure the values from a pool."
  [struct pool]
  (loop [new-args []
         old-args (:args struct)]
    (if (empty? old-args)
      (PL-Structure. (:name struct) new-args)
      (let [elem (first old-args)
            new-elem (set-values elem pool)]
        (recur (conj new-args new-elem)
               (rest old-args))))))

(defn set-values
  [x pool]
  (cond
   (pl-list? x)
     (set-values-list x pool)
   (pl-variable? x)
     (set-values-var x pool)
   (pl-structure? x)
     (set-values-struct x pool)
   :else x))


(defn match-to-functor
  "Matches a struct to a functor"
  [struct functor pool]
  (let [[uni-head head-pool] (unify-structures struct (first (generate-structure (:head functor) {})) pool)
        new-head (set-values-struct uni-head head-pool)]
    (if (false? new-head)
      [false pool]
      (loop [new-body []
             old-body (:body functor)]
        (if (empty? old-body)
          [(Functor. new-head new-body) head-pool]
          (let [elem (first old-body)
                new-elem (set-values-struct elem head-pool)]
            (recur (conj new-body new-elem)
                   (rest old-body))))))))
