;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logic.terms
  (:require [logic.util :refer :all]))


(declare unify)

;; ===========================================================================
;;  Prolog Atoms: cat. dog.
;;  They are more like facts - Prolog treats them as a TRUE value.
;;  An Atom name always starts with a small letter.
;;
(defrecord PL-Atom [name])

(defn pl-atom? [x]
  (= (type x) logic.terms.PL-Atom))

(defn -->atom [keyw]
  (PL-Atom. keyw))

(defn unify-atoms
  "Two atoms unify if they are the same."
  [x y]
  (if (same? (:name x) (:name y))
    x
    false))



;; ===========================================================================
;;  Prolog Numbers: 1. 5.5.
;;  They are just numbers.
;;

(defrecord PL-Number [value])

(defn -->number [n]
  (PL-Number. n))

(defn pl-number? [x]
  (= (type x) logic.terms.PL-Number))

(defn unify-numbers
  "Two numbers unify if they have the same value."
  [x y]
  (if (same? (:value x) (:value y))
    x
    false))

;; ===========================================================================
;;  Prolog Structures: cat(tom). member(X, Y).
;;  Each structure has a name and arguments.
;;
(defrecord PL-Structure [name args])

(defn pl-structure? [x]
  (= (type x) logic.terms.PL-Structure))

(declare -->arg)

(defn -->args [arg-list]
  (mapv -->arg arg-list))

(defn -->structure [name args]
  (let [args-new (-->args args)]
    (PL-Structure. name args-new)))

(defn unify-args
  [arg1 arg2 arg-pool]
  (loop [res []
         x arg1
         y arg2
         pool arg-pool]
    (if (empty? x)
      [res pool]
      (let [[z p] (unify (first x) (first y) pool)]
        (if (false? z)
          [false arg-pool]
          (recur (conj res z)
                 (rest x)
                 (rest y)
                 p))))))

(unify-args [(-->number 0)] [(-->number 0)] {})

(defn unify-structures
  "Two structures unify if they have the same name and arity, and each pair of respective arguments unify."
  [x y pool]
  (if (different? (:name x) (:name y))
    false
    (if (different? (arity x) (arity y))
      false
      (let [[new-args new-pool] (unify-args (:args x)
                                            (:args y)
                                            pool)]
        (if (false? new-args)
          false
          [(PL-Structure. (:name x) new-args) new-pool])))))

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

(defn pl-variable? [x]
  (= (type x) logic.terms.PL-Variable))

(defn unify-variables
  "Two variables unify by agreeing to \"share\" bindings. This means that if later on, one or the other unifies with another term, then both unify with the term."
  [x y]
  (let [new-x (PL-Variable. (:name x) nil (conj (:binds x) (:name y)))
        new-y (PL-Variable. (:name y) nil (conj (:binds y) (:name x)))]
    [new-x new-y]))



;; ============================================================================
;;  Prolog String: 'str1'
;;  tom. and 'tom'. can be unified.
;;
(defrecord PL-String [word])

(defn -->string [s]
  (PL-String. s))

(defn pl-string? [x]
  (= (type x) logic.terms.PL-String))

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



(defn pl-list? [x]
  (= (type x) logic.terms.PL-List))


;; =============================================================================
;;  The common unifier.
;;  It automaticly recognizes objects types and unifies them.
;;
;;  The pool is the map of all variables in the current stack frame of the interpreter.
;;


(defn bind
  "Binds PL-Variables x and y together."
  [x y pool]
  (let [x-new (PL-Variable. (:name x) nil (:binds x))
        y-new (PL-Variable. (:name y) nil (:binds y))
        z-new (PL-Variable. (keyword (gensym)) nil [(:name x) (:name y)])
        pool-new (assoc pool (:name x) x (:name y) y)]
    [z-new pool-new]))


(defn evaluate
  "Evaluates Pl-Variable x with whatever y is"
  [x y pool]
  (let [new-pool (evaluate-many (:binds x) y pool)
        new-pool (assoc pool (:name x) (-->variable (:name x) y []))]
    [y new-pool]))


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
     [(unify-atoms x y) pool]
   (pl-number? x)
     [(unify-numbers x y) pool]
   (pl-structure? x)
     (unify-structures x y pool)))


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
  (let [struct (-->structure title args)
        new-body (mapv -->structure body)]
    (Functor. struct body)))

