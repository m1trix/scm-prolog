;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logical-programming.terms
  (:require [logical-programming.util :refer :all]))


(declare unify)

;; ===========================================================================
;;  Prolog Atoms: cat. dog.
;;  They are more like facts - Prolog treats them as a TRUE value.
;;  An Atom name always starts with a small letter.
;;
(defrecord PL-Atom [name])

(defn pl-atom? [x]
  (= (type x) logical_programming.terms.PL-Atom))

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
  (= (type x) logical_programming.terms.PL-Number))

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
(defrecord PL-Structure [name arguments])

(defn pl-structure? [x]
  (= (type x) logical_programming.terms.PL-Structure))

(declare -->arg)

(defn -->args [arg-list]
  (mapv -->arg arg-list))

(defn -->structure [name args]
  (let [args-new (-->args args)]
    (PL-Structure. name args-new)))

(defn unify-args
  [x y pool]
  (let [uargs (map #(unify %1 %2 pool) x y)
        hits (count (filter true? uargs))]
    (if (different? (count x) hits)
      false
      uargs)))

(defn unify-structures
  "Two structures unify if they have the same name and arity, and each pair of respective arguments unify."
  [x y]
  (if (different? (:name x) (:name y))
    false
    (if (different? (arity x) (arity y))
      false
      (let [uargs (unify-args (:args x)
                              (:args y))]
        (if (false? uargs)
          false
          (PL-Structure. (:name x) uargs))))))



;; ============================================================================
;;  Prolog Variable: X. Whatever.
;;  They do not have a value at their creation.
;;  Once they are evaluated, they cannot be changed.
;;  A Variable name always starts with a capital letter.
;;  The uni-list is a set, that holds all other variables, that are bound to the current one.
;;
(defrecord PL-Variable [name value binds])

(defn -->variable [v]
  (PL-Variable. v nil #{}))

(defn pl-variable? [x]
  (= (type x) logical_programming.terms.PL-Variable))

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
  (= (type x) logical_programming.terms.PL-String))

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

(-->list [1 2 :| [2]] )




(defn pl-list? [x]
  (= (type x) logical_programming.terms.PL-List))


;; =============================================================================
;;  The common unifier.
;;  It automaticly recognizes objects types and unifies them.
;;
;;  The pool is the map of all variables in the current stack frame of the interpreter.
;;
(defn unify [x y pool]
  (if (pl-variable? x)
    (if (pl-variable? y)
      (let [[ux uy] (unify-variables x y)]
        (assoc pool (:name ux) ux (:name uy) uy))
      (assoc pool (:name x) (PL-Variable. (:name x) y #{})))
    (if (and (pl-structure? x) (pl-structure? y))
      (unify-structures x y))))


(defn -->arg [a]
  (if (keyword? a)
    (if (a-z? a)
      (-->atom a)
      (if (A-Z? a)
        (-->variable a)
        (if (same? a :_)
          (-->variable a)
          (throw "Illigal name."))))
    (if (number? a)
      (-->number a)
      (if (vector? a)
        (-->list a)))))



;; ============================================================================
;;  Prolog Funct: member(A, [_|X]) :- member(A, X).
;;  It's place is not among the Terms (I think), but so be it.
;;
;;  The :- sign is called "neck". It means body -> head.
;;  The head is the structure of the funct - name and arguments list.
;;  The body is a list of structures.
;;
(defrecord Functor [head body])

(defn -->functor [title args body]
  (let [struct (-->structure title args)
        new-body (mapv -->structure body)]
    (Functor. struct body)))

