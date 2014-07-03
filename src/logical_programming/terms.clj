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

(defn unify-args
  [x y]
  (let [uargs (map unify x y)
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

(defn pl-variable? [x]
  (= (type x) logical_programming.terms.PL-Variable))

(defn unify-variables
  "Two variables unify by agreeing to \"share\" bindings. This means that if later on, one or the other unifies with another term, then both unify with the term."
  [x y]
  (let [new-x (PL-Variable. (:name x) nil (conj (:binds x) (:name y)))
        new-y (PL-Variable. (:name y) nil (conj (:binds y) (:name x)))]
    [new-x new-y]))



