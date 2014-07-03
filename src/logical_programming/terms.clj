;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logical-programming.terms
  (:use [logical-programming.util]))



;; ===========================================================================
;;  Prolog Atoms: cat. dog.
;;  They are more like facts - Prolog proves them to be true.
;;
(defrecord PL-Atom [name])

(defn pl-atom? [x]
  (= (type x) logical_programming.terms.PL-Atom))

(defn unify-atoms
  ;; Two atoms unify if they are the same.
  [x y]
  (if (same? (:name x) (:name y))
    x
    false))



;; ===========================================================================
;;  Prolog Numbers: 1, 5.5.
;;  They are just numbers.
;;

(defrecord PL-Number [value])

(defn pl-number? [x]
  (= (type x) logical_programming.terms.PL-Number))

(defn unify-numbers

  [x y]
  (if (same? (:value x) (:value y))
    x
    false))
