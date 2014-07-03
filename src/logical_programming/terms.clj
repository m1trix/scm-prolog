;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logical-programming.terms
  (:use [logical-programming.util]))


(declare unify)

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

;; ===========================================================================
;;  Prolog Structures: cat(tom), cat(X).
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
  ;; Two structures unify if they have the same name and arity, and each pair of respective arguments unify.
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






