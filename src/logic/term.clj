;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logic.term
  [:require [logic.util :refer :all]])


(def unify)
(def generate)
(def print-term)



;; ===========================================================================
;;  Prolog Atoms: cat. dog.
;;  They are more like facts - Prolog treats them as a TRUE values.
;;  An Atom name always starts with a small letter.
;;
(defrecord PrologAtom [name])

(defn >atom< [name]
  (if (a-z? name)
    (PrologAtom. name)
    (throw (Exception. (str "Illegal PrologAtom name: " (keyword->string name))))))


(defn prolog-atom? [atom]
  (same? (type atom) logic.term.PrologAtom))


(defn unify-atoms
  "Two atoms unify if they are the same."
  [atom-x atom-y pool]
  (if (same? (:name atom-x)
             (:name atom-y))
    [atom-x pool]
    [false pool]))


(defn print-atom
  "Prints the atom to the screen in a specific format."
  [atom]
  (let [text (subs (str (:name atom)) 1)]
    (print-blue text)))



;; ===========================================================================
;;  Prolog Numbers: 1. 5.5.
;;  They are just numbers.
;;
(defrecord PrologNumber [value])


(defn >number< [value]
  (if (number? value)
    (PrologNumber. value)
    (throw (Exception. (str "Illegal PrologNumber: " value)))))


(defn prolog-number? [number]
  (same? (type number) logic.term.PrologNumber))


(defn unify-numbers
  "Two numbers unify if they have the same value."
  [number-x number-y pool]
  (if (same? (:value number-x)
             (:value number-y))
    [number-x pool]
    [false pool]))


(defn print-number
  "Prints the number to the screen in a specific format."
  [number]
  (print-blue (:value number)))



