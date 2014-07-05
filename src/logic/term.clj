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
    (throw (Exception. (str "Illegal PrologAtom name: \"" (keyword->string name) "\"")))))


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
    (throw (Exception. (str "Illegal PrologNumber value: \"" value "\"")))))


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





;; ============================================================================
;;  Prolog String: 'str1' 'Yes! It is a string!'
;;  tom. and 'tom'. can be unified.
;;
(defrecord PrologString [string])


(defn >string< [s]
  (if (string? s)
    (PrologString. s)
    (throw (Exception. (str "Illegal PrologString value: \"" s "\"")))))


(defn prolog-string? [string]
  (same? (type string) logic.term.PrologString))


(defn unify-strings
  "Two strings unify if and only if they have precisely the same characters in them."
  [string-x string-y pool]
  (if (= (:string string-x)
         (:string string-y))
    [string-x pool]
    [false pool]))


(defn print-string
  "Prints the string to the screen in a specific format."
  [s]
  (print-blue (str \' (:string s) \')))






