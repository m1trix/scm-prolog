;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logic.term
  [:require [logic.util :refer :all]])


(def unify-term)
(def generate-term)
(def output-term)



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


(defn output-atom
  "Prints the atom to the screen in a specific format."
  [atom]
  (let [text (subs (str (:name atom)) 1)]
    text))




;; ===========================================================================
;;  Prolog Numbers: 1,   5.5.
;;  They are just numbers.
;;
(defrecord PrologNumber [value])


(defn >number< [n]
  (if (number? n)
    (PrologNumber. n)
    (throw (Exception. (str "Illegal PrologNumber value: \"" n "\"")))))


(defn prolog-number? [number]
  (same? (type number) logic.term.PrologNumber))


(defn unify-numbers
  "Two numbers unify if they have the same value."
  [number-x number-y pool]
  (if (same? (:value number-x)
             (:value number-y))
    [number-x pool]
    [false pool]))


(defn output-number
  "Prints the number to the screen in a specific format."
  [number]
  (:value number))





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


(defn output-string
  "Prints the string to the screen in a specific format."
  [s]
  (str \' (:string s) \'))




;; ============================================================================
;;  Prolog Variable: X. Whatever.
;;  They do not have a value at their creation.
;;  Once they are evaluated, they cannot be changed.
;;  A Variable name always starts with a capital letter.
;;  The uni-list is a set, that holds all other variables, that are bound to the current one.
;;
;;  A Variable can be bound to another (or many others) variable. That means that, whenever
;;  this Variable gets evaluated, all other Variables that are bound to it, also are evaluated
;;  with the same value.
;;
(defrecord PrologVariable [name value binds])


(defn >variable<
  ([name] (>variable< name nil []))
  ([name value] (>variable< name value []))
  ([name value binds]
   (if (A-Z? name)
     (PrologVariable. name value binds)
     (throw (Exception. (str "Invalid PrologVariable name: \"" name "\""))))))


(defn prolog-variable? [var]
  (same? (type var) logic.term.PrologVariable))


(defn output-variable
  "Prints the variable to the screen in a specific format."
  [var]
  (if (prolog-variable? var)
    (if (nil? (:value var))
      (keyword->string (:name var))
      (str (keyword->string (:name var))
           " = "
           (output-term (:value var))))
    (throw (Exception. (str "Trying to print a " (type var) " like a PrologVariable.")))))





(defn output-term
  [term]
  (cond
   (prolog-atom? term)
     (output-atom term)
   (prolog-number? term)
     (output-number term)
   (prolog-string? term)
     (output-string term)
   (prolog-variable? term)
     (output-variable term)))
