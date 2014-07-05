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


