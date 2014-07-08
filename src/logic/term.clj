;; ==========================================================================
;;  This file contains all types of Terms that Prolog uses in order to work.
;; ==========================================================================

(ns logic.term
  [:require [logic.util :refer :all]])


(def unify-terms)
(def generate-term)
(def output-term)
(def >term<)



;; ===========================================================================
;;  Prolog Atoms: cat. dog.
;;  They are more like facts - Prolog treats them as a TRUE values.
;;  An Atom name always starts with a small letter.
;;
(defrecord PrologAtom [name])

(defn >atom< [name]
  (if (a-z? name)
    (PrologAtom. name)
    (throw
     (Exception.
      (str
       "Illegal PrologAtom name: \""
       (keyword->string name)
       "\"")))))


(defn prolog-atom? [atom]
  (same? (type atom) logic.term.PrologAtom))


(defn unify-atoms
  "Two atoms unify if they are the same."
  [atom-x atom-y pool]
  (if (same?
       (:name atom-x)
       (:name atom-y))
    [atom-x pool]
    [false pool]))


(defn output-atom
  "Prints the atom to the screen in a specific format."
  [atom]
  (let [text (keyword->string (:name atom))]
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
  ([name]
   (>variable< name nil []))
  ([name value]
   (>variable< name value []))
  ([name value binds]
   (if (A-Z? name)
     (PrologVariable. name value binds)
     (throw (Exception.
             (str "Invalid PrologVariable name: \""
                  name
                  "\""))))))


(defn prolog-variable? [var]
  (same? (type var)
         logic.term.PrologVariable))


(defn unify-variables
  "Two variables unify by agreeing to 'share' bindings. This means that if later on, one or the other unifies with another term, then both unify with the term."
  [var-x var-y pool]
  (cond

   (not (prolog-variable? var-x))
     (throw
      (Exception.
       (str
        (output-term var-x)
        " is not a PrologVariable!")))

   (not (prolog-variable? var-y))
     (throw
      (Exception.
       (str
        (output-term var-y)
        " is not a PrologVariable!")))

   :else
   (let [new-y
         (>variable<
          (:name var-y)
          nil
          (conj
           (:binds var-y)
           (:name var-x)))

         new-x
         (>variable<
          (:name var-x)
          new-y
          (:binds
           var-x
           (:name var-y)))

         new-pool
         (assoc pool
           (:name new-x) new-x
           (:name new-y) new-y)]

     [new-y new-pool])))


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



;; ============================================================================
;;  Prolog List: [A, B, C]. [A | X]. [_ | 3]. [1 | _].
;;  List of Prolog Temrs - the elementss can be anything.
;;
;;  They have a head [1, 2 and a tail | X], where the head
;;  contains elemens and the tail is another list (could be and empty one),
;;  or a variable.
;;
(defrecord PrologList [head tail])


(defn >list< [elems]
  (loop [new-head []
         old-head elems]
    (if (empty? old-head)
      (PrologList. new-head [])
      (let [elem (first old-head)]
        (if (same? elem :|)
          (let [last (second old-head)]
            (if (= second [])
              (PrologList. new-head [])
              (PrologList. new-head (>term< last))))
          (recur (conj new-head (>term< elem))
                 (rest old-head)))))))


(defn prolog-list? [list]
  (same? (type list)
         logic.term.PrologList))


(defn unify-tails
  "Unifies the tails of two PrologLists"
  [tail-x tail-y pool]
  (cond

   (and (same? tail-x [])
        (same? tail-y []))
     [[] pool]

   (same? tail-x [])
     (if (prolog-variable? tail-y)
       (unify-terms tail-y (>list< []) pool)
       [false pool])

   (same? tail-y [])
     (if (prolog-variable? tail-x)
       (unify-terms tail-x (>list< []) pool))

   :else
     (unify-terms tail-x tail-y pool)))


(defn unify-lists
  "Two lists unify if their initial elements unify, and the lists which remain after removing both initial elements unify."
  [list-x list-y pool]
  (loop [new-head []
         head-x (:head list-x)
         head-y (:head list-y)
         new-pool pool]
    (cond

     ;; Both lists heads are empty => unifying the tails.
     (and (empty? head-x)
          (empty? head-y))
       (let [[new-tail final-pool]
             (unify-tails
              (:tail list-x)
              (:tail list-y)
              new-pool)]
         (if (false? new-tail)
           [false pool]
           [(PrologList. new-head new-tail) final-pool]))

     ;; List-x head is empty => matching it's tail with the rest of list-y.
     (empty? head-x)
       (let [rest-y (PrologList. head-y (:tail list-y))
             [new-tail final-pool] (unify-tails (:tail list-x)
                                                rest-y
                                                new-pool)]
         (if (false? new-tail)
           [false pool]
           [(PrologList. new-head new-tail) final-pool]))

     ;; List-y head is empty => matching it's tail with the rest of list-x.
     (empty? head-y)
       (let [rest-x (PrologList. head-x (:tail list-x))
             [new-tail final-pool] (unify-tails (:tail list-y)
                                                 rest-x
                                                 new-pool)]
         (if (false? new-tail)
           [false pool]
           [(PrologList. new-head new-tail) final-pool]))

     :else
       (let [[new-elem newer-pool] (unify-terms
                                     (first head-x)
                                     (first head-y)
                                     new-pool)]
         (if (false? new-elem)
           [false pool]
           (recur (conj new-head new-elem)
                  (vec (rest head-x))
                  (vec (rest head-y))
                  newer-pool))))))


(defn output-list
  "Makes a string, that represents the PrologList in it's output format."
  [list]
  (let [out-tail (if (empty? (:tail list))
                   ""
                   (str "| "
                        (output-term (:tail list))))]
    (loop [out-head "["
           elems (:head list)]
      (cond
       (empty? elems)
         (str out-head out-tail "]")
       (empty? (rest elems))
         (str out-head
              (output-term (first elems))
              out-tail
              "]")
       :else
         (recur (str out-head
                     (output-term (first elems))
                     ",")
                (rest elems))))))





(defn evaluate-variable
  [var term pool]
  (if (nil? (:value var))
    (let [new-var (>variable<
                   (:name var)
                   term
                   #{})]
      (loop [new-pool pool
             vars (:binds var)]
        (if (empty? vars)
          [term (assoc new-pool
                     (:name var)
                     new-var)]
          (let [[_ newer-pool] (evaluate-variable
                                (first vars)
                                term
                                new-pool)]
            (recur newer-pool
                   (dissoc vars (first vars)))))))
    [false pool]))





(defn unify-terms
  [term-x term-y pool]
  (cond
   (and (prolog-variable? term-x)
        (prolog-variable? term-y))
     (unify-variables term-x term-y pool)
   (prolog-variable? term-x)
     (evaluate-variable term-x term-y pool)
   (prolog-variable? term-y)
     (evaluate-variable term-y term-x pool)
   (different? (type term-x)
               (type term-y))
     [false pool]
   (prolog-atom? term-x)
     (unify-atoms term-x term-y pool)
   (prolog-number? term-x)
     (unify-numbers term-x term-y pool)
   (prolog-string? term-x)
     (unify-strings term-x term-y pool)
   (prolog-list? term-x)
     (unify-lists term-x term-y pool)
  ))


(defn >term<
  "Creates a term from the input."
  [inp]
  (cond
   (number? inp)
     (>number< inp)
   (vector? inp)
     (>list< inp)
   (string? inp)
     (>string< inp)
   (same? :_ inp)
     (>variable< (keyword (gensym)))
   (a-z? inp)
     (>atom< inp)
   (A-Z? inp)
     (>variable< inp)))


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
     (output-variable term)
   (prolog-list? term)
     (output-list term)))



