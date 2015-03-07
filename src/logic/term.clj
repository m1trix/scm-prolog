(ns logic.term
  (:use [logic.environment]))


(defmulti create (fn [inp & _] (type inp)))


(defprotocol IPrologTerm
  (unify [this other pool])
  (to-string [this pool])
  (generate [this] [this names] "Creates a clone instance of the IPrologTerm, where all PrologVariables have newly generated names.")
  (names-set [this]))


(load "term/variable")
(load "term/atom")
(load "term/string")
(load "term/number")
(load "term/list")
(load "term/fact")
(load "term/rule")
(load "term/conjunction")
(load "term/disjunction")
(load "term/negation")
(load "term/formula")


(def cannot-create-term-msg "Cannot create an IPrologTerm from \"%s\"")


(defmethod create Number
  [input]
  (create-number input))


(defmethod create String
  [input]
  (cond
   (re-matches var-name-pattern input)
   (PrologVariable. input)

   (re-matches atom-name-pattern input)
   (PrologAtom. input)

   (re-matches string-name-pattern input)
   (PrologString. input)

   :else
   (throw (IllegalArgumentException.
           (format cannot-create-term-msg input)))))


(defmethod create clojure.lang.PersistentVector
  [[key & rest :as all]]
  (cond

   (= key :fact)
   (apply create-fact rest)

   (= key :rule)
   (apply create-rule rest)

   (= key :conj)
   (apply create-conj rest)

   (= key :disj)
   (apply create-disj rest)

   (= key :not)
   (apply create-not rest)

   (= key :form)
   (apply create-formula rest)

   :else
   (create-list all)))
