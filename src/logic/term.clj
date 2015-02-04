(ns logic.term
  (:use logic.util))


(defprotocol IPrologTerm
  (unify [this other pool])
  (to-string [this pool])
  (generate [this names]))


(load "term/fact")


(defmulti create (fn [inp & _] (type inp)))


(defmethod create String
  [input]
  (cond
   (re-matches var-name-pattern input)
   (PrologVariable. input)

   (re-matches atom-name-pattern input)
   (PrologAtom. input)

   :else
   (throw (IllegalArgumentException.
           (str "Cannot recognize a Term in \"" input "\"")))))

(defmethod create clojure.lang.PersistentVector
  [[key & rest :as all]]
  (cond

   (= key :fact)
   (create-fact rest)))
