(ns lojic.core.term
  (use lojic.core.env
       lojic.core.utils))


(defprotocol ITerm
  (to-string [this env])
  (generate [this pool])
  (unify [this other env]))


(declare
  create-term
  fact-term?)


(load "term/variable")
(load "term/atom")
(load "term/tuple")
(load "term/fact")
(load "term/rule")
(load "term/null")
(load "term/list")


(defmulti create-term (fn [first & _] (type first)))


(defmethod create-term java.lang.String
  [form]
  (cond
    (valid-var-name? form)
    (create-var form)

    (valid-atom-name? form)
    (create-atom form)

    :else
    (-> "Cannot create a Term from '%s'"
      (format form)
      IllegalArgumentException.
      throw)))


(defmethod create-term clojure.lang.Keyword
  [key & rest]
  (cond
    (= :fact key)
    (apply create-fact rest)

    (= :rule key)
    (apply create-rule rest)))


(defmethod create-term clojure.lang.IPersistentCollection
  [forms]
  (create-list forms))


(defmethod create-term clojure.lang.IPersistentMap
  [map]
  (let [key (-> map keys first)]
    (cond
      (= :fact key)
      (apply create-fact (map key))

      (= :rule key)
      (apply create-rule (map key)))))
