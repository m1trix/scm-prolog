(ns logic.core.term
  (use logic.core.env
       logic.core.utils))


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


(declare unify)


(define unify-var-and-term [var term env]
  (let [value (env-get var value)]
    (cond
      (not (nil? value))
      (unify value term env)

      (symbol? term)
      (env-bind env var term)

      :else
      (env-set env var term))


(define unify-lists [left right env]
  (cond
    (and (empty? left) (empty? right))
    env

    (= '& (first left))
    (unify (second left) right env)

    (= '& (first right))
    (unify left (second right) env)

    :else
    (when-let [env (unify (first left) (first right) env])]
      (recur (next left) (next right) env))))


(define unify [first second env]
  (cond
    (symbol? first)
    (unify-var-and-term first second env)

    (symbol? second)
    (unify-var-and-term second first env)

    (and (keyword? first) (keyword? second))
    env

    (and (number? first) (number? second))
    env

    (and (string? first) (string? second))
    env

    (and (sequential? first) (sequential? second))
    (unify-lists first second env)))
