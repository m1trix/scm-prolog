;;  NULL
;;  ====

;;  + to-string: The string representation of a Null - [].

;;  + generate:  Returns the same instance.

;;  + unify:     A Null unifies with another Null or a Variable.

(in-ns 'logic.core.term)


(declare unify-null-and-term)


(defrecord Null []
  ITerm

  (to-string [this env] "[]")
  (generate [this pool] [this pool])
  (unify [this other env]
    (unify-null-and-term
      this
      other
      env)))


(defn create-null
  []
  (->Null))


(defn null?
  [term]
  (instance? logic.core.term.Null term))


(defn- unify-null-and-term
  [null term env]
  (if (variable? term)
    [true (evaluate-var term null env)]
    [(null? term) env]))
