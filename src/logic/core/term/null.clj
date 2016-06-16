(in-ns 'logic.core.term)


(declare unify-null-and-term)


(defrecord Null []
  ITerm
  (to-string [this env]
    "[]")

  (generate [this pool]
    [this pool])

  (unify [this other env]
    (unify-null-and-term
      this
      other
      env)))


(def ^:private null-term (->Null))


(defn create-null
  "
  @return
    The Null term instance.
  "
  []
  null-term)


(defn null-term?
  "
  @return
    True if the given object is the Null term.
    False otherwise.
  "
  [term]
  (instance?
    logic.core.term.Null
    term))


(defn- unify-null-and-term
  "
  Tries to unify the given Null with the given Term
  inside the given environment. The Null term unifies
  only with another Null term or with an unbound
  Variable.

  @return
    The new environment if the two Terms unify.
    Nil otherwise.
  "
  [null term env]
  (cond
    (var-term? term)
    (try-unify-with-var term null env)

    (null-term? term)
    env))
