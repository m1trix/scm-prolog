(in-ns 'lojic.core.term)


(def var-name-pattern #"^(?:_)|(?:[A-Z]\w*)$")


(declare unify-var-and-term)
(declare var->string)
(declare generate-var)


(defrecord Variable [^String name]
  ITerm
  (to-string [this env]
    (var->string this env))

  (generate [this pool]
    (generate-var this pool))

  (unify [this other env]
    (unify-var-and-term this other env)))


(defn valid-var-name?
  "
  Tells whether the name can be used as a Logic Variable.
  A valid variable name starts with a capital letter A-Z and contains
  only letters, numbers or underscores.

  '_' is also a valid Variable name and it means
  that the Variable has a random name.

  @return
    True if the name is valid, false otherwise.
  "
  [name]
  (and
    (string? name)
    (truthy?
      (re-matches
        var-name-pattern
        name))))


(defn- var-ensure-name
  [name]
  (when-not (valid-var-name? name)
    (->
      "Invalid Logic Variable name '%s'"
      (format name)
      illegal-argument)))


(defn create-var
  "
  @return
    A new Variable instance with the given name.
  @throws
    IllegalArgumentException If that name is invalid.
  "
  [name]
  (var-ensure-name name)
  (->Variable name))


(defn var-term?
  "
  @return
    True if the given instance is a Variable, false otherwise.
  "
  [term]
  (instance?
    lojic.core.term.Variable
    term))


(defn set-var-value
  "
  Binds the given Term as a value of the Variable inside the environment,
  overwritting any existing values.

  @return
    A new environment with the rebound Variable.
  "
  [var term env]
  (env-set
    env
    (:name var)
    term))


(defn get-var-value
  "
  @return
    The value from the environment, that is bound to that Variable.
    If no value is bound, then nil is returned.
  "
  [var env]
  (env-get
    env
    (:name var)))


(defn- var->string
  "
  @return
    The string representation of the Variable inside the environment:
    If the Variable has value bound to it, then its name is returned.
    The result of #to-string of the value is returned otherwise.
  "
  [var env]
  (if-let [value (get-var-value var env)]
    (.to-string value env)
    (:name var)))


(defn- generate-var
  "
  Generates a new Variable instance from the given one.
  1. If the given one is a random Variabe (name is '_'),
     then a unique name is generated.
  2. If the name of the Variable is not mapped to a name in the pool,
     a new name is generated and stored in the pool.
  3. If the name of the Variable is mapped to a name in the pool,
     that name is used and no new names are generated.

  @return
    The newly generated Variable and the new pool of names.
  "
  [var pool]
  (let [name (:name var)
        new-name (-> '_V gensym str)
        pool-name (pool name)]
    (cond
      (= "_" name)
      [(->Variable new-name) pool]

      (nil? pool-name)
      [(->Variable new-name)
       (assoc pool name new-name)]

      :else
      [(->Variable pool-name) pool])))


(defn- bind-variables
  [left right env]
  (env-bind
    env
    (:name left)
    (:name right)))


(defn- unify-variables
  [left right env]
  (let [l-value (get-var-value left env)
        r-value (get-var-value right env)]
    (cond
      (nil? l-value)
      (bind-variables left right env)

      (nil? r-value)
      (bind-variables right left env)

      :else
      (.unify l-value r-value env))))


(defn try-unify-with-var
  "
  Attemt to unify the Variable and the Term inside the environment.
  If the Variable has a value, then the value and the Term are unified.
  Otherwise, the Term becomes the value of the Variable.

  @return
    The result environment, or nil if the Variable already has a value
    and that value does not unify with the Term.
  "
  [var term env]
  (if-let [value (get-var-value var env)]
    (.unify value term env)
    (set-var-value var term env)))


(defn- unify-var-and-term
  "
  A Variable unifies with a Term in the following cases:
  1. The variable has a value in the environment:
     The value is unified with the Term.
  2. The Term is not a Variable:
     The Term is insteam unified with the Variable.
  3. The Term is a Variable:
     If both have values, the values are unified.
     Otherwise the Variables are bound together.

  @return
    A new environemt with different bindings, or nil if not unified.
  "
  [var term env]
  (if (var-term? term)
    (unify-variables var term env)
    (.unify term var env)))
