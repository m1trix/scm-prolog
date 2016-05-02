;;  VARIABLE
;;  ========

;;  - name: A string, starting with a capital letter.
;;          Contains small letters, capital letters, digits,
;;          underscores and dashes.

;;  + to-string: The string representation of the variable
;;               inside the given environment. If it does
;;               not have a value - the name is used. Otherwise,
;;               the #to-string method of the value is called.

;;  + generate:  Generates a new Variable instance.
;;               The new name is taken from the pool of
;;               names (if an entry exists), or a new
;;               name is generated and then added to the pool.

;;  + unify:     Tries to unify the variable with the given
;;               term. If the term is not a Variable,
;;               false is returned. If both variables have
;;               values, then the values are unified, otherwise
;;               the variables are bound together.

(in-ns 'logic.term)


(def var-name-pattern #"^(?:_)|(?:[A-Z]\w*)$")

(declare var-unify)
(declare var->string)
(declare var-generate)

(defrecord Variable [name] ITerm
  (to-string [this env]
    (var->string this env))

  (generate [this pool]
    (var-generate this pool))

  (unify [this other env]
    (var-unify this other env)))


(defn- var-ensure-name
  "Ensures that the given name could be used as a Logic Variable name."
  [name]
  (when-not (re-matches var-name-pattern name)
    (-> "Invalid Logic Variable name '%s'"
      (format name)
      (IllegalArgumentException.)
      (throw))))


(defn var-create
  "Creates a new Logic Variable."
  [name]
  (var-ensure-name name)
  (->Variable name))


(defn var?
  "Tells whether the given instance is a Logic Variable."
  [var]
  (instance? Variable var))


(defn var->string
  "Returns the string representation of the variable inside the environment"
  [var env]
  (let [value (env-get env (:name var))]
    (if (nil? value)
      (:name var) 
      (.to-string value env))))


(defn var-generate
  [var pool]
  (let [name (:name var)
        new-name (-> '_G gensym str)
        pool-name (env-get pool name)]
    (cond
      (= "_" name)
      [(->Variable new-name)
       pool]

      (nil? pool-name)
      [(->Variable new-name)
       (env-set pool name new-name)]

      :else
      [(->Variable pool-name)
       pool])))


(defn- var-bind
  [left right env]
  (let [left-value
        (env-get env (:name left))

        right-value
        (env-get env (:name right))]
    (cond
      (nil? left-value)
      [true (env-bind env
                     (:name left)
                     (:name right))]
      (nil? right-value)
      [true (env-bind env
                      (:name right)
                      (:name left))]
      :else
      (unify left-value
             right-value
             env))))


(defn var-unify
  [left right env]
  (if-not (var? right)
    [false env]
    (var-bind left right env)))
