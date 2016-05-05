;;  VARIABLE
;;  ========

;;  - name: A string, starting with a capital letter.
;;          Contains small letters, capital letters, digits,
;;          and underscores: Variable, VAR_123a.
;;          It could also be a single underscore, meaning
;;          that the variable has a random name nad it's
;;          not used anywhere: _.

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


(declare unify-var-and-term)
(declare var->string)
(declare generate-var)


(defrecord Variable [name]
  ITerm
  (to-string [this env]
    (var->string this env))

  (generate [this pool]
    (generate-var this pool))

  (unify [this other env]
    (unify-var-and-term this other env)))


(defn valid-var-name?
  "Tells whether the name can be used as a Logic Variable."
  [name]
  (-> var-name-pattern 
      (re-matches name)
      nil?
      not))


(defn- var-ensure-name
  "Ensures that the given name could be used as a Logic Variable name."
  [name]
  (when-not (valid-var-name? name)
    (-> "Invalid Logic Variable name '%s'"
      (format name)
      (IllegalArgumentException.)
      (throw))))


(defn create-var
  "Creates a new Logic Variable."
  [name]
  (var-ensure-name name)
  (->Variable name))


(defn variable?
  "Tells whether the given instance is a Logic Variable."
  [term]
  (= (type term)
     logic.term.Variable))

(defn- var->string
  "Returns the string representation of the variable inside the environment"
  [var env]
  (let [value (env-get env (:name var))]
    (if (nil? value)
      (:name var) 
      (.to-string value env))))


(defn- generate-var
  [var pool]
  (let [name (:name var)
        new-name (-> '_G gensym str)
        pool-name (pool name)]
    (cond
      (= "_" name)
      [(->Variable new-name) pool]

      (nil? pool-name)
      [(->Variable new-name)
       (assoc pool name new-name)]

      :else
      [(->Variable pool-name) pool])))


(defn- unify-variables
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
      (.unify left-value
              right-value
              env))))


(defn unify-var-and-term
  [var term env]
  (let [value (env-get env (:name var))]
    (cond
      (not (nil? value))
      (.unify value term env)

      (variable? term)
      (unify-variables var term env)

      :else
      [true (env-set env (:name var) term)])))
