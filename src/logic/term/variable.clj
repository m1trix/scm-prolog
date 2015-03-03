(in-ns 'logic.term)
(use 'logic.environment)


(def var-name-pattern #"[A-Z][A-Za-z0-9_]*|[_]")


(declare var->string)
(declare var-unify)
(declare generate-var)


(defrecord PrologVariable [#^String name]
  IPrologTerm
  (to-string [this env] (var->string this env))
  (unify [this other env] (var-unify this other env))
  (generate [this names] (generate-var this names))
  (names-set [this] #{(:name this)}))


(defn prolog-var? [term]
  (instance? PrologVariable term))


(defn create-var
  "Cretes an unbound PrologVariable."
  [name]
  (if (re-matches var-name-pattern name)
    (PrologVariable. name)
    (throw (Exception. (format "Cannot create a PrologVariable with name \"%s\"!" name)))))


(defn var->string
  [var env]
  (let [value (get-value env (:name var))]
    (if (nil? value)
      (:name var)
      (.to-string value env))))


(defn bind-variables
  [var-x var-y env]
  (println "binding variables")
  (let [val-x (get-value env (:name var-x))
        val-y (get-value env (:name var-y))]
    (cond
     (nil? val-x)
     (do
       (bind-names env (:name var-x) (:name var-y))
       true)

     (nil? val-y)
     (do
       (bind-names env (:name var-y) (:name var-x))
       true)

     :else
     (.unify val-x val-y env))))


(defn unify-with-term
  [var term env]
  (let [val (get-value env (:name var))]
    (if (nil? val)
      (do
        (bind-value env (:name var) term)
        true)
      (.unify val term env))))


(defn var-unify
  [var term env]
  (if (prolog-var? term)
    (bind-variables var term env)
    (unify-with-term var term env)))


(defn generate-name
  ([] (-> '_G gensym str))
  ([name env]
   (let [new-name (generate-name)]
     (bind-value env name new-name)
     new-name)))


(defn get-val
  [var env]
  (or (get-value env var)
      var))


(defn generate-var
  "Generates a new PrologVariable. If the name of the variable has
  a value inside the pool, that value is used. Otherwise a new
  name is generated and stored in the pool."
  [var pool]
  (let [name (:name var)]
    (PrologVariable.
     (or (and (= "_" name) (generate-name))
         (get-value pool name)
         (generate-name name pool)))))
