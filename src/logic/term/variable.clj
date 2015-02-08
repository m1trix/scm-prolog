;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;    PrologVariable
;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;  The PrologVarible is a single word consisted of
;;  A-Z, a-z and underscore. It starts with a
;;  capital letter.
;;  +++++++++++++++++++++++++++++++++++++++++++++++
(in-ns 'logic.term)


(def var-name-pattern #"[A-Z][A-Za-z0-9_]*|[_]")


(declare var->string)
(declare var-unify)
(declare generate-var)


(defrecord PrologVariable [#^String name]
  IPrologTerm
  (to-string [this pool] (var->string this pool))
  (unify [this other pool] (var-unify this other pool))
  (generate [this names] (generate-var this names)))


(defn prolog-var? [term]
  (instance? PrologVariable term))


(defn create-var
  "Cretes an unbound PrologVariable."
  [name]
  (if (re-matches var-name-pattern name)
    (PrologVariable. name)
    (throw (Exception. (format "Cannot create a PrologVariable with name \"%s\"!" name)))))


(defn get-root
  "Returns the root variable of the current tree of bounds."
  [var pool]
  (let [value (pool var)]
    (cond

     (nil? value)
     [var pool]

     (prolog-var? value)
     (let [[root new-pool] (get-root value pool)]
       [root (assoc new-pool var root)])

     :else
     [var pool])))


(defn get-val
  "Returns the value of the variable from the pool."
  [var pool]
  (let [[root new-pool] (get-root var pool)
        value (new-pool root)]
    (if (nil? value)
      [root new-pool]
      [value new-pool])))


(defn get-two-vals
  "Returns a vector of the two variable vals from the pool."
  [var-x var-y pool]
  (let [[val-x pool-x] (get-val var-x pool)
        [val-y new-pool] (get-val var-y pool-x)]
    [val-x val-y new-pool]))


(defn set-root
  "Gives a value to the root inside the pool."
  [root value pool]
  (assoc pool root value))


(defn unify-var-with-term
  "Unifies a variable with a term inside the pool."
  [var term pool]
  (let [[value new-pool] (get-val var pool)]
    (if (prolog-var? value)
      [true (set-root value term new-pool)]
      (.unify value term new-pool))))


(defn unify-vars
  "Two variables unify if they bind together, one of them accepts the value
  of the other, or if their values unify."
  [var-x var-y pool]
  (let [[val-x val-y new-pool]
        (get-two-vals var-x var-y pool)]
    (cond

     (= val-x val-y)
     [true new-pool]

     (prolog-var? val-y)
     [true (set-root val-y val-x new-pool)]

     (prolog-var? val-x)
     [true (set-root val-x var-y new-pool)]

     :else (unify val-x val-y new-pool))))


(defn var-unify
  "Unifies a PrologVariable with a IPrologTerm inside the pool."
  [var term pool]
  (if (prolog-var? term)
    (unify-vars var term pool)
    (unify-var-with-term var term pool)))


(defn var->string
  "Returns a string that represents the output of the PrologVariable value."
  [var pool]
  (let [[value _] (get-val var pool)]
    (if (prolog-var? value)
      (:name value)
      (.to-string value pool))))


(defn generate-var
  [var names]
  (let [name (names (:name var))]
    (if-not (nil? name)
      [(PrologVariable. name) names]
      (let [gen-name (-> '_G gensym str)]
        [(PrologVariable. gen-name) (assoc names (:name var) gen-name)]))))
