(defrecord PrologVariable [name])


(defmacro prolog-var? [term]
  `(= (type ~term) PrologVariable))


(def var-name-regex #"[A-Z][A-Za-z0-9_]*")


(defn create-var
  "Cretes an unbound PrologVariable."
  [[name]]
  (if (re-matches var-name-regex name)
  (->PrologVariable name)
    (throw (Exception. (format "Cannot create a PrologVariable with name \"%s\"!" name)))))


(defn get-root
  "Returns the root variable of the current tree of bounds."
  [var pool]
  (let [val (pool var)]
    (if (prolog-var? val)
      (let [[root new-pool] (get-root val pool)]
        (if (= root val)
          [root new-pool]
          [root (assoc new-pool var root)]))
      [var pool])))


(defn get-val
  "Returns the value of the variable from the pool."
  [var pool]
  (let [[root new-pool] (get-root var pool)
        val (new-pool root)]
    (if (nil? val)
      [root new-pool]
      [val new-pool])))


(defn get-two-vals
  "Returns a vector of the two variable vals from the pool."
  [var-x var-y pool]
  (let [[val-x pool-x] (get-val var-x pool)
        [val-y new-pool] (get-val var-y pool-x)]
    [val-x val-y new-pool]))


(defn set-root
  "Gives a value to the root inside the pool."
  [root val pool]
  [val (assoc pool root val)])


(defn unify-var-with-term
  "Unifies a variable with a term inside the pool."
  [var term pool]
  (let [[val new-pool] (get-val var pool)]
    (if (prolog-var? val)
      (set-root val term new-pool)
      (unify-terms val term new-pool))))


(defn unify-vars
  "Two variables unify if they bind together, one of them accepts the value
  of the other, or if their values unify."
  [var-x var-y pool]
  (let [[val-x val-y new-pool]
        (get-two-vals var-x var-y pool)]
    (cond

     (= val-x val-y)
     [val-x new-pool]

     (prolog-var? val-y)
     (set-root val-y val-x new-pool)

     (prolog-var? val-x)
     (set-root val-x var-y new-pool)

     :else (unify-terms val-x val-y new-pool))))


(defmethod unify-terms [PrologVariable PrologVariable]
  [var-x var-y pool]
  (unify-vars var-x var-y pool))
