(in-ns 'logic.term)
(load "term/fact")


(declare formula-unify)


(defrecord PrologFormula [#^PrologFact fact
                          func]
  IPrologTerm
  (to-string [this env] (-> this :fact (.to-string env)))
  (generate [this pool] (PrologFormula. (-> this :fact (.generate))
                                        (:func this)))
  (names-set [this] (-> this :fact .names-set))
  (unify [_ _ _] formula-unify))


(defn prolog-formula?
  [term]
  (instance? PrologFormula term))


(defn create-formula
  [name args func]
  (PrologFormula. (create-fact name args) func))


(defn formula-unify
  [form term env]
  (cond

   (prolog-fact? term)
   (.unify (:fact form) term env)

   (prolog-atom? term)
   (and (empty? (-> form :fact :args :args))
        (.unify (-> form :fact :atom) term env))

   :else false))
