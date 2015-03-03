(in-ns 'logic.term)


(load "term/variable")


(def atom-name-pattern #"[a-z][a-zA-Z_]*|'[^']+'")
(def atom-unquoted-name-pattern #"[^']+")


(declare atom-unify)


(defrecord PrologAtom [#^String name]
  IPrologTerm
  (to-string [this _] (:name this))
  (unify [this other env] (atom-unify this other env))
  (generate [this _] this)
  (names-set [_] #{}))


(defn prolog-atom?
  "Tells whether the term is an atom."
  [term]
  (instance? PrologAtom term))


(def illegal-atom-name-msg "Cannot create PrologAtom with name \"%s\"")


(defn create-atom
  "Validates the name and creates an instance of PrologAtom."
  [name]
  (if (re-matches atom-name-pattern name)
    (PrologAtom. name)
    (throw (IllegalArgumentException.
            (format illegal-atom-name-msg name)))))


(defn atom-unify
  "Unifies a PrologAtom with a IPrologTerm inside the environment."
  [atom term env]
  (cond

   (prolog-atom? term)
   (= (re-find atom-unquoted-name-pattern (:name atom))
      (re-find atom-unquoted-name-pattern (:name term)))

   (prolog-var? term)
   (.unify term atom env)

   (prolog-formula? term)
   (.unify term atom env)

   :else false))
