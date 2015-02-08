;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;    PrologAtom
;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;  The PrologAtom is a single word, consisted of
;;  A-Z, a-z and underscores, but starts with a
;;  small letter.
;;
;;  It may also be surrounded by single quotes.
;;  Then inside the qoutes all symbols may be used
;;  except single qoutes.
;;  +++++++++++++++++++++++++++++++++++++++++++++++
(in-ns 'logic.term)


(def atom-name-pattern #"[a-z][a-zA-Z_]*|'[^']+'")
(def atom-name-with-no-qoutes-pattern #"[^']+")


(declare atom->string)
(declare atom-unify)


(defrecord PrologAtom [#^String name]
  IPrologTerm
  (to-string [this pool] (:name this))
  (unify [this other pool] (atom-unify this other pool))
  (generate [this names] [this names]))


(defn prolog-atom?
  "Tells whether the term is an atom."
  [term]
  (instance? PrologAtom term))


(defn create-atom
  "Validates the name and creates an instance of PrologAtom."
  [name]
  (if (re-matches atom-name-pattern name)
    (PrologAtom. name)
    (throw (IllegalArgumentException. (str "Cannot create PrologAtom with name \"" name "\"")))))


;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;    .unify(PrologAtom, IPrologTerm, Map)
;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;  Two atom unify if their names are the same
;;  when the qoutes are removed (if any).
;;
;;  A PrologVariable and a PrologAtom unify, when
;;  the PrologVriale receives the value of the atom
;;  or if it's value unifies with the atom.
;;  +++++++++++++++++++++++++++++++++++++++++++++++


(defn unify-atoms
  "Two PrologAtoms unify if their names are the same
  when the quotes are removed."
  [x y pool]
  ;; Getting the unqoted names
  (let [name-x (re-find atom-name-with-no-qoutes-pattern (. x name))
        name-y (re-find atom-name-with-no-qoutes-pattern (. y name))]
      [(= name-x name-y) pool]))


(defn atom-unify
  "Unifies a PrologAtom with a IPrologTerm inside the pool."
  [atom term pool]
  (cond

   (prolog-atom? term)
   (unify-atoms atom term pool)

   (prolog-var? term)
   (.unify term atom pool)

   :else [false pool]))
