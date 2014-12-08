(defrecord PrologAtom [name])


(def atom-name-pattern #"[a-z][a-zA-Z_]*|'[^']+'")
(def atom-name-with-no-qoutes-pattern #"[^']+")

(defn create-atom
  "Creates a PrologAtom."
  [[name]]
  (if (nil? (re-matches atom-name-pattern name))
    (throw (Exception. (format "Cannot create PrologAtom with name \"%s\"!" name)))
    (->PrologAtom name)))


(defmacro prolog-atom?
  "Tells whether the term is an atom."
  [term]
  `(= (type ~term) PrologAtom))


(defn unify-atoms
  "Two PrologAtoms unify if their names are the same
  when the quotes are removed."
  [x y pool]
  ;; Getting the unqoted names
  (let [name-x (re-find atom-name-with-no-qoutes-pattern (:name x))
        name-y (re-find atom-name-with-no-qoutes-pattern (:name y))]
    (if (= name-x name-y)
      [(->PrologAtom [name-x]) pool]
      [false pool])))


(defmethod unify-terms [PrologAtom PrologAtom]
  [x y pool]
  (unify-atoms x y pool))


(defmethod unify-terms [PrologVariable PrologAtom]
  [var atom pool]
  (unify-var-with-term var atom pool))


(defmethod unify-terms [PrologAtom PrologVariable]
  [atom var pool]
  (unify-var-with-term var atom pool))


(defmethod to-string PrologAtom
  [atom _]
  (:name atom))
