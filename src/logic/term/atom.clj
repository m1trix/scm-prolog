;;  ATOM
;;  ====

;;  - name: A string, starting with a small letter and containing
;;          small letters, capital letters, digits and underscores:
;;          atom, aTOM.
;;          It could also be quoted in ingle quotes and can
;;          contain any symbols: 'atom @123 '

;;  + to-string: The string representation of the atom - it's name.

;;  + generate:  Atoms cannot be generated. The same atom is returned.

;;  + unify:     Two atoms unify if their names are the same, when
;;               unqoted.

(in-ns 'logic.term)


(def atom-unquoted-name-pattern #"^[a-z]\w*$")
(def atom-quoted-name-pattern #"^'([^']+)'$")

(declare unify-atom-and-term)

(defrecord Atom [name] ITerm
  (to-string [this _] (:name this))
  (generate [this pool] [this pool])
  (unify [this other env]
    (unify-atom-and-term this
                         other
                         env)))


(defn valid-atom-name?
  "Checks whther the name is a valid Logic Atom name."
  [name]
  (or (re-matches atom-unquoted-name-pattern name)
      (re-matches atom-quoted-name-pattern name)))


(defn- ensure-valid-atom-name
  "Ensures that the given name could be used as a Logic Atom name."
  [name]
  (when-not (valid-atom-name? name)
    (-> "Invalid Logic Atom name '%s'"
      (format name)
      IllegalArgumentException.
      throw)))


(defn create-atom
  "Creates a new Logic Atom."
  [name]
  (ensure-valid-atom-name name)
  (->Atom name))


(defn atom?
  "Tells whether the given instance is a Logic Atom."
  [term]
  (= (type term)
     logic.term.Atom))


(defn- unqote-atom-name
  "Returns the unqoted name of the Logic Atom."
  [name]
  (let [matches (re-matches atom-quoted-name-pattern name)]
    (if (empty? matches)
      name
      (second matches))))


(defn- same-atom-names?
  "Returns true if the two atom names can be unified, false otherwise."
  [left right]
  (= (-> left :name unqote-atom-name)
     (-> right :name unqote-atom-name)))


(defn unify-atom-and-term
  [atom term env]
  (cond
    (variable? term)
    [true (evaluate-var term atom env)]

    (atom? term)
    [(same-atom-names? atom term) env]

    (fact? term)
    (.unify term atom env)

    :else
    [false env]))
