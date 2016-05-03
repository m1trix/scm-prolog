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

(declare atom-unify)

(defrecord Atom [name] ITerm
  (to-string [this _] (:name this))
  (generate [this _] this)
  (unify [this other env]
    (atom-unify this other env)))


(defn- atom-ensure-name
  "Ensures that the given name could be used as a Logic Atom name."
  [name]
  (when (and (not (re-matches atom-unquoted-name-pattern name))
             (not (re-matches atom-quoted-name-pattern name)))
    (-> "Invalid Logic Atom name '%s'"
      (format name)
      (IllegalArgumentException.)
      (throw))))


(defn create-atom
  "Creates a new Logic Atom."
  [name]
  (atom-ensure-name name)
  (->Atom name))


(defn atom?
  "Tells whether the given instance is a Logic Atom."
  [var]
  (instance? Atom var))


(defn- atom-drop-quotes
  "Returns the unqoted name of the Logic Atom."
  [name]
  (let [matches (re-matches atom-quoted-name-pattern name)]
    (if (empty? matches)
      name
      (second matches))))


(defn- atom-unify-names
  "Returns true if the two atom names can be unified, false otherwise."
  [left right]
  (= (-> left :name atom-drop-quotes)
     (-> right :name atom-drop-quotes)))


(defn atom-unify
  [left right env]
  (cond
    (var? right)
    [true (var-evaluate right left env)]

    (atom? right)
    [(atom-unify-names left right) env]

    :else
    [false env]))
