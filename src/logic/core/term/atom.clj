(in-ns 'logic.core.term)


(def atom-unquoted-name-pattern #"^[a-z]\w*$")
(def atom-quoted-name-pattern #"^'([^']+)'$")

(declare unify-atom-and-term)

(defrecord Atom [name]
  ITerm
  (to-string [this _]
    (:name this))

  (generate [this pool]
    [this pool])

  (unify [this other env]
    (unify-atom-and-term this other env)))


(defn valid-atom-name?
  "
  Tells whether the given string is a valid Atom name.
  Atom names can start with a small letter: atom, aTom, aT0m.
  Atom names can also be single qouted. This enables them
  to contain any kinds of symbols: 'atom with spaces!'.

  @return
    True if the name is a valid Atom name, false otherwise.
  "
  [name]
  (and
    (string? name)
    (or (re-matches atom-unquoted-name-pattern name)
        (re-matches atom-quoted-name-pattern name))))


(defn- ensure-valid-atom-name
  [name]
  (when-not (valid-atom-name? name)
    (->
      "Invalid Logic Atom name '%s'"
      (format name)
      illegal-argument)))


(defn create-atom
  "
  @return
    A new Atom instance of the given name.
  @throws
    InvalidArgumentException if the string is invalid Atom name.
  "
  [name]
  (ensure-valid-atom-name name)
  (->Atom name))


(defn atom-term?
  "
  @return
    True if the given object is an instance of an Atom.
    False otherwise.
  "
  [term]
  (instance?
    logic.core.term.Atom
    term))


(defn- unqote-atom-name
  "
  @return
    The name of the Atom, when unqouted (the same name initially unqouted).
  "
  [name]
  (let [matches (re-matches atom-quoted-name-pattern name)]
    (if-not (empty? matches)
      (second matches)
      name)))


(defn- same-atom-names?
  "
  @return
    True if the two Atom names unify.
    False otherwise."
  [left right]
  (= (-> left :name unqote-atom-name)
     (-> right :name unqote-atom-name)))


(defn- unify-atom-and-term
  "
  Tries to unify the given Atom with the given Term
  inside the given environment.

  @return
    The new environment, if the two terms unify.
    Nil otherwise.
  "
  [atom term env]
  (cond
    (var-term? term)
    (try-unify-with-var term atom env)

    (fact-term? term)
    (.unify term atom env)

    (and (atom-term? term)
         (same-atom-names? atom term))
    env))
