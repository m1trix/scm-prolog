;;  FACT
;;  ====

;;  - atom:  An Atom that represents the name of the Fact.

;;  - tuple: The parameters of the Fact.

;;  + to-string: The string representation of the fact
;;               inside the given environment. A Fact is
;;               represented by its Atom and its Tuple:
;;               'example name'(parameter1, PARAMETER2)

;;  + generate:  Generates a new Fact instance by generating
;;               its Atom and its Tuple.

;;  + unify:     Two fact unify if their Atoms unify and if
;;               their Tuples unify.

(in-ns 'logic.term)


(declare unify-fact-and-term)
(declare fact->string)
(declare generate-fact)


(defrecord Fact [atom tuple]
  ITerm
  (to-string [this env]
    (fact->string this env))

  (generate [this pool]
    (generate-fact this pool))

  (unify [this other env]
    (unify-fact-and-term this other env)))


(defn create-fact
  "Creates a new Fact."
  [name parameters]
  (->Fact (create-atom name)
          (create-tuple parameters)))


(defn fact?
  "Tells whether the given instance is a Fact."
  [term]
  (instance? logic.term.Fact term))


(defn- fact->string
  "Returns the string representation of the fact inside the environment."
  [fact env]
  (str (.to-string (:atom fact) env)
       (.to-string (:tuple fact) env)))


(defn- generate-fact
  [fact pool]
  (let [[new-tuple pool]
        (.generate (:tuple fact) pool)]
    [(->Fact (-> fact :atom :name ->Atom)
             new-tuple)
     pool]))


(defn unify-facts
  "Tries to unify the two facts inside the environment."
  [left right env]
  (let [[names-unify? env]
        (.unify (:atom left)
                (:atom right)
                env)]
    (if (not names-unify?)
      [false env]
      (.unify (:tuple left)
              (:tuple right)
              env))))


(defn unify-fact-and-atom
  "Ties to unify the term with the atom inside the environment."
  [term atom env]
  (if-not (-> term :tuple empty-tuple?)
    [false env]
    (.unify (:atom term) atom env)))


(defn unify-fact-and-term
  "Tries to unify the Fact with the Term inside the environment."
  [fact term env]
  (cond
    (fact? term)
    (unify-facts fact term env)

    (atom? term)
    (unify-fact-and-atom fact term env)

    :else
    [false env]))
