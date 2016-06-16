(in-ns 'logic.core.term)


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
  "
  @return
    A new instance of a Fact.
    All Term parameters are automatically created based on the given values.
  "
  [name parameters]
  (->Fact (create-atom name)
          (create-tuple parameters)))


(defn fact-term?
  "
  @return
    True if the given object is an instance of a Fact.
    False otherwise.
  "
  [term]
  (instance?
    logic.core.term.Fact
    term))


(defn- fact->string
  "
  @return
    The string representation of the Fact:
    <name>([<arg1>[, <arg2> [, ...]]])
  "
  [fact env]
  (str (.to-string (:atom fact) env)
       (.to-string (:tuple fact) env)))


(defn- generate-fact
  "
  @return
    A newly generated Fact instance.
    All parameter Terms that have the same name in the original Tuple
    will have the same names in the new Tuple.
  "
  [fact pool]
  (let [[new-tuple pool]
        (.generate (:tuple fact) pool)]
    [(->Fact (-> fact :atom :name ->Atom)
             new-tuple)
     pool]))


(defn unify-facts
  "
  Tries to unify the two Facts inside the given environment.
  Two Facts unify if their names unify and their parameters unify.

  @return
    The new environment if the given Facts unify.
    Nil otherwise.
  "
  [left right env]
  (and
    (.unify (:atom left)
            (:atom right)
            env)
    (.unify (:tuple left)
            (:tuple right)
            env)))


(defn unify-fact-and-atom
  "
  Tries to unify the given Fact with the given Atom inside the given environment.
  A Fact and an Atom unify only if their names unify and if the Fact
  has no parameters.

  @return
    The new environment if the two Terms unify.
    False otherwise. 
  "
  [term atom env]
  (and (-> term :tuple empty-tuple?)
       (.unify (:atom term) atom env)))


(defn unify-fact-and-term
  "
  Tries to unify the given Fact with the given Term
  inside the given environment.
  A Fact can only be unified with an Atom or with another Fact.

  @return
    The new environment if the two Terms unify.
    Nil otherwise.
  "
  [fact term env]
  (cond
    (fact-term? term)
    (unify-facts fact term env)

    (atom-term? term)
    (unify-fact-and-atom fact term env)))
