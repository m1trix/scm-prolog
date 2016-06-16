(in-ns 'logic.core.term)


(declare unify-rule-and-term)
(declare rule->string)
(declare generate-rule)


(defrecord Rule [head body]
  ITerm
  (to-string [this env]
    (rule->string this env))

  (generate [this pool]
    (generate-rule this pool))

  (unify [this other env]
    (unify-rule-and-term this other env)))


(defn create-rule
  "
  @return
    A new instance of a Rule.
    The parameters and the body are automatically created
    based on the given values.
  "
  [name parameters body]
  (->Rule (create-fact name parameters)
          (create-term body)))


(defn rule-term?
  "
  @return
    True if the given object is an intance of a Rule.
    False otherwise.
  "
  [term]
  (instance? logic.core.term.Rule term))


(defn- rule->string
  "@return
    The string representation of the Rule inside the environment:
    <name>([<arg1>[, <arg2>[, ...]]]) :- <body>
  "
  [rule env]
  (format
    "%s :- %s"
    (.to-string (:head rule) env)
    (.to-string (:body rule) env)))


(defn- generate-rule
  "
  @return
    A newly generated Rule instance.
    All the Terms that are parmeters or inside the body
    that have the same name in the original Rule will have
    the same names in the new Rule.
  "
  [rule pool]
  (let [[new-head pool]
        (.generate (:head rule) pool)

        [new-body pool]
        (.generate (:body rule) pool)]

    [(->Rule new-head new-body) pool]))


(defn- unify-rule-and-term
  "
  Tries to unify the given Rule with the given Term
  inside the given environment. A Rule can only be
  unified with a Fact or an Atom.

  @return
    The new environment if the two Terms unify.
    Nil othewise.
  "
  [rule term env]
  (and
    (or (fact-term? term)
        (atom-term? term))
    (.unify (:head rule) term env)))
