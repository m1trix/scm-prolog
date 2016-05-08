;;  RULE
;;  ====

;;  - head: A Fact that is to be proved.

;;  - body: The Term that should be true.

;;  + to-string: The string representation of the rule
;;               inside the given environment. A Rule is
;;               represented by its head and its body,
;;               separated by ':-' :
;;               fact(Var) :- fact2(Var, atom).

;;  + generate:  Generates a new Rule instance by generating
;;               its head and its body.

;;  + unify:     A Rule can be unified to a Fact, if its head
;;               unifies with the Fact.
;;               A Rule can be unified to an Atom, if its head
;;               unifies with the Atom.

(in-ns 'logic.term)


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
  "Creates a new Rule."
  [name parameters body]
  (->Rule (create-fact name parameters)
          (create body)))


(defn rule?
  "Tells whether the given instance is a Rule."
  [term]
  (instance? logic.term.Rule term))


(defn- rule->string
  "Returns the string representation of the Rule inside the environment."
  [rule env]
  (format
    "%s :- %s"
    (.to-string (:head rule) env)
    (.to-string (:body rule) env)))


(defn- generate-rule
  [rule pool]
  (let [[new-head pool]
        (.generate (:head rule) pool)

        [new-body pool]
        (.generate (:body rule) pool)]

    [(->Rule new-head new-body) pool]))


(defn unify-rule-and-term
  "Tries to unify the Rule with the Term inside the environment."
  [rule term env]
  (if (or (fact? term)
          (atom? term))
    (.unify (:head rule) term env)
    [false env]))
