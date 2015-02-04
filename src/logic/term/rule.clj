(ns logic.term)


(load "term/fact")


(declare rule->string)
(declare generate-rule)


(defrecord PrologRule [#^PrologFact head
                       body]
  IPrologTerm
  (to-string [this pool] (rule->string this pool))
  (unify [this other pool] (.unify (:head this) other pool))
  (generate [this names] (generate-rule this names)))


(defmacro prolog-rule?
  [term]
  `(= (type ~term) PrologRule))


(defn create-rule
  [[head tail]]
  (PrologRule. (create head)
               (create tail)))


(defn rule->string
  [rule pool]
  (-> (StringBuilder.)
      (.append (.to-string (:head rule) pool))
      (.append " :- ")
      (.append (.to-string (:body rule) pool))
      (.toString)))


(defn generate-rule
  [rule names]
  (let [[new-head head-names] (.generate (:head rule) names)
        [new-body body-names] (.generate (:body rule) head-names)]
    [(PrologRule. new-head new-body)
     body-names]))
