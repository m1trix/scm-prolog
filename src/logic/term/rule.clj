(in-ns 'logic.term)
(use 'clojure.set)


(declare rule->string)
(declare generate-rule)

(defrecord PrologRule [#^PrologFact head
                       #^logic.term.IPrologTerm body]
  IPrologTerm
  (to-string [this env] (rule->string this env))
  (unify [this other env] (.unify (:head this) other env))
  (generate [this names] (generate-rule this names))
  (names-set [this] (union (-> this :head .names-set)
                       (-> this :body .names-set))))


(defn prolog-rule? [term]
  (instance? PrologRule term))


(defn create-rule
  [head tail]
  (PrologRule. (create head)
               (create tail)))


(defn rule->string
  "Returns the stirng representation of the PrologRule
  with values from the env."
  [rule env]
  (-> (StringBuilder.)
      (.append (.to-string (:head rule) env))
      (.append " :- ")
      (.append (.to-string (:body rule) env))
      (.toString)))


(defn generate-rule
  [rule pool]
  (PrologRule. (.generate (:head rule) pool)
               (.generate (:body rule) pool)))
