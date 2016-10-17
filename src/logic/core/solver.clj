(ns logic.core.solver
  (:use logic.core.term
        logic.core.env
        logic.core.db)
  (:refer-clojure
    :exclude [create-ns]))


(declare solve-seq)


(defn- next-term
  "
  Returns a newly generated instance
  of the first term from the given @terms.
  "
  [terms]
  (-> terms first (.generate {}) first))


(defn- get-head
  [term]
  (cond
    (rule-term? term) (:head term)
    :else term))

(defn- get-body
  [term]
  (cond
    (rule-term? term) (:body term)))


(defn- get-name
  [term]
  (cond
    (atom-term? term) (:name term)
    (fact-term? term) (-> term :atom :name)
    (rule-term? term) (-> term :head :atom :name)))


(defn- solve-next
  [what terms env]
  (let [with (next-term terms)]
    (when-let [env (.unify what (get-head with) env)]
      [env (get-body with)])))


(declare solve-seq)


(defn solve
  [term db]
  (let [terms (db-get db (get-name term))]
    (solve-seq
      db
      (list [term terms (env-create)]))))


(defn- push-next-frame
  [stack [term terms env]]
  (conj
    stack
    [term (next terms) env]))


(defn- push-body
  [stack db env body]
  (conj
    stack
    [body
     (db-get db (get-name body))
     env]))


(defn- solve-seq
  "
  Returns a lazy seq of all solutions.
  Each element of the seq is an environment that
  holds the value bindings of all variables.
  "
  [db stack]
  (let [[term terms env :as frame] (first stack)
        stack (next stack)]
    (cond
      ; Empty execution tree
      (nil? frame)
      '()

      ; Empty execution branch - go to the next branch
      (empty? terms)
      (recur db stack)

      :else
      (let [stack (push-next-frame stack frame)
            [env body] (solve-next term terms env)]
        (cond
          ; No match - continue with the next term
          (nil? env)
          (recur db stack)

          ; No body - add the result and continue to the next branch
          (nil? body)
          (lazy-seq (cons env (solve-seq db stack)))

          ; Solve the body and then continue to the next branch
          :else
          (recur db (push-body stack db env body)))))))
