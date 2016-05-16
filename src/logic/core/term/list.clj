;;  LIST
;;  ====

;;  - head: The first Term of the List.

;;  - tail: The Sublist containing all remaining Terms or a Variable.
;;          The Null is considered an empty List.

;;  + to-string: The string representation of the List - comma separated
;;               string representations of the head and all remaining
;;               Terms reachable from the tail, surrounded by brackets:
;;               [1, atom, Variable, "string"]
;;
;;               If the tail is a Variable with a value, the value
;;               is used to continue the string representation. If the
;;               variable has no value, then the variable is separated
;;               from the Terms with a '|' : [1, 2, 3 | Tail]

;;  + generate:  Returns a new List, where all the head and the tail are
;;               newly generated using the provided names pool.

;;  + unify:     Two Lists unify if their heads unify and their tails unify.
;;               A Variable unifies to a List, by excepting it as a value. 

(in-ns 'logic.core.term)


(declare unify-list-and-term)
(declare list->string)
(declare generate-list)
(declare create-list)


(defrecord List [head tail]
  ITerm

  (to-string [this env]
    (list->string this env))

  (generate [this pool]
    (generate-list this pool))

  (unify [this other env]
    (unify-list-and-term
      this
      other
      env)))


(defn- list-from-vector
  [vector tail]
  (loop [terms (rseq vector)
         result tail]
    (if (empty? terms)
      result
      (recur (next terms)
             (->List (first terms)
                     result)))))


(defn- throw-illegal-arg
  [message]
  (-> message
    (IllegalArgumentException.)
    (throw)))


(defn- valid-list-form?
  [form]
  (and (coll? form)
       (not (map? form))))



(defn- ensure-valid-list-form
  [forms]
  (when-not (valid-list-form? forms)
    (-> "Cannot create a List instance from: %s"
        (format forms)
        throw-illegal-arg)))


(defn- ensure-valid-tail-form
  [form]
  (cond
    (empty? form)
    (throw-illegal-arg
      "A List cannot have an empty tail: [... :| (HERE) ]")

    (-> form next empty? not)
    (-> "A List can have only a single tail Term: [ ... :| (HERE) %s]"
      (format (clojure.string/join " " form))
      throw-illegal-arg)))


(defn- create-list-tail
  [form]
  (ensure-valid-tail-form form)
  (cond
    (valid-var-name? (first form))
    (create-var (first form))

    :else
    (create-list (first form))))


(defn create-list
  "
    Creates a new List.
    The Terms are automatically built, based on their names and type.
  "
  [^:clojure.lang.IPersistentCollection form]
  (ensure-valid-list-form form)
  (loop [forms form
         terms []]
    (cond
      (empty? forms)
      (list-from-vector
        terms
        (create-null))

      (= :| (first forms))
      (list-from-vector
        terms
        (-> forms rest
                  create-list-tail))

      :else
      (recur (next forms)
             (conj terms
                   (-> forms first create-term))))))


(defn list-term?
  "Tells whether the given instance is a List."
  [term]
  (or (instance? logic.core.term.List term)
      (null? term)))


(defn empty-list?
  [list]
  (null? list))


(defn- tail-from-env
  ([list] list)
  ([list env]
    (let [tail (:tail list)]
      (if (list-term? tail)
        tail
        (let [value (get-var-value tail env)]
          (if (nil? value)
            tail
            value))))))


(defn- head->string
  [list env]
  (.to-string (:head list) env))


(defn- list->string
  [list env]
  (if (empty-list? list)
    "[]"
    (loop [result (str "[" (head->string list env))
           next (tail-from-env list env)]
      (cond
        (variable? next)
        (recur (str result " | " (.to-string next env))
               (create-null))
        
        (empty-list? next)
        (str result "]")

        :else
        (recur (str result ", " (head->string next env))
               (tail-from-env next env))))))


(defn- unify-heads
  [left right env]
  (.unify
    (:head left)
    (:head right)
    env))


(defn- unify-lists
  [left right env]
  (loop [left left
         right right
         env env]
    (cond
      (or (variable? left)
          (variable? right))
      (.unify left right env)

      (empty-list? left)
      [(empty-list? right) env]

      (empty-list? right)
      [(empty-list? left) env]

      :else
      (let [[unified? env]
            (unify-heads left right env)]
        (if (not unified?)
          [false nil]
          (recur (tail-from-env left env)
                 (tail-from-env right env)
                 env))))))


(defn- unify-list-and-term
  [list term env]
  (cond
    (list-term? term)
    (unify-lists list term env)

    (variable? term)
    [true (evaluate-var term list env)]

    :else
    [false env]))


(defn- generate-head
  [list pool]
  (.generate (:head list) pool))


(defn- generate-list
  [list pool]
  (loop [result []
         next list
         pool pool]
    (cond
      (variable? next)
      (let [[new-var pool]
            (.generate next pool)]
        [(list-from-vector result new-var) pool])

      (empty-list? next)
      [(list-from-vector result (create-null)) pool]

      :else
      (let [[new-head pool]
            (generate-head next pool)]
        (recur (conj result new-head)
               (:tail next)
               pool)))))
