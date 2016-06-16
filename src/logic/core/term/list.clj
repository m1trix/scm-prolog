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
  "
  @return
    A new List that holds the Terms from the given vector
    and has the given tail.
  "
  [vector tail]
  (loop [terms (rseq vector)
         result tail]
    (if (empty? terms)
      result
      (recur (next terms)
             (->List (first terms) result)))))


(defn- valid-list-form?
  "
  @return
    True if the given object can be used to create a List.
    False otherwise.
  "
  [form]
  (and (coll? form)
       (not (map? form))))


(defn- ensure-valid-list-form
  "
  @throw
    IllegalArgumentException if the given object
    cannot be used to create a List.
  "
  [form]
  (when-not (valid-list-form? form)
    (-> "Cannot create a List instance from: %s"
        (format form)
        illegal-argument)))


(defn- ensure-valid-tail-form
  "
  @throw
    IllegalArgumentException if the given object
    cannot be used to create a tail of a List.
  "
  [form]
  (cond
    (-> form empty?)
    (-> "A List cannot have an empty tail: [... :| (HERE) ]"
        illegal-argument)

    (-> form next empty? not)
    (-> "A List can have only a single tail Term: [ ... :| %s (HERE) %s]"
        (format
          (first form)
          (->> form next (clojure.string/join)))
        illegal-argument)))


(defn- create-list-tail
  "
  @return
    A new Term instance that can be used as a List tail.
  @throws
    IllegalArgumentException if the given object
    cannot be used to create a List tail.
  "
  [form]
  (ensure-valid-tail-form form)
  (cond
    (valid-var-name? (first form))
    (create-var (first form))

    :else
    (create-list (first form))))


(defn create-list
  "
  @return
    A new List instance.
    The Terms are automatically created based on the given values.
  "
  [^:clojure.lang.IPersistentCollection form]
  (ensure-valid-list-form form)
  (loop [forms form
         terms []]
    (cond
      (empty? forms)
      (list-from-vector terms (create-null))

      (= :| (first forms))
      (list-from-vector
        terms
        (-> forms rest create-list-tail))

      :else
      (recur (next forms)
             (conj terms
                   (-> forms first create-term))))))


(defn list-term?
  "
  @return
    True if the given object is an instance of a List.
    False otherwise.
  "
  [term]
  (or (instance? logic.core.term.List term)
      (null-term? term)))


(defn empty-list?
  "
  @return
    True if the given Term is a List with no elements.
    False otherwise.
  "
  [list]
  (null-term? list))


(defn- tail-from-env
  "
  @return
    A Term that represents the tail of the List.
    1. If the tail is a List, it is returned.
    2. If the tail is a unbound Variable, it is returned.
    3. The value of the Variable is returned otherwise.
  "
  ([list] list)
  ([list env]
    (let [tail (:tail list)]
      (if (list-term? tail)
        tail
        (let [value (get-var-value tail env)]
          (or value tail))))))


(defn- head->string
  "
  @return
    The string representation of the head of the given List
    inside the given environment.
  "
  [list env]
  (.to-string (:head list) env))


(defn- list->string
  [list env]
  (if (empty-list? list)
    "[]"
    (loop [result (str "[" (head->string list env))
           next (tail-from-env list env)]
      (cond
        (var-term? next)
        (recur (str result " | " (.to-string next env))
               (create-null))
        
        (empty-list? next)
        (str result "]")

        :else
        (recur (str result ", " (head->string next env))
               (tail-from-env next env))))))


(defn- unify-heads
  "
  Tries to unify the heads of the given Lists
  inside the given environment.

  @return
    The new environment if the heads unify.
    Nil otherwise.
  "
  [left right env]
  (when-let [left (:head left)]
    (when-let [right (:head right)]
      (.unify left right env))))


(defn- unify-lists
  [left right env]
  (loop [left left
         right right
         env env]
    (cond
      (or (var-term? left)
          (var-term? right))
      (.unify left right env)

      (and (empty-list? left)
           (empty-list? right))
      env

      :else
      (when-let
        [env (unify-heads left right env)]
        (recur
          (tail-from-env left env)
          (tail-from-env right env)
          env)))))


(defn- unify-list-and-term
  [list term env]
  (cond
    (list-term? term)
    (unify-lists list term env)

    (var-term? term)
    (try-unify-with-var term list env)))


(defn- generate-head
  [list pool]
  (.generate (:head list) pool))


(defn- generate-list
  [list pool]
  (loop [result []
         next list
         pool pool]
    (cond
      (var-term? next)
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
