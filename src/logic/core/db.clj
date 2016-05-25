(ns logic.core.db
  (:use logic.core.utils)
  (:refer-clojure :exclude [create-ns]))


(defn create-db
  "
  Returns an empty database.
  "
  []
  {})


(defn- ensure-not-ns
  "
  Throws an exception, unless the given name is not a
  name of a namespace from the database.
  "
  [db ns]
  (when (contains? db ns)
    (-> "Namespace '%s' already exists"
        (format ns)
        illegal-argument)))


(defn- ensure-ns
  "
  Throws an exception, unless the given name is a
  name of a namespace from the database.
  "
  [db ns]
  (when-not (contains? db ns)
    (-> "Namespace '%s' doesn't exist"
        (format ns)
        illegal-argument)))


(defn create-ns
  "
  Creates a new empty namespace with the given name
  and returns the new database.
  "
  [db ns]
  (ensure-not-ns db ns)
  (assoc db ns {}))


(defn drop-ns
  "
  Deletes the namespace with the given name
  and returns the new database.
  "
  [db ns]
  (ensure-ns db ns)
  (dissoc db ns))


(defn add-term
  "
  Associates @term with the name @term-name
  inside the namespace with name @ns-name.
  "
  [db ns-name name term]
  (ensure-ns db ns-name)
  (let [ns (db ns-name)
        terms (ns name [])
        new-terms (conj terms term)
        new-ns (assoc ns name new-terms)]
    (assoc db ns-name new-ns)))


(defn drop-terms
  "
  Removes all terms from namespace @ns-name
  that are associated to the @term-name.
  "
  [db ns-name term-name]
  (ensure-ns db ns-name)
  (let [ns (db ns-name)
        new-ns (dissoc ns term-name)]
    (assoc db ns-name new-ns)))


(defn db-view
  "
  Returns an operator that is wrapped arround the
  given namespace. It will return elements from that
  namespace only.
  "
  [db ns]
  (ensure-ns db ns)
  (db ns {}))


(defn db-operator
  "
  Returns an operator that is wrapped around the given
  database. It can be used to obtain Terms by introducing
  custom logic.
  "
  [db operator]
  (fn [name default]
    (or (operator db name)
        default)))


(defn db-get
  "
  Returns the result of applying the operator with the given name.
  "
  [operator name]
  (operator name []))