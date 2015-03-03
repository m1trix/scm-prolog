(in-ns 'logic.term)


(load "term/common")


(declare args->string)
(declare args-unify)
(declare generate-args)
(declare args-names)


(defrecord PrologArgsList [#^clojure.lang.PersistentList args]
  IPrologTerm
  (to-string [this env] (args->string this env))
  (unify [this other env] (args-unify this other env))
  (generate [this names] (generate-args this names))
  (names-set [this] (args-names this)))


(defn prolog-args-list?
  "Tells whether the IPrologTerm is a PrologArgsList."
  [term]
  (instance? PrologArgsList term))


(defn create-args-list
  "Creates new PrologArgsList."
  [terms]
  (PrologArgsList. (map create terms)))


(defn args->string
  [args env]
  (-> (StringBuilder. "(")
      (append-coll (:args args) env ", ")
      (.append ")")
      (.toString)))


(defn generate-args
  [args pool]
  (-> #(.generate % pool)
      (map (:args args))
      (PrologArgsList.)))


(defn unify-next
  [env]
  (fn [res [x y]]
    (and res
         (.unify x y env))))


(defn unify-arg-lists
  "Unifies two lists by unifying each pair of elements."
  [x y env]
  (and (= (count (:args x))
          (count (:args y)))
       (->> (map vector (:args x) (:args y))
            (reduce (unify-next env) true))))


(defn args-unify
  [args term env]
  (cond
   (prolog-var? term)
   (.unify term args env)

   (prolog-args-list? term)
   (unify-arg-lists args term env)

   :else false))


(defn args-names
  [args]
  (->> (map #(.names-set %) (:args args))
       (reduce union)))
