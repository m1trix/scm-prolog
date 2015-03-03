(in-ns 'logic.term)


(load "term/common")


(declare disj->string)
(declare disj-generate)


(defrecord PrologDisjunction [#^clojure.lang.PersistentList terms]
  IPrologTerm
  (to-string [this env] (disj->string this env))
  (unify [_ _ _] false)
  (generate [this pool] (disj-generate this pool)))


(defn prolog-disj?
  [term]
  (instance? PrologDisjunction term))


(defn create-disj
  [& terms]
  (PrologDisjunction. (map create terms)))


(defn disj->string
  [disj env]
  (-> (StringBuilder. "(")
      (append-coll (:terms disj) env "; ")
      (.append ")")
      (.toString)))


(defn disj-generate
  [conj env]
  (PrologDisjunction. (map #(.generate % env)
                           (:terms conj))))
