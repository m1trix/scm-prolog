(in-ns 'logic.term)


(load "term/common")


(declare conj->string)
(declare conj-generate)


(defrecord PrologConjunction [#^clojure.lang.PersistentList terms]
  IPrologTerm
  (to-string [this env] (conj->string this env))
  (unify [_ _ _] false)
  (generate [this pool] (conj-generate this pool)))


(defn prolog-conj?
  [term]
  (instance? PrologConjunction term))


(defn create-conj
  [& terms]
  (PrologConjunction. (map create terms)))


(defn conj->string
  [conj env]
  (-> (StringBuilder. "(")
      (append-coll (:terms conj) env ", ")
      (.append ")")
      (.toString)))


(defn conj-generate
  [conj env]
  (PrologConjunction. (map #(.generate % env)
                           (:terms conj))))
