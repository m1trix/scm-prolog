(in-ns 'logic.term)
(load "term/common")

(declare prolog-tuple->string)
(declare prolog-tuple-generate)
(declare prolog-tuple-names-set)
(declare prolog-tuple-unify)

(defrecord PrologTuple [#^clojure.lang.PersistentList terms]
  IPrologTerm
  (to-string [this env] (prolog-tuple->string this env))
  (generate [this] (.generate this (create-env)))
  (generate [this pool] (prolog-tuple-generate this pool))
  (names-set [this] (prolog-tuple-names-set this))
  (unify [this other env] (prolog-tuple-unify this other env)))

(defn create-prolog-tuple
  [& elems]
  (PrologTuple. (map create elems)))

(defn prolog-tuple?
  [term]
  (instance? PrologTuple term))

(defn prolog-tuple->string
  [tuple env]
  (-> (StringBuilder. "(")
      (append-coll (:terms tuple) env ", ")
      (.append ")")
      (.toString)))

(defn prolog-tuple-generate
  [tuple env]
  (-> #(.generate % env)
      (map (:terms tuple))
      (PrologTuple.)))

(defn prolog-tuple-names-set
  [tuple]
  (->> (map #(.names-set %)
            (:terms tuple))
       (reduce union)))

(defn prolog-tuple-unify
  [x y env]
  (and (prolog-tuple? y)
       (= (-> x :terms count)
          (-> y :terms count))
       (loop [terms-x (:terms x)
              terms-y (:terms y)]
         (println terms-x terms-y)
         (cond
          (empty? terms-x) true

          (.unify (first terms-x) (first terms-y) env)
          (recur (rest terms-x) (rest terms-y))

          :else false))))
