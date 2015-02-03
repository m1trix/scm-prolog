(ns logic.term)


(load "term/atom")
(load "term/arguments")


(declare fact->string)
(declare fact-unify)
(declare generate-fact)


(defrecord PrologFact [#^PrologAtom name
                       #^PrologArgsList args]
  IPrologTerm
  (to-string [this pool] (fact->string this pool))
  (unify [this other pool] (fact-unify this other pool))
  (generate [this names] (generate-fact this names)))


(defmacro prolog-fact? [term]
  `(= (type ~term) PrologFactor))


(defn create-fact
  [[name args]]
  (PrologFact. (create-atom name)
               (create-args-list args)))


(defn fact->string
  "Returns a string that represents the output of the PrologFact."
  [fact pool]
  (-> (StringBuilder.)
      (.append (.to-string (:name fact)pool))
      (.append (.to-string (:args fact) pool))
      (.toString)))
