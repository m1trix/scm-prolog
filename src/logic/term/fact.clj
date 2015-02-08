(in-ns 'logic.term)


(load "term/arguments")


(declare fact->string)
(declare fact-unify)
(declare generate-fact)


(defrecord PrologFact [#^PrologAtom atom
                       #^PrologArgsList args]
  IPrologTerm
  (to-string [this pool] (fact->string this pool))
  (unify [this other pool] (fact-unify this other pool))
  (generate [this names] (generate-fact this names)))


(defn prolog-fact? [term]
  (instance? PrologFact term))


(defn create-fact
  [[name args]]
  (PrologFact. (create-atom name)
               (create-args-list args)))


(defn fact->string
  "Returns a string that represents the output of the PrologFact."
  [fact pool]
  (-> (StringBuilder.)
      (.append (.to-string (:atom fact)pool))
      (.append (.to-string (:args fact) pool))
      (.toString)))


(defn unify-facts
  [left right pool]
  (if-not (-> (.unify (:atom left)
                      (:atom right)
                      pool)
              (first))
    [false pool]
    (.unify (:args left)
            (:args right)
            pool)))


(defn fact-unify
  [fact term pool]
  (cond

   (prolog-fact? term)
   (unify-facts term fact pool)

   :else [false pool]))


(defn generate-fact
  [fact names]
  (let [[new-args new-names]
        (.generate (:args fact) names)]
    [(PrologFact. (:atom fact) new-args)
     new-names]))
