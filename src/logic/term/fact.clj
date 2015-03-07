(in-ns 'logic.term)


(load "term/tuple")


(declare fact->string)
(declare fact-unify)
(declare generate-fact)


(defrecord PrologFact [#^PrologAtom atom
                       #^PrologTuple args]
  IPrologTerm
  (to-string [this env] (fact->string this env))
  (unify [this other env] (fact-unify this other env))
  (generate [this names] (generate-fact this names))
  (names-set [this] (-> this :args .names-set)))


(defn prolog-fact? [term]
  (instance? PrologFact term))


(defn create-fact
  [name args]
  (PrologFact. (create-atom name)
               (create-args-list args)))


(defn fact->string
  "Returns the string representation of the PrologFact."
  [fact env]
  (-> (StringBuilder.)
      (.append (.to-string (:atom fact) env))
      (.append (.to-string (:args fact) env))
      (.toString)))


(defn unify-facts
  "Two facts unify if their names unify and their arguments unify."
  [left right env]
  (and (.unify (:atom left)
               (:atom right)
               env)
       (.unify (:args left)
               (:args right)
               env)))


(defn fact-unify
  [fact term env]
  (cond

   (prolog-fact? term)
   (unify-facts term fact env)

   (prolog-formula? term)
   (.unify term fact env)

   :else false))


(defn generate-fact
  [fact pool]
  (PrologFact. (:atom fact)
               (.generate (:args fact) pool)))
