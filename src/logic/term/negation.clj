(in-ns 'logic.term)


(declare not->string)
(declare not-generate)


(defrecord PrologNegation [#^logic.term.IPrologTerm term]
  IPrologTerm
  (to-string [this env] (not->string this env))
  (unify [_ _ _] false)
  (generate [this pool] (not-generate this pool)))


(defn prolog-not?
  [term]
  (instance? PrologNegation term))


(defn create-not
  [input]
  (PrologNegation. (create input)))


(defn not->string
  [neg env]
  (-> (StringBuilder. "not(")
      (.append (.to-string (:term neg) env))
      (.append ")")
      (.toString)))


(defn not-generate
  [neg env]
  (PrologNegation. (.generate (:term neg) env)))
