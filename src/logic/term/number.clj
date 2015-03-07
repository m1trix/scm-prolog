(in-ns 'logic.term)


(declare number-unify)


(defrecord PrologNumber [#^Number number]
  IPrologTerm
  (to-string [this _] (-> this :number str))
  (generate [this _] this)
  (names-set [_] #{})
  (unify [this other env] (number-unify this other env)))


(defn prolog-number?
  [term]
  (instance? PrologNumber term))


(defn create-number
  [input]
  (PrologNumber. input))


(defn number-unify
  [number term env]
  (and (prolog-number? term)
       (= (:number number)
          (:number term))))
