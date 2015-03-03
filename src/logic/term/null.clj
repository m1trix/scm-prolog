(in-ns 'logic.term)


(declare prolog-null?)
(declare null-unify)

(defrecord PrologNull []
  IPrologTerm
  (to-string [this _] "[]")
  (generate [this _] this)
  (unify [this other env] (null-unify this other env))
  (names-set [_] #{}))


(defn prolog-null?
  [term]
  (instance? PrologNull term))


(defn null-unify
  [null term env]
  (cond

   (prolog-null? term)
   true

   (prolog-var? term)
   (.unify term null env)

   :else false))
