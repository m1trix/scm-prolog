(in-ns 'logic.term)


(def string-name-pattern #"\"[^\"]+\"")
(declare string-unify)


(defrecord PrologString [#^String string]
  IPrologTerm
  (to-string [this _] (:string this))
  (unify [this other env] (string-unify this other env))
  (generate [this _] this)
  (names-set [_] #{}))


(def illegal-string-msg "Cannot create PrologString from \"%s\"")


(defn create-string
  "Validates the input and creates an instance of PrologString."
  [input]
  (if (re-matches string-name-pattern input)
    (PrologString. name)
    (throw (IllegalArgumentException.
            (format illegal-string-msg input)))))


(defn prolog-string?
  [term]
  (instance? PrologString term))


(defn string-unify
  [string term env]
  (cond

   (prolog-string? term)
   (= (:string string)
      (:string term))

   (prolog-var? term)
   (.unify term string env)

   :else false))
