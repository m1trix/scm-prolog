;;  TUPLE
;;  =====

;;  - terms: A vector containing Logic ITerm-s.

;;  + to-string: The string representation of a tuple is
;;               the string representations of all the terms
;;               separated by a comma and surrounded in
;;               brackets: (atom, Variable)

;;  + generate:  Generates a new Tuple instance, where
;;               all the terms are also generated using the
;;               same names pool. All variables with the same
;;               name will continue to have the same name
;;               after that.

;;  + unify:     Two tuples unify if they have the same number
;;               of terms and each pair or terms unify.
(in-ns 'logic.term)


(declare unify-tuple-and-term)
(declare tuple->string)
(declare generate-tuple)


(defrecord Tuple [terms]
  ITerm
  (to-string [this env]
    (tuple->string this env))

  (generate [this pool]
    (generate-tuple this pool))

  (unify [this other env]
    (unify-tuple-and-term this other env)))


(defn create-tuple
  "Creates a new Logic Tuple of ITerms."
  [terms]
  (->Tuple (mapv create terms)))


(defn tuple?
  "Tells whether the given Term is a Tuple."
  [term]
  (= (type term)
     logic.term.Tuple))


(defn empty-tuple?
  "Tells whether the given Tuple is empty."
  [tuple]
  (-> tuple :terms empty?))


(defn- unify-tuple-sizes
  "Tells whether the sizes of the two tuples are the same."
  [left right]
  (= (-> left :terms count)
     (-> right :terms count)))


(defn- zip-tuples
  "Zips the terms of the two tuples."
  [left right]
  (map vector
       (:terms left)
       (:terms right)))


(defn- unify-pair
  "Unifies a pair of ITerm-s."
  [[left right] env]
  (.unify left right env))


(defn- unify-tuples-elements
  "Tells whether each pair of terms from the two tuples unify."
  [left right initial-env]
  (loop [pairs (zip-tuples left right)
         env initial-env]
    (if (empty? pairs)
      [true env]
      (let [[unified? env]
            (unify-pair (first pairs) env)]
        (if (not unified?)
          [false initial-env]
          (recur (next pairs) env))))))


(defn- unify-tuples
  "Tells whether the two tuples unify."
  [left right env]
  (if-not (unify-tuple-sizes left right)
    [false env]
    (unify-tuples-elements
      left
      right
      env)))


(defn- unify-tuple-and-term
  "Tries to unify"
  [left right env]
  (if (tuple? right)
    (unify-tuples left right env)
    [false env]))


(defn- tuple->string
  "Returns the string representation of the Tuple."
  [tuple env]
  (->> (:terms tuple)
       (map #(.to-string % env))
       (clojure.string/join ", ")
       (format "(%s)")))


(defn- generate-next-term
  [[result pool] terms]
  (let [[term pool]
        (.generate (first terms) pool)]
    [(conj result term) pool]))


(defn- generate-tuple
  "Returns a newly generated Tuple."
  [tuple pool]
  (loop [result []
         terms (:terms tuple)
         pool pool]
    (if (empty? terms)
      [(->Tuple result) pool]
      (let [term (first terms)
            [term pool] (.generate term pool)
            result (conj result term)
            terms (next terms)]
        (recur result terms pool)))))
