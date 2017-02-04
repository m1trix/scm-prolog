(in-ns 'lojic.core.term)


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
  "
  @return
    A new Tuple instance. All the Terms are created automatically
    based on the given values.
  "
  [terms]
  (->Tuple (mapv create-term terms)))


(defn tuple-term?
  "
  @return
    True if the given object is an instance of a Tuple.
    False otherwise.
  "
  [term]
  (instance?
    lojic.core.term.Tuple
    term))


(defn empty-tuple?
  "
  @return
    True if the given Tuple is empty.
    False otherwise.
  "
  [tuple]
  (-> tuple :terms empty?))


(defn- unify-tuple-sizes
  "
  @return
    True if the sizes of the given tuples are the same.
    False otherwise.
  "
  [left right]
  (= (-> left :terms count)
     (-> right :terms count)))


(defn- unify-tuples-elements
  "
  Tells whether each pair of terms from the two tuples unify.
  It's important that both tuples have the same sizes.

  @return
    The new environment if each pair of Terms unify.
    Nil otherwise.
  "
  [left right env]
  (loop [left (:terms left)
         right (:terms right)
         env env]
    (if (empty? left)
      env
      (when-let [env (.unify (first left)
                             (first right)
                             env)]
        (recur (next left)
               (next right)
               env)))))


(defn- unify-tuples
  "
  @return
    The new environment if the Tuples unify.
    Nil otherwise.
  "
  [left right env]
  (and (unify-tuple-sizes left right)
       (unify-tuples-elements left right env)))


(defn- unify-tuple-and-term
  "
  A Tuple can only be unified with another Tuple.
  "
  [left right env]
  (and (tuple-term? right)
       (unify-tuples left right env)))


(defn- tuple->string
  "
  @return
    The string representation of the Tuple:
    ([<arg1>[, <arg2>[, ...]]])
  "
  [tuple env]
  (->> (:terms tuple)
       (map #(.to-string % env))
       (clojure.string/join ", ")
       (format "(%s)")))


(defn- generate-tuple
  "
  @return
    A newly generated Tuple instance.
    All Terms with the same name in the original Tuple
    will have the same names in the new Tuple.
  "
  [tuple pool]
  (loop [result []
         terms (:terms tuple)
         pool pool]
    (if (empty? terms)
      [(->Tuple result) pool]
      (let [[term pool] (.generate (first terms) pool)]
        (recur (conj result term)
               (next terms)
               pool)))))
