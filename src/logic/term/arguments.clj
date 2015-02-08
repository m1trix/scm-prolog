;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;    PrologArgsList
;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;  The PrologArgsList represents a fixed size
;;  list of comma-separated IPrologTerms.
;;  +++++++++++++++++++++++++++++++++++++++++++++++
(in-ns 'logic.term)


(declare args->string)
(declare args-unify)
(declare generate-args)


(defrecord PrologArgsList [#^clojure.lang.PersistentList args]
  IPrologTerm
  (to-string [this pool] (args->string (:args this) pool))
  (unify [this other pool] (args-unify this other pool))
  (generate [this names] (generate-args (:args this) names)))


(defn prolog-args-list?
  "Tells whether the IPrologTerm is a PrologArgsList."
  [term]
  (instance? PrologArgsList term))


(defn create-args-list
  "Creates new PrologArgsList."
  [terms]
  (PrologArgsList. (map create terms)))


;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;    args->string:
;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;  The function produces a String that represents
;;  the output format of a PrologArgsList.
;;
;;  The function uses Java StringBuilder to build
;;  the String more effectivelly.
;;  +++++++++++++++++++++++++++++++++++++++++++++++
(defn append-first
  "Appends the first element of a sequence (if such exists)."
  [builder elems env prefix]
  (if (empty? elems)
    builder
    (-> builder
        (.append prefix)
        (.append (.to-string (first elems) env)))))


(defn append-next
  "Returns a two-arguments function that receives a StringBuilder and an IPrologTerm.
  When called, the function appends the string representation of the term to the builder."
  [pool prefix]
  (fn [builder elem]
    (-> builder
        (.append prefix)
        (.append (.to-string elem pool)))))


(defn args->string
  "Returns a string that represents the PrologArgsList."
  [args env]
  (let [builder (StringBuilder.)]
    (-> (reduce (append-next env ", ")
                (append-first builder args env "(")
                (rest args))
        (.append ")")
        (.toString))))


(defn generate-next
  "Returns a single argument function, which receives
  an IPrologTerm and generates new IPrologTerm."
  [names]
  (fn [term]
    (let [[new-term new-names] (.generate term @names)]
      (reset! names new-names)
      new-term)))


(defn generate-args
  [args names]
  (let [atom-names (atom names)]
    (-> (map (generate-next atom-names) args)
        (doall)
        (PrologArgsList.)
        (vector @atom-names))))


;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;    args-unify
;;  +++++++++++++++++++++++++++++++++++++++++++++++
;;  PrologArgsList can be unified only with
;;  PrologVariable or another PrologArgsList.
;;
;;  If the PrologVariable is not bound, it is bound
;;  to the list, otherwise they not unify.
;;
;;  If the other IPrologTerm is PrologVariable,
;;  they unify if each pair of thei elements unify.
;;  +++++++++++++++++++++++++++++++++++++++++++++++

(defn unify-next
  "Unifies the next pair of elements."
  [[result pool] [left right]]
  (if-not result
    [false pool]
    (.unify left right pool)))


(defn unify-arg-lists
  "Unifies two lists by unifying each pair of elements."
  [left right pool]
  (cond

   (not= (count left) (count right))
   [false pool]

   (= 0 (count left))
   [true pool]

   :else
   (->> (map #(vector %1 %2) left right)
        (reduce unify-next [true pool]))))


(defn args-unify
  [args term pool]
  (cond
   (prolog-var? term)
   (.unify term args pool)

   (prolog-args-list? term)
   (unify-arg-lists (:args args) (:args term) pool)

   :else [false pool]))
