(in-ns 'logic.term)
(use 'clojure.set)


(load "term/common")
(load "term/null")


(declare list->string)
(declare list-unify)
(declare generate-list)
(declare list-names-set)


(defrecord PrologList [#^clojure.lang.PersistentList head
                       #^logic.term.IPrologTerm tail]
  IPrologTerm
  (to-string [this pool] (list->string this pool))
  (unify [this other pool] (list-unify this other pool))
  (generate [this names] (generate-list this names))
  (names-set [this] (list-names-set this)))


(defn prolog-list?
  [term]
  (instance? PrologList term))


(defn create-tail
  [tail]
  (cond

   (empty? tail)
   (PrologNull.)

   (> 1 (count tail))
   (throw (IllegalArgumentException. (str "PrologList tail must be a single PrologTerm!")))


   :else
   (let [term (-> tail first create)]
     (if (or (prolog-var? term) (prolog-list? term))
       term
       (throw (IllegalArgumentException. (str "PrologList tail cannot be of " (type term))))))))


(defn create-list
  [elems]
  (let [pos (.indexOf elems :|)
        [head tail] (split-at pos elems)]
    (cond

     (< 1 (count (filter #(= :| %) elems)))
     (throw (IllegalArgumentException. (str "Cannot create PrologList with multiple tails!")))

     (= 0 (count head))
     (PrologList. (map create tail) (PrologNull.))

     :else
     (PrologList. (map create head)
                  (create-tail (rest tail))))))


(defn nested-list?
  [term env]
  (if (prolog-list? term)
    true
    (and (prolog-var? term)
         (prolog-list? (get-val term env)))))


(defn nested-head
  [term env]
  (cond
   (prolog-list? term)
   (:head term)))


(defn nested-tail
  [term env]
  (cond
   (prolog-list? term)
   (:tail term)))


(defn list->string
  [l env]
  (let [builder (StringBuilder. "[")]
    (loop [head (:head l)
           tail (:tail l)]
      (append-coll builder head env ", ")
      (cond

       (nested-list? tail env)
       (do
         (.append builder ", ")
         (recur (nested-head tail env)
                (nested-tail tail env)))

       (prolog-var? tail)
       (append-next builder tail env " | ")))
    (.append builder "]")
    (.toString builder)))


(defn generate-list
  [l pool]
  (PrologList. (map #(.generate % pool)
                    (:head l))
               (.generate (:tail l) pool)))


(defn unify-lists
  [x y env]
  (loop [head-x (:head x)
         tail-x (:tail x)
         head-y (:head y)
         tail-y (:tail y)]
    (cond

     (> (count head-x) (count head-y))
     (recur head-y tail-y head-x tail-x)

     (not-empty head-x)
     (if (.unify (first head-x) (first head-y) env)
       (recur (rest head-x) tail-x (rest head-y) tail-y)
       false)

     (empty? head-y)
     (.unify tail-x tail-y env)

     (nested-list? tail-x env)
     (recur (nested-head tail-x env)
            (nested-tail tail-x env)
            head-y
            tail-y)

     (prolog-var? tail-x)
     (.unify tail-x (PrologList. head-y tail-y))

     :else false)))


(defn list-unify
  [l term env]
  (cond

   (prolog-list? term)
   (unify-lists l term env)

   (prolog-var? term)
   (.unify term l env)

   :else false))


(defn list-names-set
  [l]
  (->> (map #(.names-set %) (:head l))
       (reduce union (.names-set (:tail l)))))
