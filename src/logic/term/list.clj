(in-ns 'logic.term)


(load "term/arguments")


(declare list->string)
(declare list-unify)
(declare generate-list)


(defrecord PrologList [#^clojure.lang.PersistentList head
                       tail]
  IPrologTerm
  (to-string [this pool] (list->string this pool))
  (unify [this other pool] (list-unify this other pool))
  (generate [this names] (generate-list this names)))



(defn create-tail
  [tail]
  (cond

   (empty? tail)
   nil

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
     (PrologList. (map create tail) nil)

     :else
     (PrologList. (map create head)
                  (create-tail (rest tail))))))


(defn prolog-list?
  [term]
  (instance? PrologList term))


(defn list->string
  [l env]
  (let [builder (StringBuilder.)
        append-term #((append-next env ", ") builder %)
        append-tail #((append-next env " | ") builder %)]
    (append-first builder (:head l) env "[")
    (loop [head (-> l :head rest)
           tail (-> l :tail)]
      (if (empty? head)
        (cond

         (nil? tail)
         true

         (prolog-list? tail)
         (do
           (append-first builder (:head tail) env ", ")
           (recur (-> tail :head rest) (:tail tail)))

         :else
         (append-tail tail))

        (do
          (-> head first append-term)
          (recur (rest head) tail))))
    (.append builder "]")
    (.toString builder)))
