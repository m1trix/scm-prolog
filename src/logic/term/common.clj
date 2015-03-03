(in-ns 'logic.term)


(defn append-initial
  [builder elems env]
  (if (empty? elems)
    builder
    (.append builder
             (.to-string (first elems) env))))


(defn append-next
  ([builder term env separator]
   ((append-next env separator) builder term))
  ([env separator]
   (fn [builder term]
     (-> builder
         (.append separator)
         (.append (.to-string term env))))))


(defn append-coll
  [builder coll env separator]
  (reduce (append-next env separator)
          (append-initial builder coll env)
          (rest coll)))
