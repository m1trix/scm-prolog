(ns logic.interpreter
  (use logic.term))


(def built-in-terms {"exit" [(create [:fact "exit" []])]
                     "cat" [(create [:fact "cat" ["tom"]])
                            (create [:fact "cat" ["garfield"]])]})


(defmulti get-name #(type %))


(defmethod get-name logic.term.PrologAtom
  [a]
  (:name a))


(defmethod get-name logic.term.PrologFact
  [fact]
  (:name (:atom fact)))


(defmacro get-matches
  [term]
  `(let [all# (built-in-terms (get-name ~term))]
     (if (nil? all#)
       []
       all#)))


(defmulti resolve (fn [a & _] (type a)))


(defmethod resolve :default
  [term in-pool start]
  (let [matches (get-matches term)
        length (count matches)]
    (loop [pool in-pool, index start]
      (if (>= index length)
        [false in-pool]
        (let [[elem _] (.generate (first matches) {})
              [result new-pool] (.unify term elem pool)]
          (if (true? result)
            [true new-pool]
            (recur new-pool
                   (inc index))))))))


(defn prove
  [in-query]
  (let [[_ names] (generate in-query {})]
    (loop [query in-query
           pool {}]
      (let [[result new-query] (resolve query pool)]))))
