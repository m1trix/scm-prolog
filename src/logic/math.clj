(ns logic.math
  (:use logic.term)
  (:refer-clojure :exclude [resolve replace]))



(defn math-add [[left right] pool]
  [(create (+ (:value left)
             (:value right)))
   pool])



(defn math-unify [[left right] pool]
  (let [[new-term new-pool] (unify left right pool)]
    (if (false? new-term)
      [false pool]
      [true new-pool])))


(def built-in-math
  (atom
   {"+" [(create [:form "'+'" ["Left" "Right"] math-add])]
    "=" [(create [:form "'='" ["Left" "Right"] math-unify])]}))



(defmulti calculate (fn [term _] (type term)))


(defmethod calculate logic.term.PrologNumber
  [number pool]
  [number pool])

(defmethod calculate logic.term.PrologVariable
  [var pool]
  (let [val (extract var pool)]
    (calculate val pool)))

(defmethod calculate logic.term.PrologFact
  [fact pool]
  (let [name (-> fact :atom :name)
        target (@built-in-math name)]
    (if (nil? target)
      (throw (Exception. (str "Unknown operation \"" name "\".")))
      (let [[new-left left-pool] (calculate (-> fact :args :args first) pool)
            [new-right right-pool] (calculate (-> fact :args :args second) left-pool)
            form (first target)
            new-fact (->PrologFact (:atom fact) (->PrologArguments [new-left new-right]))
            [_ new-term new-pool] (resolve new-fact form right-pool)]
        [new-term new-pool]))))


(defn math-is [[left right] pool]
  (cond

   (prolog-number? left)
   (let [[new-number new-pool] (calculate right pool)
         [answer final-pool] (math-unify [left new-number] new-pool)]
     [true final-pool])

   (prolog-variable? left)
   (let [[new-val new-pool] (calculate right pool)]
     (unify left new-val new-pool))

   :else
   (throw (Exception. (str "Operator 'is' cannot work with \"" (type left) "\".")))))


(swap! built-in-math assoc "is" [(create [:form "is" ["Left" "Right"] math-is])])
