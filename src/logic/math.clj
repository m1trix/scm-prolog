(ns logic.math
  (:use logic.term)
  (:refer-clojure :exclude [resolve replace]))



(def built-in-math {})



(defmulti calculate (fn [term _] (type term)))


(defmethod calculate logic.term.PrologNumber
  [number pool]
  number)

(defmethod calculate logic.term.PrologVariable
  [var pool]
  (let [val (get-val var pool)]
    (if (or (set? val)
            (nil? val))
      (throw (Exception. (str "Cannot evaluate unbound Variable \"" (:name var) "\".")))
      (calculate val pool))))


(defmethod calculate logic.term.PrologFact
  [fact pool]
  (let [name (-> fact :atom :name)
        target (built-in-math name)]
    (if (nil? target)
      (throw (Exception. (str "Cannot execute \"" name "\".")))
      (loop [all target]
        (if (empty? all)
          (throw (Exception. (str "No operator matches \"" name "\".")))
          (let [form (first all)
                new-fact (->PrologFact (:atom fact)
                                       (->PrologArgsList (mapv #(calculate % pool)
                                                                (-> fact :args :args))))
                [status new-term _] (resolve new-fact form pool)]
            (if (true? status)
              new-term
              (recur (rest all)))))))))



(defn math-add [[left right] pool]
  [(create (+ (:value left) (:value right))) pool])

(defn math-plus [[term] pool]
  [(create (+ (:value term))) pool])

(defn math-sub [[left right] pool]
  [(create (- (:value left) (:value right))) pool])

(defn math-minus [[term] pool]
  [(create (- (:value term))) pool])

(defn math-mul [[left right] pool]
  [(create (* (:value left) (:value right))) pool])

(defn math-div [[left right] pool]
  [(create (/ (:value left) (:value right))) pool])



(defn math-unify [[left right] pool]
  (let [[new-term new-pool] (unify left right pool)]
    (if (false? new-term)
      [false pool]
      [new-term new-pool])))


(defn math-is [[left right] pool]
  (cond

   (prolog-number? left)
   (let [[answer _] (math-unify [left (calculate right pool)] pool)]
     [answer pool])

   (prolog-var? left)
   (unify left (calculate right pool) pool)

   :else
   [false pool]))


(defn math-less [[left right] pool]
  (if (< (:value (calculate left pool))
         (:value (calculate right pool)))
    [true pool]
    [false pool]))


(defn math-less-eq [[left right] pool]
  (if (<= (:value (calculate left pool))
          (:value (calculate right pool)))
    [true pool]
    [false pool]))


(defn math-more [[left right] pool]
  (if (> (:value (calculate left pool))
         (:value (calculate right pool)))
    [true pool]
    [false pool]))


(defn math-more-eq [[left right] pool]
  (if (>= (:value (calculate left pool))
         (:value (calculate right pool)))
    [true pool]
    [false pool]))


(defn math-eq [[left right] pool]
  (if (= (:value (calculate left pool))
         (:value (calculate right pool)))
    [true pool]
    [false pool]))


(defn math-not-eq [[left right] pool]
  (if-not (= (:value (calculate left pool))
             (:value (calculate right pool)))
    [true pool]
    [false pool]))




(def built-in-math
   {"+" [(create [:form "'+'" ["Left" "Right"] math-add])
         (create [:form "'+'" ["Term"] math-plus])]
    "-" [(create [:form "'-'" ["Left" "Right"] math-sub])
         (create [:form "'-'" ["Term"] math-minus])]
    "*" [(create [:form "'*'" ["Left" "Right"] math-mul])]
    "/" [(create [:form "'/'" ["Left" "Right"] math-div])]
    "=" [(create [:form "'='" ["Left" "Right"] math-unify])]
    "is" [(create [:form "is" ["Left" "Right"] math-is])]

    "<" [(create [:form "'<'" ["Left" "Right"] math-less])]
    "=<" [(create [:form "'=<'" ["Left" "Right"] math-less-eq])]
    ">" [(create [:form "'>'" ["Left" "Right"] math-more])]
    ">=" [(create [:form "'>='" ["Left" "Right"] math-more-eq])]
    "=\\=" [(create [:form "'=\\='" ["Left" "Right"] math-not-eq])]
    "=:=" [(create [:form "'=:='" ["Left" "Right"] math-eq])]})
