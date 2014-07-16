(ns logic.math
  [:use [logic.term]
        [logic.util]])


(defn math-add [num-x num-y]
  (if (and (prolog-number? num-x)
           (prolog-number? num-y))
    (>number< (+ (:value num-x)
                 (:value num-y)))
    (throw (Exception. "Operation + requires numbers as arguments!"))))


(defn math-substract [num-x num-y]
  (if (and (prolog-number? num-x)
           (prolog-number? num-y))
    (>number< (- (:value num-x)
                 (:value num-y)))
    (throw (Exception. "Operation + requires numbers as arguments!"))))


(defn math-multiply [num-x num-y]
  (if (and (prolog-number? num-x)
           (prolog-number? num-y))
    (>number< (* (:value num-x)
                 (:value num-y)))
    (throw (Exception. "Operation + requires numbers as arguments!"))))


(defn math-divide [num-x num-y]
  (if (and (prolog-number? num-x)
           (prolog-number? num-y))
    (>number< (/ (:value num-x)
                 (:value num-y)))
    (throw (Exception. "Operation + requires numbers as arguments!"))))


(defn math-method? [method]
  (let [func (:func method)]
    (if (or (same? func math-add)
            (same? func math-substract)
            (same? func math-multiply)
            (same? func math-divide))
      true
      false)))


(defn execute [method pool]
  ((:func method) (:args method) pool))


(defn calculate [term pool]
  (cond
   (math-method? term)
     (let [new-args (map #(calculate % pool) (:args term))]
       (execute (->PrologMethod (:name term)
                                new-args
                                (:func term)) pool))
   (prolog-number? term)
     term
   :else
     (throw (Exception. (str (type term)
                             " is not an arithmetic expression.")))))


(defn math-equals [[term-x term-y] pool]
  (unify-terms term-x term-y pool))


(defn math-is [[term-x term-y] pool]
  (cond

   (prolog-variable? term-x)
     (=variable= term-x (calculate term-y pool) pool)

   (prolog-number? term-x)
     (math-equals term-x
                  (calculate term-y pool))
   :else
   (throw (Exception. "Operator \"is\" can be used only with variable or number!"))))


(def math-functions {:+ (>term< [:#met :+ [:A :B] math-add])
                     :- (>term< [:#met :- [:A :B] math-substract])
                     :* (>term< [:#met :* [:A :B] math-multiply])
                     :is (>term< [:#met :is [:A :B] math-is])
                     := (>term< [:#met := [:A :B] math-equals])})


(unify-terms (>method< [:#met :is ]))
