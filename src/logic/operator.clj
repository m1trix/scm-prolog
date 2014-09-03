(ns logic.operator
  (:use [logic.term])
  (:refer-clojure :exclude [resolve]))


(defrecord PrologOperator [prec name left right])


(defn create-operator [prec type name]
  (cond

   (= "xf" type)  (PrologOperator. prec name :less :none)
   (= "yf" type)  (PrologOperator. prec name :same :none)
   (= "xfx" type) (PrologOperator. prec name :less :less)
   (= "xfy" type) (PrologOperator. prec name :less :same)
   (= "yfx" type) (PrologOperator. prec name :same :less)
   (= "fx" type)  (PrologOperator. prec name :none :less)
   (= "fy" type)  (PrologOperator. prec name :none :same)
   :else (throw (Exception. (str "Unknown Operator type: " type ".")))))


(def built-in-unary
  {"." (create-operator 9001 "xf" ".")})

(def built-in-binary
  {":-" (create-operator 1200 "xfx" ":-")
   "," (create-operator 1000 "xfy" ",")
   ";" (create-operator 1100 "xfy" ";")})


(def operators-binary (atom built-in-unary))
(def operators-unary (atom built-in-binary))


(defn operator-arity
  "Returns the arity of a Prolog Operator (1 or 2)."
  [op]
  (if (or (= :none (:left op))
          (= :none (:right op)))
    1
    2))


(defn prolog-operator?
  "Returns wheater there is an Operator with that name."
  [name]
  (if (and (nil? (@operators-unary name))
           (nil? (@operators-binary name)))
    false
    true))


(defn make-fact [op args]
  (->PrologFact (-> op :name ->PrologAtom) (->PrologArguments args)))


(defn get-binary [name]
  (get @operators-binary name))


(defn get-unary [name]
  (get @operators-unary name))