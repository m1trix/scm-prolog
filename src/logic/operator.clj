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
  {":-"  (create-operator 1200 "fx" ":-")
   "?-"  (create-operator 1200 "fx" "?-")

   "dynamic"  (create-operator 1150 "fx" "dynamic")
   "discontiguous"  (create-operator 1150 "fx" "discontiguous")
   "initialization"  (create-operator 1150 "fx" "initialization")
   "module_transparent"  (create-operator 1150 "fx" "module_transparent")
   "multifile"  (create-operator 1150 "fx" "multifile")
   "thread_local"  (create-operator 1150 "fx" "thread_local")
   "volatile"  (create-operator 1150 "fx" "volatile")

   "\\+" (create-operator 900 "fy" "?\\+")
   "~"   (create-operator 900 "fx" "~")

   "+"   (create-operator 500 "fx" "+")
   "-"   (create-operator 500 "fx" "-")
   "?"   (create-operator 500 "fx" "?")
   "\\"  (create-operator 500 "fx" "\\")
   "not" (create-operator 1   "fx" "not")})


(def built-in-binary
  {":-"  (create-operator 1200 "xfx" ":-")
   "-->" (create-operator 1200 "xfx" "-->")
   ";"   (create-operator 1100 "xfy" ";")
   "|"   (create-operator 1100 "xfy" "|")
   "->"  (create-operator 1050 "xfy" "->")
   ","   (create-operator 1000 "xfy" ",")
   "\\"  (create-operator 954  "xfy" "\\")

   "<"   (create-operator 700  "xfx" "<")
   "="   (create-operator 700  "xfx" "=")
   "=.." (create-operator 700  "xfx" "=..")
   "=@=" (create-operator 700  "xfx" "=@=")
   "=:=" (create-operator 700  "xfx" "=:=")
   "=<"  (create-operator 700  "xfx" "=<")
   "=="  (create-operator 700  "xfx" "==")
   "=\\=" (create-operator 700  "xfx" "=\\=")
   ">"   (create-operator 700  "xfx" ">")
   ">="  (create-operator 700  "xfx" ">=")
   "@<"  (create-operator 700  "xfx" "@<")
   "@=<" (create-operator 700  "xfx" "@=<")
   "@>"  (create-operator 700  "xfx" "@>")
   "@>=" (create-operator 700  "xfx" "@>=")
   "\\="  (create-operator 700  "xfx" "\\=")
   "\\==" (create-operator 700  "xfx" "\\==")
   "is"  (create-operator 700  "xfx" "is")

   ":"   (create-operator 600  "xfy" ":")

   "+"   (create-operator 500  "yfx" "+")
   "-"   (create-operator 500  "yfx" "-")
   "/\\" (create-operator 500  "yfx" "/\\")
   "\\/" (create-operator 500  "yfx" "\\/")
   "xor" (create-operator 500  "yfx" "xor")

   "*"   (create-operator 400  "yfx" "*")
   "/"   (create-operator 400  "yfx" "/")
   "//"  (create-operator 400  "yfx" "//")
   "rdiv" (create-operator 400  "yfx" "rdiv")
   "<<"  (create-operator 400  "yfx" "<<")
   ">>"  (create-operator 400  "yfx" ">>")
   "mod" (create-operator 400  "yfx" "mod")
   "rem" (create-operator 400  "yfx" "rem")

   "**"  (create-operator 400  "xfx" "**")
   "^"   (create-operator 400  "xfy" "^")})


(def operators-binary (atom built-in-binary))
(def operators-unary (atom built-in-unary))



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
  (->PrologFact (-> op :name ->PrologAtom)
                (->PrologArguments args)))


(defn get-binary [name]
  (let [op (get @operators-binary name)]
    (if (nil? op)
      (throw (Exception. (str "There is no binary operator \"" name "\".")))
      op)))


(defn get-unary [name]
  (let [op (get @operators-unary name)]
    (if (nil? op)
      (throw (Exception. (str "There is no unary operator \"" name "\".")))
      op)))
