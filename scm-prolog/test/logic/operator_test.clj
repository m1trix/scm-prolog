(ns logic.operator-test
  (:use logic.operator
        clojure.test))


(deftest test-operator

  (testing "Getting Operators"
    (is (= (get @operators-binary ",") (get-binary ",")))
    (is (= (get @operators-binary "-->") (get-binary "-->")))
    (is (= (get @operators-unary ":-") (get-unary ":-")))
    (is (= (get @operators-unary "-") (get-unary "-"))))

  (testing "Creating Operators"
    (is (= (->PrologOperator 1200 ":-" :less :less)
           (create-operator 1200 "xfx" ":-")))
    (is (= (->PrologOperator 1200 ":-" :none :less)
           (create-operator 1200 "fx" ":-"))))

  (testing "Geting Operator arity"
    (is (= 1 (operator-arity (create-operator 9001 "xf" "."))))
    (is (= 2 (operator-arity (create-operator 42 "xfy" "plus")))))

  (testing "Checking for Operator"
    (is (true? (prolog-operator? ",")))
    (is (true? (prolog-operator? ":-")))
    (is (false? (prolog-operator? "%")))
    (is (false? (prolog-operator? ".")))))
