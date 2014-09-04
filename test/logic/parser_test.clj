(ns logic.parser-test
  (:use logic.parser
        clojure.test))

(deftest test-parser

  (testing "Parsing Operators"
    (is (= ["operator" " rest"] (find-word-operator "operator rest")))
    (is (= ["->" "atom"] (find-symbol-operator "->atom")))
    (is (= [":-" "Var"] (find-operator ":-Var")))

    (is (= ["" " text"] (find-operator " text")))
    (is (= ["" "=>"] (find-operator "=>")))

    (is (= ["." "and more"] (find-operator ".and more")))
    (is (= ["," "and more"] (find-operator ",and more"))))

  (testing "Parsing Terms"

    (is (= [[1 "Var" "atom" "\"string\""] "."] (extract-list "[1,Var, atom, \"string\" ].")))
    (is (= [[1 2 3 "|" [4 5]] "."] (extract-list "[1,2,3 | [4, 5]].")))
    (is (= [["A" "|" "X"] "."] (extract-list "[A | X]."))))

  (testing "Shunting-Yard"
    (is (= [[{:op "," :arity 2}] []] (add-operation [] [] ",")))))
