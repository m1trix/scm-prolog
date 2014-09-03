(ns logic.parser-test
  (:use logic.parser
        clojure.test))

(deftest test-parser

  (testing "Parsing operators"
    (is (= ["operator" " rest"] (find-word-operator "operator rest")))
    (is (= ["->" "atom"] (find-symbol-operator "->atom")))
    (is (= [":-" "Var"] (find-operator ":-Var")))

    (is (= ["" " text"] (find-operator " text")))
    (is (= ["" "=>"] (find-operator "=>")))

    (is (= ["." "and more"] (find-operator ".and more")))
    (is (= ["," "and more"] (find-operator ",and more"))))

  (testing "Shunting-Yard"
    (is (= [[{:op "," :arity 2}] []] (add-operation [] [] ",")))))
