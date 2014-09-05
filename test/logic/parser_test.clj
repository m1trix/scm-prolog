(ns logic.parser-test
  (:use logic.parser
        logic.term
        clojure.test)
  (:refer-clojure :exclude [resolve]))

(deftest test-parser

  (testing "Parsing Operators"
    (is (= ["operator" " rest"] (find-word-operator "operator rest")))
    (is (= ["->" "atom"] (find-symbol-operator "->atom")))

    (is (= [":-" "Var"] (find-operator ":-Var")))
    (is (= ["" " text"] (find-operator " text")))
    (is (= ["" "=>"] (find-operator "=>")))

    (is (not= ["." "and more"] (find-operator ".and more")))
    (is (= ["," "and more"] (find-operator ",and more"))))


  (testing "Parsing Numbers"
    (is (= ["11" "+2"] (find-number "11+2")))
    (is (= ["1" ",2"] (find-number "1,2")))
    (is (= ["6.6" ",2"] (find-number "6.6,2")))
    (is (= ["6.6" ".2"] (find-number "6.6.2"))))

  (testing "Parsing Lists"
    (is (= [(create [1 2 3])] (parse "[1,2,3].")))
    (is (= [(create [1 2 "|" "X"])] (parse "[1,2|X].")))
    (is (= [(create [1 2 "|" []])] (parse "[1,2|[]].")))
    (is (= [(create [1 2 "|" [3]])] (parse "[1,2|[3]].")))
    (is (= [(create [])] (parse "[].")))))
