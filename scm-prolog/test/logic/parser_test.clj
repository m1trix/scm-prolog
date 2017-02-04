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



  (testing "Parsing Atoms"
    (is (= ["atom" "."] (find-atom-single "atom.")))
    (is (= ["" "'atom'."] (find-atom-single "'atom'.")))
    (is (= ["'atom'" "."] (find-atom-multi "'atom'.")))
    (is (= ["" "atom."] (find-atom-multi "atom.")))
    (is (= ["atom" "."] (find-atom "atom.")))
    (is (= ["'atom'" "."] (find-atom "'atom'."))))


  (testing "Parsing Numbers"
    (is (= ["11" "+2"] (find-number "11+2")))
    (is (= ["1" ",2"] (find-number "1,2")))
    (is (= ["6.6" ",2"] (find-number "6.6,2")))
    (is (= ["6.6" ".2"] (find-number "6.6.2"))))


  (testing "Parsing Variables and Strings"
    (is (= ["Var" "."] (find-variable "Var.")))
    (is (= ["" "atom."] (find-variable "atom.")))
    (is (= ["\"atom\"" "."] (find-string "\"atom\".")))
    (is (= ["" "'atom'."] (find-string "'atom'."))))


  (testing "Parsing Lists"
    (is (= [[1 2 3] "."] (extract-list "[1,2,3].")))
    (is (= [[1 2 "|" "X"] "."] (extract-list "[1,2|X].")))
    (is (= [[1 2 "|" []] "."] (extract-list "[1,2|[]].")))
    (is (= [[1 2 "|" [3]] "."] (extract-list "[1,2|[3]].")))
    (is (= [[] "."] (extract-list "[]."))))


  (testing "Parsing Arguments"
    (is (= [(create-arguments ["a" "X" 3 "\"str\"" []]) "."]
           (extract-arguments "(a, X, 3, \"str\", [])."))))


  (testing "Parsing Complex Terms"
    (is (= [(create-fact ["fact" ["atom" "Var"]])]
           (parse "fact(atom, Var).")))

    (is (= [(create-rule ["demo" ["atom" ["element"]]
                          [:fact "demo" ["element"]]])]
           (parse "demo(atom, [element]) :- demo(element).")))

    (is (= [(create [:conj [:fact "fact" ["A" "B"]]
                           [:fact "fact" ["A" "C"]]])]
           (parse "fact(A, B), fact(A, C).")))

    (is (= [(create [:disj [:fact "fact" ["A" "B"]]
                           [:fact "fact" ["A" "C"]]])]
           (parse "fact(A, B); fact(A, C).")))

    (is (= [(create [:not [:fact "is" ["X" 5]]])]
           (parse "not(X is 5).")))))
