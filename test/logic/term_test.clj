(ns logic.term-test
  (:use logic.term
        clojure.test))


(deftest atoms-test

  (testing "Creating Atoms."
    (is (= (logic.term.PrologAtom. "x") (create :atom "x")))
    (is (= (logic.term.PrologAtom. "'a name of an atom'") (create :atom "'a name of an atom'")))
    (is (thrown? Exception (create :atom "A lot of things"))))

  (testing "Unifying Atoms"
    (is (= (unify (create :atom "atom") (create :atom "atom") {}) [true {}]))
    (is (= (unify (create :atom "'atom'") (create :atom "atom") {}) [true {}]))
    (is (= (unify (create :atom "atom") (create :atom "'atom'") {}) [true {}]))
    (is (= (unify (create :atom "'atom'") (create :atom "'atom'") {}) [true {}]))
    (is (= (unify (create :atom "atom") (create :atom "anotherAtom") {}) [false {}]))
    (is (= (unify (create :atom "atom") (create :atom "'another atom'") {}) [false {}]))
    (is (= (unify (create :atom "'atom'") (create :atom "anotherAtom") {}) [false {}]))
    (is (= (unify (create :atom "'atom'") (create :atom "'anotherAtom'") {}) [false {}]))))


(deftest variables-test

  (testing "Creating Variables"
    (is (= (logic.term.PrologVariable. "Variable") (create :var "Variable")))
    (is (thrown? Exception (create :var "invalid_name"))))

  (testing "When unifiyng two unbound variables, the left becomes a root: { Right -> Left }."
    (is (= (unify (create :var "Left") (create :var "Right") {})
           [true {(create :var "Right") (create :var "Left")}])))

  (testing "When unifying a root variable with a variable, the left becomes the new root and the right tree is flattened:
    { Y->X ; Z->Y ; V->Z ; T->W }; W ~ V => { Y->X ; Z->X ; V->X ; X->W ; T->W }."
    (is (= (unify (create :var "W") (create :var "V")
                  {(create :var "Y") (create :var "X")
                   (create :var "Z") (create :var "Y")
                   (create :var "V") (create :var "Z")
                   (create :var "T") (create :var "W")})
           [true {(create :var "Y") (create :var "X")
                               (create :var "Z") (create :var "X")
                               (create :var "V") (create :var "X")
                               (create :var "T") (create :var "W")
                               (create :var "X") (create :var "W")}])))

  (testing "When variables are from the same bounds tree, they are not bound again."
    (is (= (unify (create :var "X") (create :var "Y")
                  {(create :var "X") (create :var "Root")
                   (create :var "Y") (create :var "Root")})
           [true {(create :var "X") (create :var "Root")
                                  (create :var "Y") (create :var "Root")}]))))
