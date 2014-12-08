(ns logic.term-test
  (:use logic.term
        clojure.test))


(deftest test-atoms-creation

  (testing "When trying to create PrologAtoms with correct names, the atoms are created."
    (is (= (->PrologAtom "x") (create :atom "x")))
    (is (= (->PrologAtom "'a name of an atom'") (create :atom "'a name of an atom'"))))

  (testing "When trying to create PrologAtom with invalid name, exception is thrown"
    (is (thrown? Exception (create :atom "A lot of things"))))

  (testing "Unify atoms"
    (is (= (unify-terms (create :atom "atom") (create :atom "atom") {}) [(create :atom "atom") {}]))
    (is (= (unify-terms (create :atom "'atom'") (create :atom "atom") {}) [(create :atom "atom") {}]))
    (is (= (unify-terms (create :atom "atom") (create :atom "'atom'") {}) [(create :atom "atom") {}]))
    (is (= (unify-terms (create :atom "atom") (create :atom "anotherAtom") {}) [false {}]))
    (is (= (unify-terms (create :atom "atom") (create :atom "'another atom'") {}) [false {}]))
    (is (= (unify-terms (create :atom "'atom'") (create :atom "anotherAtom") {}) [false {}]))))

