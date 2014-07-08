(ns logic.term-test
  (:require [logic.term :refer :all]))

(use 'clojure.test)

(deftest test-terms

  (testing "Type checking"
    (is (true? (prolog-atom?      (>atom< :atom))))
    (is (true? (prolog-number?    (>number< 42))))
    (is (true? (prolog-variable?  (>variable< :X))))
    (is (true? (prolog-string?    (>string< "string")))))

  (testing "Unifying terms"
    (is (= (unify-numbers (>number< 42)
                          (>number< 42)
                          {})
           [(>number< 42) {}]))
    (is (= (unify-numbers (>number< 1)
                          (>number< 2)
                          {})
           [false {}]))
    (is (= (unify-atoms (>atom< :example)
                        (>atom< :example)
                        {})
           [(>atom< example) {}]))
    (is (= (unify-atoms (>atom< :example)
                        (>atom< :exampl)
                        {})
           [false {}]))))
