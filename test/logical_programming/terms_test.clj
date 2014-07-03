(ns logical-programming.terms-test)

(use 'clojure.test)
(use '[logical-programming.terms :as LP])

(deftest test-terms
  (testing "Type checking"
    (is (true? (pl-atom?      (->PL-Atom :petko))))
    (is (true? (pl-number?    (->PL-Number 1))))
    (is (true? (pl-structure? (->PL-Structure :member [(->PL-Number 1)]))))
    (is (true? (pl-variable?  (->PL-Variable :X nil []))))))
