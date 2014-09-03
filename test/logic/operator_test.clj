(ns logic.operator-test
  (:use logic.operator
        clojure.test))

(deftest test-operator

  (testing "Getting operators"
    (is (= (get @operators-binary ",") (get-binary ",")))))
