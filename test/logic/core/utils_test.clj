(ns logic.core.utils-test
  (use logic.core.utils
       clojure.test))

(deftest test-utils

  (testing "#illegal-argument"
    (is (thrown? IllegalArgumentException
                 (illegal-argument "test"))))

  (testing "#deep-concat"
    (is (= [1 2 3 4]
           (deep-concat [[1] [2 3] [] [4]]))))

  (testing "#deep-concat lazyness"
    (let [result
          (deep-concat [[1] [2 3] [] (repeatedly #(inc 3))])]

      ;; #repeatedly creates an endless sequense
      ;; so these cases will not work if #deep-concat
      ;; is not lazy

      (is (= [1 2 3 4 4 4]
             (take 6 result)))
      (is (= [2 3 4 4 4 4]
             (take 6 (next result)))))))
