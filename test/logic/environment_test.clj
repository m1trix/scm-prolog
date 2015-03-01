(ns logic.environment-test
  (:use logic.environment
        clojure.test))


(deftest environment-functions-test

  (testing "Binding names"
    (let [env (create-env)]
      (is (= "name" (get-root env "name")))
      (bind-names env "name" "root")
      (is (= "root" (get-root env "name")))))

  (testing "Binding values"
    (let [env (create-env)]
      (-> env
          (bind-names "X" "Y")
          (bind-names "Z" "X")
          (bind-names "W" "V")
          (bind-names "T" "W"))
      (is (= "Y" (get-root env "Y")))
      (is (= "Y" (get-root env "X")))
      (is (= "Y" (get-root env "Z")))
      (is (= "V" (get-root env "V")))
      (is (= "V" (get-root env "W")))
      (is (= "V" (get-root env "T")))

      (bind-names env "T" "X")

      (is (= "Y" (get-root env "T")))
      (is (= "Y" (get-root env "X")))
      (is (= "Y" (get-root env "Y")))
      (is (= "Y" (get-root env "V")))
      (is (= "Y" (get-root env "W")))
      (is (= "Y" (get-root env "Z"))))))
