(ns logic.term-test
  (:use logic.env
        logic.term
        clojure.test))


(deftest test-variable
  (testing "#var-create"
    (is (= (->Variable "_")
           (var-create "_")))
    (is (= (->Variable "Variable")
           (var-create "Variable")))
    (is (= (->Variable "Aa1_")
           (var-create "Aa1_")))
    (is (thrown? IllegalArgumentException
                 (var-create "variable"))))

  (testing "#generate"
    (let [env (-> (env-create)
                  (env-set "Variable" "RANDOM"))]
      (is (not= (var-create "_")
                (-> (var-create "_")
                    (.generate env)
                    first)))
      (is (= (var-create "RANDOM")
             (-> (var-create "Variable")
                 (.generate env)
                 first)))
      (is (not= (var-create "RANDOM")
                (-> (var-create "V")
                    (.generate env)
                    first)))
      (is (= 2
          (-> (var-create "V")
              (.generate env)
              second
              count)))))

  (testing "#to-string"
    (is (= "Variable"
           (-> (var-create "Variable")
               (.to-string (env-create))))))

  (testing "#unify"
    (let [env (env-create)]
      (is (true?
          (->> env
              (.unify (var-create "X")
                      (var-create "Y"))
              first)))
      (is (-> (.unify (var-create "X")
                      (var-create "Y")
                      env)
              second
              (env-bound? "X" "Y")))
      (let [env (-> env
                    (env-bind "X" "Y")
                    (env-set "X" 42))]
        (is (= 42 (env-get env "Y")))
        (is (true?
            (->> env
                 (.unify (var-create "Z") 
                         (var-create "X"))
                 first)))

        (is (= 42
               (-> (.unify (var-create "Z")
                           (var-create "X")
                           env)
                   second
                   (env-get "Z"))))
        (is (true?
            (->> env
                 (.unify (var-create "X")
                         (var-create "V"))
                 first)))))))
