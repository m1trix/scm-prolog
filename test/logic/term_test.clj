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
                    (.generate env))))
      (is (= (var-create "RANDOM")
             (-> (var-create "Variable")
                 (.generate env))))
      (is (not= (var-create "RANDOM")
                (-> (var-create "V")
                    (.generate env))))
      (is (= 2 (count @env)))))

  (testing "#to-string"
    (is (= "Variable"
           (-> (var-create "Variable")
               (.to-string (env-create))))))

  (testing "#unify"
    (let [env (env-create)]
      (is (= true
             (.unify (var-create "X")
                     (var-create "Y")
                     env)))
      (is (env-bound? env "X" "Y"))

      (env-set env "X" 42)
      (is (= 42
             (env-get env "Y")))

      (is (= true
             (.unify (var-create "Z") 
                     (var-create "X")
                     env)))
      (is (= 42 (env-get env "Z")))

      (is (= true
             (.unify (var-create "X")
                     (var-create "V")
                     env))))))
