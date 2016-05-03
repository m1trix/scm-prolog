(ns logic.term-test
  (:use logic.env
        logic.term
        clojure.test))


(deftest test-variable
  (testing "#create-var"
    (is (= (->Variable "_")
           (create-var "_")))
    (is (= (->Variable "Variable")
           (create-var "Variable")))
    (is (= (->Variable "Aa1_")
           (create-var "Aa1_")))
    (is (thrown? IllegalArgumentException
                 (create-var "variable"))))

  (testing "#generate"
    (let [pool {"Variable" "RANDOM"}]
      (is (not= (create-var "_")
                (-> (create-var "_")
                    (.generate pool)
                    first)))
      (is (= (create-var "RANDOM")
             (-> (create-var "Variable")
                 (.generate pool)
                 first)))
      (is (not= (create-var "RANDOM")
                (-> (create-var "V")
                    (.generate pool)
                    first)))
      (is (= 2
          (-> (create-var "V")
              (.generate pool)
              second
              count)))))

  (testing "#to-string"
    (is (= "Variable"
           (-> (create-var "Variable")
               (.to-string (env-create))))))

  (testing "#unify"
    (let [env (env-create)]
      (is (true?
          (->> env
              (.unify (create-var "X")
                      (create-var "Y"))
              first)))
      (is (-> (.unify (create-var "X")
                      (create-var "Y")
                      env)
              second
              (env-bound? "X" "Y")))
      (let [env (-> env
                    (env-bind "X" "Y")
                    (env-set "X" 42))]
        (is (= 42 (env-get env "Y")))
        (is (true?
            (->> env
                 (.unify (create-var "Z") 
                         (create-var "X"))
                 first)))

        (is (= 42
               (-> (.unify (create-var "Z")
                           (create-var "X")
                           env)
                   second
                   (env-get "Z"))))
        (is (true?
            (->> env
                 (.unify (create-var "X")
                         (create-var "V"))
                 first)))))))


(deftest test-atom

  (testing "#create-atom"
    (is (= (->Atom "aT12mM")
           (create-atom "aT12mM")))
    (is (= (->Atom "'atom with wh@th3vEr__symbols!'")
           (create-atom "'atom with wh@th3vEr__symbols!'")))
    (is (= (->Atom "a_T_0_m")
           (create-atom "a_T_0_m")))
    (is (thrown? IllegalArgumentException
                 (create-atom "'atom with unfinished quoting")))
    (is (thrown? IllegalArgumentException
                 (create-atom "'atom with''too much qouting'")))
    (is (thrown? IllegalArgumentException
                 (create-atom "atom with N0 Quoting"))))

  (testing "#to-string"
    (is (= "'atom n@m3'"
           (.to-string (create-atom "'atom n@m3'")
                       (env-create))))
    (is (= "aT_"
           (.to-string (create-atom "aT_")
                       (env-create)))))

  (testing "#unify"
    (is (true? (->> (env-create)
                    (.unify (create-atom "atom")
                            (create-atom "atom"))
                    first)))
    (is (true? (->> (env-create)
                    (.unify (create-atom "atom")
                            (create-atom "'atom'"))
                    first)))
    (is (true? (->> (env-create)
                    (.unify (create-atom "'atom'")
                            (create-atom "atom"))
                    first)))
    (is (true? (->> (env-create)
                    (.unify (create-atom "'atom'")
                            (create-atom "'atom'"))
                    first)))
    (is (false? (->> (env-create)
                     (.unify (create-atom "atom_")
                             (create-atom "atom"))
                     first)))
    (is (false? (->> (env-create)
                     (.unify (create-atom "'atom '")
                             (create-atom "'atom'"))
                     first)))
    (is (true? (->> (env-create)
                    (.unify (create-var "Variable")
                            (create-atom "atom"))
                    first)))
    (is (= {"Variable" (create-atom "atom")}
           (->> (env-create)
                (.unify (create-var "Variable")
                        (create-atom "atom"))
                second
                :values)))
    (is (true? (->> (env-create)
                    (.unify (create-atom "atom")
                            (create-var "Variable"))
                    first)))
    (is (= {"Variable" (create-atom "atom")}
           (->> (env-create)
                (.unify (create-atom "atom")
                        (create-var "Variable"))
                second
                :values)))))
