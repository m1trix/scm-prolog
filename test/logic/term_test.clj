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

  (testing "#variable?"
    (is (true? (variable? (create-var "X"))))
    (is (false? (variable? 33))))

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
    (is (true?
        (->> (env-create)
             (.unify (create-var "X")
                     (create-var "Y"))
             first)))
    (is (-> (.unify (create-var "X")
                    (create-var "Y")
                    (env-create))
            second
            (env-bound? "X" "Y")))
    (let [env (-> (env-create)
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
          (->> (env-set env "X" (create-atom "atom"))
               (.unify (create-var "X")
                       (create-var "V"))
               first))))))


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

  (testing "#atom?"
    (is (false? (atom? (create-var "X"))))
    (is (true? (atom? (create-atom "atom")))))

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


(deftest test-tuple
  (testing "#create-tuple"
    (is (= (->Tuple [(create-atom "atom")
                     (create-var "VAR")])
           (create-tuple ["atom" "VAR"])))
    (is (= (->Tuple [])
           (create-tuple []))))

  (testing "#tuple?"
    (is (false? (tuple? (create-var "X"))))
    (is (true? (tuple? (create-tuple ["atom" "X"])))))

  (testing "#to-string"
    (is (= "(atom, Variable, (inner, (tuple)))"
           (-> (->Tuple [(create-atom "atom")
                         (create-var "Variable")
                         (->Tuple [(create-atom "inner")
                                   (create-tuple ["tuple"])])])
               (.to-string (env-create)))))
    (is (= "()" (.to-string (create-tuple [])
                            (env-create))))
    (is (= "(atom)" (.to-string (create-tuple ["atom"])
                                (env-create)))))

  (testing "#generate"
    (let [pool {"Var1" "Result1", "Var2" "Result2"}]
      (is (= (create-tuple ["Result1" "Result1" "atom"])
             (-> (create-tuple ["Var1" "Var1" "atom"])
                 (.generate pool)
                 first)))
      (is (= (create-tuple ["Result1" "Result2" "atom"])
             (-> (create-tuple ["Var1" "Var2" "atom"])
                 (.generate pool)
                 first)))))

  (testing "#unify"
    (let [env (env-create)]
      (is (true?
           (-> (create-tuple ["atom" "VAR1" "VAR2"])
               (.unify (create-tuple ["atom" "VAR1" "VAR2"])
                       env)
               first)))
      (is (true?
           (-> (create-tuple ["VAR1" "VAR2" "VAR3"])
               (.unify (create-tuple ["atom" "VAR1" "VAR2"])
                       env)
               first)))
      (is (= (create-atom "atom")
             (-> (create-tuple ["VAR1" "VAR2" "VAR3"])
                 (.unify (create-tuple ["atom" "VAR1" "VAR2"])
                         env)
                 second
                 (env-get "VAR3"))))
      (is (false?
           (-> (create-tuple ["VAR1" "VAR1"])
               (.unify (create-tuple ["atom" "another"])
                       env)
               first)))
      (is (false?
           (-> (create-tuple ["atom" "atom"])
               (.unify (create-tuple ["atom"])
                       env)
               first)))
      (is (true?
           (-> (create-tuple ["VAR1" "VAR2"])
               (.unify (create-tuple ["VAR2" "VAR1"])
                       env)
               first)))
      (is (-> (create-tuple ["VAR1" "VAR2"])
              (.unify (create-tuple ["VAR2" "VAR1"])
                      env)
              second
              (env-bound? "VAR1" "VAR2"))))))
