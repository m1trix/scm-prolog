(ns logic.core.term-test
  (:use logic.core.env
        logic.core.term
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


(deftest test-fact
  (testing "#create-fact"
    (is (= (->Fact (create-atom "name")
                   (create-tuple ["p1" "P2"]))
           (create-fact "name" ["p1" "P2"]))))

  (testing "#fact?"
    (is (-> (create-fact "name" ["p1" "P2"])
            fact?)))

  (testing "#to-string of a fact with no parameters"
    (is (= "fact()"
           (.to-string (create-fact "fact" [])
                       (env-create)))))

  (testing "#to-string of a fact with no variables"
    (is (= "fact(atom1, atom2)"
           (.to-string (create-fact "fact" ["atom1", "atom2"])
                       (env-create)))))

  (testing "#to-string of a fact with no variables"
    (let [env (-> (env-create)
                  (env-set "VAR1" (create-atom "atom")))]
      (is (= "fact(atom, VAR2)"
             (.to-string (create-fact "fact" ["VAR1", "VAR2"])
                         env)))))

  (testing "When generating facts, then the repeated variables have the same names"
    (let [[fact pool]
          (.generate (create-fact "name"
                                  ["VAR1" "atom" "VAR2" "VAR1" "V2"])
                     {"VAR1" "V1", "VAR2" "V2"})]
      (is (= fact
             (->Fact
               (->Atom "name")
               (->Tuple [
                  (create-var "V1")
                  (create-atom "atom")
                  (create-var "V2")
                  (create-var "V1")
                  (->Variable (pool "V2"))]))))))

  (testing "When two facts unify, then the variables are evaluated properly"
    (let [[unified? env]
          (.unify (create-fact "name" ["X" "Y" "atom"])
                  (create-fact "name" ["atom" "X" "Y"])
                  (env-create))]
      (is unified?)
      (is (env-bound? env "X" "Y"))
      (is (= (create-atom "atom")
             (env-get env "X")))
      (is (= (create-atom "atom")
             (env-get env "Y")))))

  (testing "When the names are different, then the facts don't unify"
    (let [[unified? _]
          (.unify (create-fact "fact1" ["X" "Y"])
                  (create-fact "fact2" ["X" "Y"])
                  (env-create))]
      (is (not unified?))))

  (testing "When the parameters have different count, then the facts don't unify"
    (let [[unified? _]
          (.unify (create-fact "fact" ["X"])
                  (create-fact "fact" ["X" "Y"])
                  (env-create))]
      (is (not unified?))))

  (testing "Unifying facts, when evaluation not possible."
    (let [[unified? _]
          (.unify (create-fact "fact" ["X" "Y" "X"])
                  (create-fact "fact" ["atom1" "atom2" "Y"])
                  (env-create))]
      (is (not unified?)))))


(deftest test-rule
  (testing "#create-rule"
    (is (= (create-rule "rule" ["atom" "Var"] {:fact ["true" []]})
           (->Rule (->Fact (->Atom "rule")
                           (->Tuple [(->Atom "atom")
                                     (->Variable "Var")]))
                   (->Fact (->Atom "true")
                           (->Tuple []))))))

  (testing "Calling #to-string, when the rule has no variables"
    (is (= "rule(atom1, atom2) :- fact(atom1)"
           (.to-string
             (create-rule "rule"
                          ["atom1", "atom2"]
                          {:fact ["fact" ["atom1"]]})
             (env-create)))))

  (testing "Calling #to-string, when the rule has variables"
    (let [env (-> (env-create)
                  (env-set "X" (create-atom "atom")))]
      (is (= "rule(atom) :- fact(atom, Y, atom)"
             (.to-string
               (create-rule "rule"
                            ["X"]
                            {:fact ["fact" ["X" "Y" "atom"]]})
               env)))))

  (let [pool {"X" "NEW_X", "Y", "NEW_Y"}]
    (testing "Generating a rule"
      (let [[result pool]
            (.generate
              (create-rule "rule" ["X", "Z"]
                           {:fact ["fact" ["atom", "Y", "X"]]})
              pool)]
        (is (= result
               (->Rule (->Fact (->Atom "rule")
                               (->Tuple [(->Variable "NEW_X")
                                         (->Variable (pool "Z"))]))
                       (->Fact (->Atom "fact")
                               (->Tuple [(->Atom "atom")
                                         (->Variable "NEW_Y")
                                         (->Variable "NEW_X")]))))))))

  (let [test-rule-with-no-args
        (create-rule "rule" [] {:fact ["fact" ["Y" "X"]]})

        test-rule-with-args
        (create-rule "rule" ["X" "Y"] {:fact ["fact" ["Y" "X"]]})]

    (testing "Unifying a Rule and an Atom, when they cannot unify"
      (let [[unified? _]
            (.unify test-rule-with-args
                    (create-atom "rule")
                    (env-create))]
        (is (not unified?))))

    (testing "Unifying a Rule and an Atom, when they can unify"
      (let [[unified? _]
            (.unify test-rule-with-no-args
                    (create-atom "rule")
                    (env-create))]
        (is unified?)))

    (testing "Unifying a Rule and a Fact, when they cannot unify"
      (let [[unified? _]
            (.unify test-rule-with-args
                    (create-fact "rule" ["X" "Y" "Z"])
                    (env-create))]
        (is (not unified?))))

    (testing "Unifying a Rule and a Fact, when they can unify"
      (let [[unified? env]
            (.unify test-rule-with-args
                    (create-fact "rule" ["A" "atom"])
                    (env-create))]
        (is unified?)
        (is (env-bound? env "A" "X"))
        (is (= (create-atom "atom")
               (env-get env "Y")))))))


(deftest null-term-test
  (testing "#create & #null?"
    (is (null? (create-null))))

  (testing "#to-string"
    (is (= "[]"
           (.to-string (create-null)
                       (env-create)))))

  (testing "#generate"
    (let [[new-term _]
          (.generate (create-null)
                     {})]
      (is (null? new-term))))

  (testing "Unifying a Null and a Variable"
    (let [[unified? env]
          (.unify (create-null)
                  (create-var "X")
                  (env-create))]
      (is unified?)
      (is (null? (env-get env "X")))))

  (testing "Unifying two Nulls"
    (let [[unified? env]
          (.unify (create-null)
                  (create-null)
                  (env-create))]
      (is unified?))))


(deftest list-term-test
  (testing "Creating an empty List"
    (is (null? (create-list '()))))

  (testing "Creating a List with no tail"
    (is (= (create-list ["a" "b" "c"])
           (->List (->Atom "a")
                   (->List (->Atom "b")
                           (->List (->Atom "c")
                                   (->Null)))))))

  (testing "Creating a List with a List for a tail"
    (is (= (create-list ["a" "b" :| ["c" :| ["d" :| []]]])
           (->List (->Atom "a")
                   (->List (->Atom "b")
                           (->List (->Atom "c")
                                   (->List (->Atom "d")
                                           (->Null))))))))

  (testing "Creating a List with a Variable for a tail"
    (is (= (create-list ["a" "b" :| ["c" :| "X"]])
           (->List (->Atom "a")
                   (->List (->Atom "b")
                           (->List (->Atom "c")
                                   (->Variable "X")))))))

  (testing "Creating a List with composed Lists"
    (is (= (create-list ["a" ["B"] [[]] :| []])
           (->List (->Atom "a")
                   (->List (->List (->Variable "B")
                                   (->Null))
                           (->List (->List (->Null)
                                           (->Null))
                                   (->Null)))))))

  (testing "#to-string of an empty list"
    (is (= "[]"
           (.to-string (create-list [])
                       (env-create)))))

  (testing "#to-string of a List without a tail"
    (is (= "[a, B, [], [c]]"
           (.to-string 
             (create-list ["a" "B" [] ["c"]])
             (env-create)))))

  (testing "#to-string of a List with a List for a tail"
    (is (= "[a, B, [], c]"
           (.to-string 
             (create-list ["a" "B" [:|[]] :| ["c"]])
             (env-create)))))

  (testing
    "
      #to-string of a List with a Variable for a tail,
      when the Variable has no value
    "
    (is (= "[a, B, [] | Tail]"
           (.to-string 
             (create-list (seq ["a" "B" [] :| "Tail"]))
             (env-create)))))

  (testing
    "
      #to-string of a List with a Variable for a tail,
      when the Variable is an empty List
    "
    (is (= "[a, B, []]"
           (.to-string 
             (create-list '("a" "B" [] :| "Tail"))
             (-> (env-create)
                 (env-set "Tail" (create-list [])))))))

  (testing
    "
      #to-string of a List with a Variable for a tail,
      when the Variable is a non-empty List
    "
    (is (= "[a, B, [], c, D | Rest]"
           (.to-string 
             (create-list ["a" "B" [] :| "Tail"])
             (env-set (env-create)
                      "Tail"
                      (create-list ["c" "D" :| "Rest"]))))))

  (testing "#to-string of a List containing variables"
    (is (= "[a, [A, C], [], c, D]"
           (.to-string 
             (create-list ["a" "B" [] :| "E"])
             (-> (env-create)
                 (env-set "B" (create-list ["A" "C"]))
                 (env-set "E" (create-list ["c" "D"])))))))

  (testing "Unifying empty lists"
    (is (true?
          (-> (.unify (create-list [])
                      (create-list [])
                      (env-create))
              first))))

  (testing "Unifying non-empty lists"
    (let [[unified? env]
          (.unify (create-list ["a" "B"])
                  (create-list ["B" "A"])
                  (env-create))]
    (is unified?)
    (is (= (create-atom "a")
           (env-get env "B")))))

  (testing "Unifying lists where one has variable tail"
    (let [[unified? env]
          (.unify (create-list ["a" "b" :| "C"])
                  (create-list ["a" "b" "c" "d"])
                  (env-create))]
    (is unified?)
    (is (= (create-list ["c" "d"])
           (env-get env "C")))))

  (testing "Unifying lists where sizes are different"
    (let [[unified? env]
          (.unify (create-list ["a" "b"])
                  (create-list ["a" "b" "c"])
                  (env-create))]
    (is (not unified?))))

  (testing "Unifying lists where a pair ot terms don't unify"
    (let [[unified? env]
          (.unify (create-list ["a" "c"])
                  (create-list ["a" "b" "c"])
                  (env-create))]
    (is (not unified?))))

  (testing "Unifying a List and a Variable"
    (let [[unified? env]
          (.unify (create-var "X")
                  (create-list ["a" "b"])
                  (env-create))]
    (is unified?)
    (is (= (create-list ["a" "b"])
           (env-get env "X"))))))


(deftest test-create
  (testing "Creating Atoms"
    (is (= (create-atom "a")
           (create-term "a")))
    (is (= (create-atom "'a & %'")
           (create-term "'a & %'"))))

  (testing "Creating Variables"
    (is (= (create-var "V")
           (create-term "V"))))

  (testing "Creating Lists"
    (is (= (create-list [])
           (create-term [])))
    (is (= (create-list '())
           (create-term '())))
    (is (= (create-list (seq ["a"]))
           (create-term (seq ["a"])))))

  (testing "Creating Facts"
    (is (= (create-fact "fact" ["a" "V"])
           (create-term {:fact ["fact" ["a" "V"]]}))))

  (testing "Creating Rules"
    (is (= (create-rule "rule" ["a" "V"]
             {:fact ["fact" []]})
           (create-term {:rule ["rule" ["a" "V"]
                                {:fact ["fact" []]}]})))))