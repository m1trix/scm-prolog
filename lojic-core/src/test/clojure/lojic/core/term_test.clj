(ns lojic.core.term-test
  (:use lojic.core.env
        lojic.core.term
        lojic.core.utils
        clojure.test))


(deftest test-variable
  (testing "Creating random variable"
    (is (= (->Variable "_")
           (create-var "_"))))

  (testing "Creating a variable with letters for name"
    (is (= (->Variable "Variable")
           (create-var "Variable"))))

  (testing "Creating a variable for all kinds of symbols for name"
    (is (= (->Variable "Aa1_")
           (create-var "Aa1_"))))

  (testing "Creating a variable with name starting wit a small letter"
    (is (thrown? IllegalArgumentException
                 (create-var "variable"))))

  (testing "#var-term?"
    (is (true? (var-term? (create-var "X"))))
    (is (false? (var-term? 33))))

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

  (testing "#to-string of a Variable with no value"
    (is (= "Variable"
           (-> (create-var "Variable")
               (.to-string (env-create))))))

  (testing "#to-string of a Variable with a value"
    (let [env (-> (env-create)
                  (env-set "Variable" (create-atom "atom")))]
      (is (= "atom"
             (-> (create-var "Variable")
                 (.to-string env))))))

  (testing "Unifying Variables with no values."
    (let [result
          (->> (env-create)
               (.unify (create-var "X")
                       (create-var "Y")))]
      (-> result nil? not is)
      (is (env-bound? result "X" "Y"))))

  (testing "Unifying Variables, where one has a value"
    (let [env (-> (env-create)
                  (env-bind "X" "Y")
                  (env-set "X" (create-atom "atom")))]
      (-> (.unify
            (create-var "Z")
            (create-var "X")
            env)
          nil? not is)

      (is (= (create-atom "atom")
             (-> (.unify (create-var "Z")
                         (create-var "X")
                         env)
                 (env-get "Z"))))

      (is (->> (env-set env "X" (create-atom "diff"))
               (.unify (create-var "X")
                       (create-var "V"))
               nil?
               not)))))


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

  (testing "#atom-term?"
    (is (false? (atom-term? (create-var "X"))))
    (is (true? (atom-term? (create-atom "atom")))))

  (testing "#to-string"
    (is (= "'atom n@m3'"
           (.to-string (create-atom "'atom n@m3'")
                       (env-create))))
    (is (= "aT_"
           (.to-string (create-atom "aT_")
                       (env-create)))))

  (testing "Unifying atoms"
    (is (not (nil?
               (.unify (create-atom "atom")
                       (create-atom "atom")
                       (env-create)))))
    (is (not (nil?
               (.unify (create-atom "atom")
                       (create-atom "'atom'")
                       (env-create)))))
    (is (not (nil?
               (.unify (create-atom "'atom'")
                       (create-atom "atom")
                       (env-create)))))
    (is (not (nil?
               (.unify (create-atom "'atom'")
                       (create-atom "'atom'")
                       (env-create)))))
    (is (nil?
         (.unify (create-atom "atom_")
                 (create-atom "atom")
                 (env-create))))
    (is (nil?
         (.unify (create-atom "'atom '")
                 (create-atom "'atom'")
                 (env-create)))))

  (testing "Unifying Atom and Variable"
    (is (= (create-atom "atom")
           (->> (env-create)
                (.unify (create-atom "atom")
                        (create-var "VAR"))
                (get-var-value (create-var "VAR")))))
    (is (= (create-atom "atom")
           (->> (env-create)
                (.unify (create-var "VAR")
                        (create-atom "atom"))
                (get-var-value (create-var "VAR")))))
    (is (= (create-atom "'atom'")
           (->> (env-create)
                (set-var-value (create-var "VAR")
                               (create-atom "'atom'"))
                (.unify (create-atom "atom")
                        (create-var "VAR"))
                (get-var-value (create-var "VAR")))))
    (is (= (create-atom "'atom'")
           (->> (env-create)
                (set-var-value (create-var "VAR")
                               (create-atom "'atom'"))
                (.unify (create-var "VAR")
                        (create-atom "atom"))
                (get-var-value (create-var "VAR")))))))


(deftest test-tuple

  (testing "#create-tuple"
    (is (= (->Tuple [(create-atom "atom")
                     (create-var "VAR")])
           (create-tuple ["atom" "VAR"])))
    (is (= (->Tuple [])
           (create-tuple []))))

  (testing "#tuple-term?"
    (is (false? (tuple-term? (create-var "X"))))
    (is (true? (tuple-term? (create-tuple ["atom" "X"])))))

  (testing "#to-string"
    (is (= "(atom, Variable, (inner, (tuple)))"
           (-> (->Tuple [(create-atom "atom")
                         (create-var "Variable")
                         (->Tuple [(create-atom "inner")
                                   (create-tuple ["tuple"])])])
               (.to-string (env-create)))))
    (is (= "()"
           (.to-string
             (create-tuple [])
             (env-create))))
    (is (= "(atom)"
           (.to-string
             (create-tuple ["atom"])
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

  (testing "Unifying tuples when all elements match"
    (is (= (env-create)
           (.unify
             (create-tuple ["atom" "VAR1" "VAR2"])
             (create-tuple ["atom" "VAR1" "VAR2"])
             (env-create)))))

  (testing "Unifying tuples when variables have to bind"
    (let [env (.unify
                (create-tuple ["VAR1" "VAR2" "VAR3"])
                (create-tuple ["atom" "VAR1" "VAR2"])
                (env-create))]
      (is (env-bound? env "VAR1" "VAR2"))
      (is (env-bound? env "VAR1" "VAR3"))))

  (testing "Unifying tuples when variables have to be evaluated"
    (let [env (.unify
                (create-tuple ["VAR1" "VAR2" "VAR3"])
                (create-tuple ["atom" "VAR3" "VAR1"])
                (env-create))]
      (is (= (create-atom "atom")
             (env-get env "VAR1")))
      (is (= (create-atom "atom")
             (env-get env "VAR2")))
      (is (= (create-atom "atom")
             (env-get env "VAR3")))))
  
  (testing "Unifying tuples when terms don't unify"
    (is (falsy? (.unify
                   (create-tuple ["VAR1" "VAR1"])
                   (create-tuple ["atom" "another"])
                   (env-create)))))

  (testing "Unifying tuples when number of elements don't match"
    (is (falsy? (.unify
                   (create-tuple ["atom" "atom"])
                   (create-tuple ["atom"])
                   (env-create))))))


(deftest test-fact
  (testing "#create-fact"
    (is (= (->Fact (create-atom "name")
                   (create-tuple ["p1" "P2"]))
           (create-fact "name" ["p1" "P2"]))))

  (testing "#fact-term?"
    (is (-> (create-fact "name" ["p1" "P2"])
            fact-term?)))

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
    (let [env (.unify
                (create-fact "name" ["X" "Y" "atom"])
                (create-fact "name" ["atom" "X" "Y"])
                (env-create))]
      (is (env-bound? env "X" "Y"))
      (is (= (create-atom "atom")
             (env-get env "X")))
      (is (= (create-atom "atom")
             (env-get env "Y")))))

  (testing "When the names are different, then the facts don't unify"
    (is
      (falsy?
        (.unify
          (create-fact "fact1" ["X" "Y"])
          (create-fact "fact2" ["X" "Y"])
          (env-create)))))

  (testing "When the parameters have different count, then the facts don't unify"
    (is
      (falsy?
        (.unify
          (create-fact "fact" ["X"])
          (create-fact "fact" ["X" "Y"])
          (env-create)))))

  (testing "Unifying facts, when evaluation not possible."
    (is
      (falsy?
        (.unify
          (create-fact "fact" ["X" "Y" "X"])
          (create-fact "fact" ["atom1" "atom2" "Y"])
          (env-create))))))


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
      (is
        (falsy?
          (.unify
            test-rule-with-args
            (create-atom "rule")
            (env-create)))))

    (testing "Unifying a Rule and an Atom, when they can unify"
      (->
        (.unify
          test-rule-with-no-args
          (create-atom "rule")
          (env-create))
        falsy? not is))

    (testing "Unifying a Rule and a Fact, when they cannot unify"
      (is
        (falsy?
          (.unify
            test-rule-with-args
            (create-fact "rule" ["X" "Y" "Z"])
            (env-create)))))

    (testing "Unifying a Rule and a Fact, when they can unify"
      (let [env (.unify
                  test-rule-with-args
                  (create-fact "rule" ["A" "atom"])
                  (env-create))]
        (is (env-bound? env "A" "X"))
        (is (= (create-atom "atom")
               (env-get env "Y")))))))


(deftest test-null-term
  (testing "#create & #null-term?"
    (is (null-term? (create-null))))

  (testing "#to-string"
    (is (= "[]"
           (.to-string (create-null)
                       (env-create)))))

  (testing "#generate"
    (let [[new-term _]
          (.generate (create-null)
                     {})]
      (is (null-term? new-term))))

  (testing "Unifying a Null and a Variable"
    (let [env (.unify
                (create-null)
                (create-var "X")
                (env-create))]
      (is (null-term? (env-get env "X"))))))


(deftest test-list-term
  (testing "Creating an empty List"
    (is (null-term? (create-list '()))))

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
    (->
      (.unify
        (create-list [])
        (create-list [])
        (env-create))
      nil? not is))

  (testing "Unifying non-empty lists"
    (let [env (.unify
                (create-list ["a" "B"])
                (create-list ["B" "A"])
                (env-create))]
      (is (= (create-atom "a")
             (env-get env "B")))))

  (testing "Unifying lists where one has variable tail"
    (let [env (.unify
                (create-list ["a" "b" :| "C"])
                (create-list ["a" "b" "c" "d"])
                (env-create))]
    (is (= (create-list ["c" "d"])
           (env-get env "C")))))

  (testing "Unifying lists where sizes are different"
    (->
      (.unify
        (create-list ["a" "b"])
        (create-list ["a" "b" "c"])
        (env-create))
      falsy? is))

  (testing "Unifying lists where a pair ot terms don't unify"
    (->
      (.unify
        (create-list ["a" "c"])
        (create-list ["a" "b" "c"])
        (env-create))
      falsy? is))

  (testing "Unifying a List and a Variable"
    (let [env (.unify
                (create-var "X")
                (create-list ["a" "b"])
                (env-create))]
    (is (= (create-list ["a" "b"])
           (env-get env "X")))))

  (let [pool {"X" "VAR_X", "Y" "VAR_Y"}]
    (testing "Generating an empty List"
      (let [[result _]
            (.generate (create-list []) pool)]
        (is (= (create-list []) result))))

    (testing "Generating a List of non-repeated Variables"
      (let [[result _]
            (.generate
              (create-list ["atom" "X" :| "Y"])
               pool)]
        (is (= (create-list ["atom" "VAR_X" :| "VAR_Y"])
               result))))

    (testing "Generating a List containing repeated Variables"
      (let [[result _]
            (.generate
              (create-list ["atom" "X" "Y" :| "X"])
               pool)]
        (is (= (create-list ["atom" "VAR_X" "VAR_Y" :| "VAR_X"])
               result))))))


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