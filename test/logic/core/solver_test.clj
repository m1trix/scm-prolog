(ns logic.core.solver-test
  (:use logic.core.solver
        logic.core.db
        logic.core.term
        logic.core.env
        clojure.test)
  (:refer-clojure
    :exclude [create-ns]))


(deftest solving-atoms
  (testing "Matching only with atoms"
    (let [db
          (->
            (create-db)
            (create-ns "test")
            (add-term "test" "atom" (create-atom "atom"))
            (add-term "test" "atom" (create-atom "atom2"))
            (db-view "test"))]
      (is (= 1
             (-> "atom" (create-atom) (solve db) count)))
      (is (empty? (-> "atom2" (create-atom) (solve db))))))

  (testing "Matching with facts & rules"
    (let [db
          (->
            (create-db)
            (create-ns "test")
            (add-term "test" "atom" (create-atom "atom"))
            (add-term "test" "atom" (create-fact "atom" []))
            (add-term "test" "atom" (create-fact "atom" ["X"]))
            (db-view "test"))]
      (is (= 2
             (-> "atom" (create-atom) (solve db) count)))
      (is (empty? (-> "atom2" (create-atom) (solve db)))))))


(deftest solving-rules
  (let [db (->
             (create-db)
             (create-ns "test")
             (add-term "test" "atom" (create-atom "atom"))
             (add-term "test" "atom" (create-fact "atom" []))
             (add-term "test" "atom" (create-fact "atom" ["X"]))
             (add-term "test" "atom" (create-rule "atom" [] "atom2"))
             (add-term "test" "atom" (create-rule "atom" ["X"] "atom2"))
             (add-term "test" "atom" (create-rule "atom" [["a" "b"] ["c" "d" "e"]] "atom2"))
             (add-term "test" "atom2" (create-atom "atom2"))
             (db-view "test"))]

    (testing "Matching when rule has no parameters"
      (is (= 3 (-> (solve (create-fact "atom" []) db)
                   count))))

    (testing "Matching when rule has parameters"
      (is (= 2 (-> (solve (create-fact "atom" [["item_1"]]) db)
                    count))))

    (testing "Matching when rule has complex parameters"
      (let [env (-> (create-fact "atom" ["A" ["B" :| "C"]])
                    (solve db)
                    first)]
        (is (= (create-list ["a" "b"])
               (env-get env "A")))
        (is (= (create-atom "c")
               (env-get env "B")))
        (is (= (create-list ["d" "e"])
               (env-get env "C")))))))
