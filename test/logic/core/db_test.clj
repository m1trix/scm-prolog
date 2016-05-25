(ns logic.core.db-test
  (use logic.core.db
       logic.core.term
       clojure.test)
  (:refer-clojure :exclude [create-ns]))


(deftest db-test
  
  (testing "Creating empty database"
    (is (= {}
           (create-db))))

  (testing "Creating a new namespace"
    (is (= {"list" {}}
           (-> (create-db)
               (create-ns "list")))))

  (testing "Creating a new namespace, when already added"
    (is (thrown? IllegalArgumentException
           (-> (create-db)
               (create-ns "list")
               (create-ns "list")))))

  (testing "Deleting a non-existing namespace"
    (is (thrown? IllegalArgumentException
           (-> (create-db)
               (drop-ns "list")))))

  (testing "Deleting an existing namespace"
    (is (= {}
           (-> (create-db)
               (create-ns "list")
               (drop-ns "list")))))

  (testing "Adding terms to non-existing namespace"
    (is (thrown? IllegalArgumentException
           (-> (create-db)
               (add-term "ns" "term" (create-atom "atom"))))))

  (testing "Adding a term to an empty namespace"
    (is (= {"demo" {"term" [(create-atom "atom")]}}
           (-> (create-db)
               (create-ns "demo")
               (add-term "demo" "term" (create-atom "atom"))))))

  (let [db (-> (create-db)

               (create-ns "1st")
               (add-term "1st" "term" (create-atom "atom"))
               (add-term "1st" "term" (create-atom "atom2"))
               (add-term "1st" "term" (create-atom "atom3"))

               (create-ns "2nd")
               (add-term "2nd" "term" (create-atom "atom4"))
               (add-term "2nd" "term2" (create-atom "atom5")))]

    (testing "Adding a term to a non-empty namespace"
      (is (= db
             {"2nd" {"term" [(create-atom "atom4")]
                     "term2" [(create-atom "atom5")]}
              "1st" {"term" [(create-atom "atom")
                              (create-atom "atom2")
                              (create-atom "atom3")]}})))

    (testing "Getting a view of a DB namespace"
      (let [op (db-view db "2nd")]
        (is (= [(create-atom "atom4")]
               (db-get op "term")))
        (is (= [(create-atom "atom5")]
               (db-get op "term2")))
        (is (= [] (db-get op "term3")))))

    (testing "Creating a DB operator"
      (let [func (fn [db name]
                   (let [result (db-get (db-view db "1st") name)]
                     (if (empty? result)
                       (db-get (db-view db "2nd") name)
                       result)))
            op (db-operator db func)]
        (is (= (db-get op "term")
               [(create-atom "atom")
                (create-atom "atom2")
                (create-atom "atom3")]))
        (is (= (db-get op "term2")
               [(create-atom "atom5")])))))) 
