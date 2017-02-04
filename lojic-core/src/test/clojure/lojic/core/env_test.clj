(ns lojic.core.env-test
  (:use lojic.core.env
        clojure.test))

(deftest test-environment

  (testing "#env-get & #env-set"
    (is (nil? (-> (env-create)
                  (env-get "X"))))
    (is (= :expected
           (-> (env-create)
               (env-set "X" :expected)
               (env-get "X"))))
    (is (= {"X" :x "Y" :y}
           (-> (env-create)
               (env-set "X" :x)
               (env-set "Y" :y)
               :values)))
    (is (nil? (-> (env-create)
                  (env-get "X")))))

  (testing "#env-bind"
    (is (= {"X" "Y"}
           (-> (env-create)
               (env-bind "X" "Y")
               :names)))
    (is (= {"X" "Y", "V" "T"
            "W" "T", "Y" "T"}
           (-> (env-create)
               (env-bind "X" "Y")
               (env-bind "V" "W")
               (env-bind "W" "T")
               (env-bind "X" "V")
               :names)))

    (let [env (-> (env-create)
                  (env-bind "X" "Y")
                  (env-bind "V" "W")
                  (env-bind "X" "V")
                  (env-set "W" :value))]
      (is (= :value
             (env-get env "X")))
      (is (= env {:names {"X" "Y", "Y" "W", "V" "W"}
                  :values {"W" :value}}))
      (is (= :value
             (-> env
                 (env-bind "T" "X")
                 (env-get "T"))))
      (is (= {:values {"W" :value}
              :names {"X" "W", "Y" "W", "V" "W", "T" "W"}}
             (env-bind env "T" "X"))))))
