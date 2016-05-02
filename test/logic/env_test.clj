(ns logic.env-test
  (:use logic.env
        clojure.test))


(defn parent
  [name]
  (->EnvNode name nil))

(defn value
  [value]
  (->EnvNode nil value))


(deftest test-environment

  (testing "#env-get & #env-set"
    (is (nil? (-> (env-create)
                  (env-get "X"))))
    (is (= :expected
           (-> (env-create)
               (env-set "X" :expected)
               (env-get "X"))))
    (is (= {"X" (value :x)
            "Y" (value :y)}
           @(-> (env-create)
                (env-set "X" :x)
                (env-set "Y" :y)))))

  (testing "#env-bind"
    (is (= {"X" (parent "Y")}
           @(-> (env-create)
                (env-bind "X" "Y"))))
    (is (= {"X" (parent "Y")
            "Y" (parent "T")
            "V" (parent "T")
            "W" (parent "T")}
           @(-> (env-create)
                (env-bind "X" "Y")
                (env-bind "V" "W")
                (env-bind "W" "T")
                (env-bind "X" "V"))))
    (let [test-env
          (-> (env-create)
               (env-bind "X" "Y")
               (env-bind "V" "W")
               (env-bind "X" "V")
               (env-set "W" :value))]
      (is (= :value
             (env-get test-env "X")))
      (is (= {"X" (parent "W")
              "Y" (parent "W")
              "V" (parent "W")
              "W" (value :value)}))
      (env-bind test-env "T" "X")
      (is (= :value
             (env-get test-env "T")))
      (is (= {"X" (parent "W")
              "Y" (parent "W")
              "V" (parent "W")
              "T" (parent "W")
              "W" (value :value)})))))
