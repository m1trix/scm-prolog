;; (ns logic.interpreter-test
;;   (:use logic.interpreter
;;         logic.term
;;         logic.parser
;;         clojure.test)
;;   (:refer-clojure :exclude [resolve replace]))


;; (deftest test-interpreter

;;   (testing "Proving Terms"

;;     (is (= [true {} []]
;;            (prove (create "notrace") (create "notrace") {} [] 0 0)))

;;     (is (= [true {} []]
;;            (prove (create [:fact "is" [3 3]])
;;                   (create [:fact "is" [3 3]])
;;                   {} [] 0 0)))

;;     (is (= [true {} []]
;;            (prove (create [:conj "notrace" "notrace"])
;;                   (create [:conj "notrace" "notrace"])
;;                   {} [] 0 0)))

;;     (is (= [true {} [[(create [:disj "trace"]) {} 0]]]
;;            (prove (create [:disj "notrace" "trace"])
;;                   (create [:disj "notrace" "trace"])
;;                   {} [] 0 0)))

;;     (is (= [true {(create "X") (create "number")} []]
;;            (prove (first (parse "X = number.")) (first (parse "X = number.")) {} [] 0 0)))


;;     (is (= [true {} ()]
;;            (prove (first (parse "not(3 > 3).")) (first (parse "not(3 > 3).")) {} [] 0 0)))))
