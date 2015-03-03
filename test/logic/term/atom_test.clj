(ns logic.term.atom-test
  (:use logic.term
        logic.environment
        clojure.test))

(deftest create-atom-test

  (is (= (create "singleWordName")
         (logic.term.PrologAtom. "singleWordName")))

  (is (= (create "'multi word name'")
         (logic.term.PrologAtom. "'multi word name'")))

  (is (thrown? Exception (create "invalid multi word atom name"))))


(deftest prolog-atom?-test

  (is (true? (prolog-atom? (create "atom")))))


(deftest atom->stirng-test

  (is (= (.to-string (create "singleWordAtom")
                     (create-env))
         "singleWordAtom"))

  (is (= (.to-string (create "'multi word atom'")
                     (create-env))
         "'multi word atom'")))


(deftest unify-atoms-test

  (is (true? (.unify (create "singleWordAtom")
                     (create "singleWordAtom")
                     (create-env))))

  (is (true? (.unify (create "singleWordAtom")
                     (create "'singleWordAtom'")
                     (create-env))))

  (is (true? (.unify (create "'singleWordAtom'")
                     (create "singleWordAtom")
                     (create-env))))

  (is (true? (.unify (create "'multi words atom'")
                     (create "'multi words atom'")
                     (create-env))))

  (is (false? (.unify (create "atom")
                      (create "anotherAtom")
                      (create-env))))

  (is (false? (.unify (create "atom")
                      (create "'another atom'")
                      (create-env))))

  (is (false? (.unify (create "'atom'")
                      (create "anotherAtom")
                      (create-env))))

  (is (false? (.unify (create "'atom'")
                      (create "'another atom'")
                      (create-env)))))
