(ns logic.term
  (:use logic.util))


(defprotocol IPrologTerm
  (unify [this other pool])
  (to-string [this pool])
  (generate [this names]))


(load "term/variable")
(load "term/atom")
(load "term/arguments")


(def create-mappings
  {:var #(create-var %)
   :atom #(create-atom %)
   :args #(create-args-list %)})


(defn create
  "Creates a new PrologTerm."
  [type & args]
  ((create-mappings type) args))


(.unify (create :args (list (create :var "Y")
                            (create :atom "asd")))
        (create :args (list (create :var "X")
                            (create :atom "asd")))
        {})

(.generate (create :args (list (create :var "X")
                               (create :var "Y")
                               (create :var "X")
                               (create :atom "asd")))
           {})
