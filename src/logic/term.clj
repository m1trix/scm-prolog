(ns logic.term)


(defprotocol IPrologTerm
  (create [args])
  (unify [this other pool])
  (to-string [this pool])
  (generate [this names]))


(load "term/variable")
(load "term/atom")


(def create-mappings
  {:var #(create-var %)
   :atom #(create-atom %)})


(defn create
  "Creates a new PrologTerm."
  [type & args]
  ((create-mappings type) args))


;; (defmulti generate (fn [term _] (type term)))
;; (defmulti get-vars type)
;; (defmulti reshape (fn [term _] (type term)))
;; (defmulti create (fn [inp & rest] (type inp)))
;; (defmulti resolve (fn [x y _] [(type x) (type y)]))
;; (defmulti get-name (fn [term] (type term)))
