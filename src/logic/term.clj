(ns logic.term
  (:require logic.util))


(defmulti to-string (fn [term _] (type term)))
(defmulti unify-terms (fn [T1 T2 _] (type T1) (type T2)))


(load "term/variable")
(load "term/atom")


(defmethod to-string :default
  [what _]
  (str what))


(defmethod unify-terms :default
  [_ _ pool]
  [false pool])


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
