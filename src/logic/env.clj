(ns logic.env
  (:use clojure.pprint))


(defrecord EnvNode [parent value])


(defn env-create
  "Creates an empty environment"
  []
  {:names {}
   :values {}})


(defn- env-assoc-parent
  [env name parent]
  (if (= name parent)
    env
    {:values (:values env)
     :names (assoc (:names env)
                   name
                   parent)}))


(defn- env-assoc-value
  "Changes the value associated with the given name."
  [env name value]
  {:names (:names env)
   :values (assoc (:values env)
                  name 
                  value)})


(defn env-get-root
  "Returns the root name from the current binding tree."
  [env name]
  (let [parent ((env :names) name)]
    (if (nil? parent)
      [name env]
      (let [[root env]
            (env-get-root env parent)]
        [root
         (env-assoc-parent
           env
           name
           root)]))))


(defn env-bind
  "Adds the left name to the binding tree of the right name."
  [env left right]
  (let [[left-root env]
        (env-get-root env left)

        [right-root env]
        (env-get-root env right)]
    (env-assoc-parent
      env
      left-root
      right-root)))


(defn env-bound?
  "Tells whether the two names are bound together or not."
  [env left right]
  (= (first (env-get-root env left))
     (first (env-get-root env right))))


(defn env-get
  "Returns the value associated to the given name."
  [env var]
  (let [[root _]
        (env-get-root env var)]
    ((env :values) root)))


(defn env-set
  "Associates the value to the given name and all names bound to it."
  [env var val]
  (let [[root env] (env-get-root env var)]
    (-> env
        (env-assoc-value root val)
        (env-assoc-parent var root))))
