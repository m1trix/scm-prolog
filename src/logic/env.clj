(ns logic.env)


(defrecord EnvNode [parent value])


(defn env-create
  "Creates an empty environment"
  []
  (atom {}))


(defn env-clone
  "Creates a copy of the given environment"
  [env]
  (atom @env))


(defn- env-assoc-parent
  "Changes the name's parent if it's not the name itself."
  [env name parent]
  (when-not (= name parent)
    (swap! env assoc name (->EnvNode parent nil)))
  env)


(defn- env-assoc-value
  "Changes the value associated with the given name."
  [env name value]
  (swap! env assoc name (->EnvNode nil value))
  env)


(defn env-get-root
  [env name]
  (let [parent (-> name (@env) :parent)]
    (if (nil? parent)
      name
      (let [root (env-get-root env parent)]
        (env-assoc-parent env name root)
        root))))


(defn env-bind
  "Binds the two names together - both will be associated to the same value."
  [env left right]
  (let [left-root
        (env-get-root env left)

        right-root
        (env-get-root env right)]
    (env-assoc-parent
      env
      left-root
      right-root)))


(defn env-get
  "Returns the value associated to the given name."
  [env var]
  (let [root (env-get-root env var)]
    (-> root (@env) :value)))


(defn env-set
  "Associates the value to the given name and all names bound to it."
  [env var val]
  (let [root (env-get-root env var)]
    (env-assoc-value env root val)
    (env-assoc-parent env var root)))
