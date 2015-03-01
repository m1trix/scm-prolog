(ns logic.environment)


(defn create-env []
  (atom {}))


(defn clone-env [env]
  (atom @env))


(defn get-root
  [env name]
  (let [[parent value] (@env name)]
    (if (nil? parent)
     name
     (let [new-parent (get-root env parent)]
       (swap! env assoc name [new-parent value])
       new-parent))))


(defn bind-names
  [env name-x name-y]
  (swap! env
         assoc
         (get-root env name-x)
         [(get-root env name-y) nil])
  env)


(defn bind-value
  [env name value]
  (swap! env
         assoc
         (get-root env name)
         [nil value])
  env)


(defn get-value
  [env name]
  (->> name
       (get-root env)
       (@env)
       second))
