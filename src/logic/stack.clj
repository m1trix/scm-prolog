(ns logic.stack)


(defn create-stack []
  (atom []))


(defn push-frame!
  [stack frame]
  (swap! stack conj frame)
  stack)


(defn pop-frame!
  [stack]
  (let [frame (peek @stack)]
    (swap! stack pop)
    frame))


(defn stack-empty?
  [stack]
  (empty? @stack))
