(ns logic.util)


(def color-red 31)
(def color-yellow 33)


(defn print-color [s c]
  (if (= "Linux" (System/getProperty "os.name"))
    (print (format "\u001b[1;%dm%s\u001b[0m" c s))
    (print s)))


(defn println-color [s c]
  (print-color s c)
  (println))
