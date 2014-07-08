(ns logic.core
  (:use [logic.util]
        [logic.term]
        [logic.interpreter]))



(def list01(>list< [:A :A :C]))
(def list02(>list< [:X 2 3]))

(def uni (unify-lists list01 list02 {}))

(defn -main
  "Entry Point."
  [& args]
  (println "\u001b[33mWellcome to SCM-Prolog! Have fun :) \u001b[0m \n")

  (println-blue (output-term (>variable< :X (>atom< :pesho))))
  (println-blue (output-term (>variable< :X (>string< "I am so sexy!"))))
  (println-blue (output-term (>variable< :X (>variable< :Y))))
  (println-blue (output-term (>variable< :X (>number< 42))))
  (println-blue (output-term (>variable< :X (>list< [1 :pesho :Y :| [:Z :ivo :| [ "gosho" :| []]]]))))
  (println-blue (output-term (first uni)))
  (println (second uni))




  (let [[name pool] (unify-variables
                     (>variable< :X)
                     (>variable< :Y)
                     {})]
    (print-blue(output-term name)))

  (let [input (atom "")]
    (while (different? @input "exit")
      (do
        (print "?- \u001b[33m")
        (flush)
        (reset! input (read-line)))))
  (println "\u001b[33mHope to see you again soon! \u001b[0m \n"))
