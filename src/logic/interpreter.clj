(ns logic.interpreter
  (:use logic.term
        logic.environment
        logic.stack))


(def built-in-terms {"dog" [(create [:fact "dog" ["lasie"]])
                            (create [:fact "dog" ["rex"]])
                            (create [:fact "dog" ["bethoven"]])]
                     "cat" [(create [:fact "cat" ["tom"]])
                            (create [:fact "cat" ["garfield"]])]})


(defmulti get-name #(type %))


(defmethod get-name logic.term.PrologAtom
  [a]
  (:name a))


(defmethod get-name logic.term.PrologFact
  [fact]
  (-> fact :atom :name))


(defn get-matches
  [term]
  (or (built-in-terms (get-name term)) []))


(defmulti resolve (fn [a & _] (type a)))


(defmethod resolve :default
  [term in-env start stack]
  (let [matches (get-matches term)
        length (count matches)]
    (loop [env in-env
           index start]
      (if (>= index length)
        [false nil in-env]
        (let [elem (.generate (nth matches index)
                              (create-env))
              new-env (clone-env env)
              result (.unify term elem new-env)]
          (if (true? result)
            (do
              (when (< (inc index) length)
                (push-frame! stack [term in-env (inc index)]))
              [result nil new-env])
            (recur env (inc index))))))))


(defmethod resolve logic.term.PrologConjunction
  [conj env start stack]
  (if (-> conj :terms empty?)
    [true nil env]
    (let [[proven? new-query new-env]
          (resolve (-> conj :terms first)
                   (clone-env env)
                   start
                   stack)]
      (cond
       (not proven?)
       [false nil env]

       (nil? new-query)
       [true (logic.term.PrologConjunction. (-> conj :terms rest)) new-env]

       :else
       [true (logic.term.PrologConjunction. (-> conj :terms rest (conj new-query))) new-env]))))



(defn prove
  ([query]
   (prove query (create-env) (create-stack) 0))
  ([in-query in-env stack in-start]
   (lazy-seq
    (loop [query in-query
           env in-env
           start in-start]
      (let [[proven? new-query new-env]
            (resolve query env start stack)]
        (if proven?
          (if (stack-empty? stack)
            (cons [true new-env] '())
            (let [[old-query old-env old-start]
                  (pop-frame! stack)]
              (cons [true new-env]
                    (prove old-query old-env stack old-start))))
          (if (stack-empty? stack)
            (cons [false env] '())
            (let [[old-query old-env old-start]
                  (pop-frame! stack)]
              (recur old-query old-env old-start)))))))))


(defn print-answers
  [all-names answers]
  (loop [names all-names]
    (when-not (empty? names)
      (let [var-name (first names)
            value (get-value answers var-name)]
        (when-not (nil? value)
          (print (str var-name " = " (.to-string value answers) ", "))))
      (recur (rest names))))
  (flush))


(defn interactive-prove
  [query]
  (let [names (.names-set query)]
    (loop [answers (prove query)]
      (if-not (empty? answers)
        (let [[proven? env] (first answers)]
          (when proven?
            (print-answers names env)
            (read-line))
          (recur (rest answers)))
        (println "false.")))))
