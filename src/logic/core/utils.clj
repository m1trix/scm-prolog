(ns logic.core.utils)


(defn illegal-argument
  [message]
  (-> message
      IllegalArgumentException.
      throw))


(defn deep-concat
  [seq]
  (if (empty? seq)
    '()
    (concat
      (first seq)
      (deep-concat (next seq)))))

(defn falsy?
  [what]
  (or (nil? what)
      (false? what)))

(defn truthy?
  [what]
  (-> what falsy? not))
