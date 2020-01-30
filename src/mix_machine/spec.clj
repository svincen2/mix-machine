(ns mix-machine.spec)

;; A specification is a seq of constraints
;; A constraint is a seq of [predicate description]

(defn valid?
  [spec x]
  (loop [constraints spec]
    (if (empty? constraints)
      true
      (let [[predicate _] (first constraints)]
        (if-not (predicate x)
          false
          (recur (rest constraints)))))))

(defn explain
  [spec x]
  (loop [constraints spec]
    (if (empty? constraints)
      x
      (let [[predicate description] (first constraints)]
        (if-not (predicate x)
          description
          (recur (rest constraints)))))))
