(ns
  ^{:author "Punit Naik" :doc "The Cookie Problem"} 
  think-bayes-clj.1-3.core)

(defn prior-probability-calculator
  "Calculates Prior Probablity of bowl selection"
  [prob-data]
  (reduce-kv
    (fn [m k v]
      (assoc m k
        (double
          (/ 1 (count prob-data)))))
    {}
    prob-data))

(defn bayes-equation-builder
  "Builds the Bayes Theorem Equation based on arguments passed"
  [a b prob-data]
  (let [prior-prob-for-all-cases (prior-probability-calculator prob-data)
        normalisation-factor (->> (map
                                    (fn [[k v]]
                                      (* (prior-prob-for-all-cases k)
                                         (/ (get-in prob-data [k b])
                                            (reduce + (map second (get prob-data k))))))
                                    prior-prob-for-all-cases)
                                  (reduce +))
        prior-prob-a (prior-prob-for-all-cases a)
        likelihood-prob-b-given-a (double (/ (-> (get prob-data a)
                                                 (get b))
                                             (->> (get prob-data a)
                                                  vals
                                                  (reduce +))))]
    (/ (* prior-prob-a likelihood-prob-b-given-a)
       normalisation-factor)))

(def prob-data
  {:bowl-1 {:vanilla 30 :chocolate 10}
   :bowl-2 {:vanilla 20 :chocolate 20}})

(bayes-equation-builder :bowl-1 :vanilla prob-data)

