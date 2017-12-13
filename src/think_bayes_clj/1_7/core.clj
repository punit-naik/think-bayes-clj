(ns
  ^{:author "Punit Naik" :doc "The Monty Hall Problem"} 
  think-bayes-clj.1-6.core)

(defn prior-probability-calculator
  "Calculates Prior Probablity hypothesis selection"
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
  [prob-data]
  (let [prior-prob-for-all-cases (prior-probability-calculator prob-data)
        probability-of-each-hypothesis (reduce-kv
                                         (fn [m k v]
                                           (assoc m k
                                             (* (prior-prob-for-all-cases k)
                                                 ; Probability of selecting Door B and not finding the car
                                                (condp = k
                                                  :car-behind-door-a (/ 1 2)
                                                  :car-behind-door-b 0
                                                  :car-behind-door-c 1))))
                                         {}
                                         prior-prob-for-all-cases)
        normalisation-factor (->> (vals probability-of-each-hypothesis)
                                  (reduce +))]
    ; Selecting the hypothesis with the maximum probability
    (->> (reduce-kv
           (fn [m k v]
             (assoc m k (double (/ v normalisation-factor))))
           {}
           probability-of-each-hypothesis)
         (apply max-key val))))

; NOTE : All values are already in terms of probabilities

(def hypotheses
  {:car-behind-door-a nil
   :car-behind-door-b nil
   :car-behind-door-c nil})
  
(bayes-equation-builder hypotheses)

