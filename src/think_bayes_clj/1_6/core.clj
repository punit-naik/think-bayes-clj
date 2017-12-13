(ns
  ^{:author "Punit Naik" :doc "The M & M Problem"} 
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
                                               ; We are multiplying probability of selecting Yellow and Green from
                                               ; Bowl 1 and Bowl 2 respectively
                                               ; as their selection is independent 
                                               (get-in prob-data [k :bowl-1 :yellow]) ; Yellow from Bowl 1
                                               (get-in prob-data [k :bowl-2 :green])))) ; Green from Bowl 2
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
  {:hypothesis-a
     {:bowl-1 {:brown 30 :yellow 20 :red 20 :green 10 :orange 10 :tan 10} ; Bowl 1 is from 1994
      :bowl-2 {:blue 24 :green 20 :orange 16 :yellow 14 :red 13 :brown 13}} ; Bowl 2 is from 1996
   :hypothesis-b
     {:bowl-1 {:blue 24 :green 20 :orange 16 :yellow 14 :red 13 :brown 13} ; Bowl 1 is from 1996
      :bowl-2 {:brown 30 :yellow 20 :red 20 :green 10 :orange 10 :tan 10}}}) ; Bowl 2 is from 1994
  
(bayes-equation-builder hypotheses)

