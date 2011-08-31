(ns pci.chap1
  ( :use clojure.contrib.math clojure.set
         clojure.contrib.pprint))


(def critics {"Lisa Rose" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.5,
"Just My Luck" 3.0, "Superman Returns" 3.5, "You, Me and Dupree" 2.5,
"The Night Listener" 3.0},
"Gene Seymour" {"Lady in the Water" 3.0, "Snakes on a Plane" 3.5,
"Just My Luck" 1.5, "Superman Returns" 5.0, "The Night Listener" 3.0,
"You, Me and Dupree" 3.5},
"Michael Phillips" {"Lady in the Water" 2.5, "Snakes on a Plane" 3.0,
"Superman Returns" 3.5, "The Night Listener" 4.0},
"Claudia Puig" {"Snakes on a Plane" 3.5, "Just My Luck" 3.0,
"The Night Listener" 4.5, "Superman Returns" 4.0,
"You, Me and Dupree" 2.5},
"Mick LaSalle" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0,
"Just My Luck" 2.0, "Superman Returns" 3.0, "The Night Listener" 3.0,
"You, Me and Dupree" 2.0},
"Jack Matthews" {"Lady in the Water" 3.0, "Snakes on a Plane" 4.0,
"The Night Listener" 3.0, "Superman Returns" 5.0, "You, Me and Dupree" 3.5},
"Toby" {"Snakes on a Plane" 4.5,"You, Me and Dupree" 1.0,"Superman Returns" 4.0}})

(defn sum [x] (reduce + x))
(defn square [x] (Math/pow x 2))

(defn find-common-keys [x y]
  "Find the keys common between two maps"
  (let [keys1 (set (keys x))
        keys2 (set (keys y))]
    (intersection keys1 keys2)))

(defn sort-map-by-value [m]
  "Sort a map by the values in ascending order."
  (into (sorted-map-by (fn [key1 key2] (compare (m key1) (m key2)))) m))

(defn sort-map-by-value-desc [m]
  "Sort a map by the values in descending order"
  (into (sorted-map-by (fn [key1 key2] (compare (m key2) (m key1)))) m))

(defn similarity-distance [x y]
  "Calculate the square of the Euclidean distance between two vectors."
  (/ 1 (+ 1 (sum (map square (map - x y))))))

(defn pearson-correlation [x y]
  "Calculate the Pearson correlation coefficient of two vectors."
  (let [n (count x)
        sum1 (sum x)
        sum2 (sum y)
        sum1sq (sum (map square x))
        sum2sq (sum (map square y))
        psum (sum (map * x y))
        num (- psum (/ (* sum1 sum2) n))
        den (Math/sqrt (* (- sum1sq (/ (square sum1) n)) (- sum2sq (/ (square sum2) n))))]
    (/ num den)))

(defn similarity [prefs s1 s2 simfn]
  "Calculate the similarity between two people using the provided similarity function."
  (let [c1 (prefs s1)
        c2 (prefs s2)
        common (find-common-keys c1 c2)]
    (simfn (map c1 (sort common))
           (map c2 (sort common)))))

(defn calculate-similarities [prefs person simfn]
  "Return a map of the similarity between one person and all other people in the prefs map."
  (let [others (filter #(not (= person %))
                       (keys prefs))]
    (reduce conj (map #(hash-map % (similarity prefs person % simfn)) others))
    ))

(defn persons-ratings [prefs person]
  "Get all of the ratings for a particular person"
  (prefs person))

(defn movie-rating [prefs person movie]
  "Get the rating for a specific item for a particular person"
  ((persons-ratings prefs person) movie))

(defn weight-ratings [m weight]
  "Given a map scale the values by a given weight"
  (into {} (map #(hash-map (key %)
                           (* weight (val %)))
                m)))

(defn weight-persons-ratings [prefs person weight]
  "Scale the values of a person's ratings by a given value"
  (weight-ratings (persons-ratings prefs person)
                    weight))

(defn weight-all-ratings [prefs person simfn]
  "Weight all the ratings based on the similarity with the given person"
  (let [similarities (calculate-similarities prefs person simfn)]
      (into {}
            (map #(hash-map %
                            (weight-persons-ratings critics %
                                                    (similarities %)))
                 (filter #(not (= % person)) (keys critics))))))

(defn simsum [prefs person movie similarities]
  "Sum the similarity measures for a movie for those that rated it."
  (let [raters (filter #(contains? (prefs %) movie) (keys prefs))
        rater-sims (filter #(contains? (set raters) (key %)) (filter #(pos? (val %)) similarities))]
    (sum (vals rater-sims))
    ))

(defn get-recommendations [prefs person simfn]
  "Get a list of recommendations for person using simlarity function."
  (let [weighted-ratings (weight-all-ratings prefs person simfn)
        weighted-sums (apply merge-with #(if (pos? %2)
                                     (+ %1 %2)
                                      %1)
                             (vals weighted-ratings))
        similarities (calculate-similarities critics person simfn)]
    (map #(hash-map (key %) (/ (val %) (simsum critics person (key %) similarities))) weighted-sums)
    ))
