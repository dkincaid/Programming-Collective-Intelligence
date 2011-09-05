(ns pci.chap1
  (:use pci.core clojure.set)
  (:require [clojure.string :as str])
  (:import [org.apache.commons.math.stat.correlation
	    PearsonsCorrelation Covariance]))

;; Note that if you're looking at the code in the book, I have replaced "person" with
;; "item" and "movie" with "rater" to try to make it more generic.

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

(def movies (invert-hash-map-of-maps critics))

(defn similarity-distance [x y] ;; sim_distance
  "Calculate the square of the Euclidean distance between two vectors and invert it."
  (/ 1 (+ 1 (sum (map square (map - x y))))))

(defn pearson-correlation [x y] ;; sim_pearson
  "Calculate the Pearson correlation coefficient of two vectors"
  (if (> (count (double-array x)) 2)
    (.correlation (PearsonsCorrelation.) (double-array x) (double-array y))
    0.0000000000001))

(defn similarity [prefs rater1 rater2 simfn]
  "Calculate the similarity between two raters using the provided similarity function."
  (let [rater1-ratings (prefs rater1)
        rater2-ratings (prefs rater2)
        common-keys (find-common-keys rater1-ratings rater2-ratings)]
;;    (println (str rater1 " " rater2))
;;    (println (rater1 prefs))
    (simfn (map rater1-ratings (sort common-keys))
           (map rater2-ratings (sort common-keys)))))

(defn calculate-similarities [prefs item simfn] ;; topMatches
  "Return a map of the similarity between one item and all other items in the prefs map."
  (let [others (filter #(not (= item %))                  
                       (keys prefs))]
    (apply merge (map #(hash-map % (similarity prefs item % simfn)) others))))

(defn item-ratings [prefs item]
  "Get all of the ratings for a particular item"
  (prefs item))

(defn item-rater-rating [prefs item rater]
  "Get the rating for a specific rater for a particular item"
  ((item-ratings prefs item) rater))

(defn weight-all-map-values [m weight]
  "Given a map scale all the values by a given weight"
  (into {} (map #(hash-map (key %) (* weight (val %))) m)))

(defn weight-item-ratings [prefs item weight]
  "Scale the values of an item's ratings by a given value"
  (weight-all-map-values (item-ratings prefs item) weight))

(defn weight-all-ratings [prefs item simfn]
  "Weight all the ratings based on the similarity with the given person"
  (let [similarities (calculate-similarities prefs item simfn)
        others (remove #(= % item) (keys prefs))]
    (into {}
          (map #(hash-map % (weight-item-ratings prefs % (similarities %)))
               others))))

(defn simsum [prefs item movie similarities]
  "Sum the similarity measures for a movie for those that rated it."
  (let [raters (filter #(contains? (prefs %) movie) (keys prefs))
        rater-sims (filter #(contains? (set raters) (key %)) (filter #(pos? (val %)) similarities))
        thesum (sum (vals rater-sims))]
    (if (= thesum 0)
      0.0000001
      thesum)))

(defn get-recommendations [prefs item simfn] ;; getRecommendations
  "Get a sorted map of recommendations for item using the given simlarity function."
  (let [weighted-ratings (weight-all-ratings prefs item simfn)
        weighted-sums (apply merge-with #(if (pos? %2) ;; weighted-sums is "Total" from the book
                                           (+ %1 %2)
                                           %1)
                             (vals weighted-ratings))
        similarities (calculate-similarities prefs item simfn)]
    (sort-map-by-value-desc (apply dissoc (into {} (map
                              #(hash-map (key %)  (/ (val %) (simsum prefs item (key %) similarities)))
                              weighted-sums)) (keys (prefs item))))))

(defn calculate-similar-items [prefs n simfn]
  "Calculate all the similarities between all items"
  (apply merge (for [k (keys prefs)]
                 {k (take n (sort-map-by-value-desc
                                 (calculate-similarities prefs k simfn))) })))

(defn split-and-mapify [s re]
  "Split a string on a regular expression and create a hash map with the first field as the key and the second field as the value"
  (let [fields (str/split s re)]
    (hash-map (keyword (first fields)) (second fields))))

(defn load-movies [file-name]
  (apply merge (map #(split-and-mapify % #"\|") (str/split-lines (slurp file-name)))))

(def moviemap (load-movies "/home/davek/src/programming-collective-intelligence/src/pci/ml-100k/u.item"))

(defn split-and-make-rating [s re]
  "Split a string on a regular expression and create a map of maps"
  (let [fields (str/split s re)]
    {(keyword (first fields)) {(moviemap (keyword (second fields))) (Double/parseDouble (fields 2))}}))

(defn load-ratings [file-name]
  (apply merge-with merge (map #(split-and-make-rating % #"\t") (str/split-lines (slurp file-name)))))

(def ratingmap (load-ratings "/home/davek/src/programming-collective-intelligence/src/pci/ml-100k/u.data"))