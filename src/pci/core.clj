(ns pci.core
  (:use clojure.set))

(defn invert-hash-map-of-maps [m]
  "Invert a hash-map of maps"
  (apply merge-with merge
         (for [[ok ov] m
               [ik iv] ov]
           {ik {ok iv}})))

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

(defn take-map [n m]
  "Take n key/value pairs from a map and return a new map"
  (into {} (take n m)))

