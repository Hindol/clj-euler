(ns clj-euler.graph
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def ^:private ^:constant sample
  {:elements [[131 673 234 103 18]
              [201 96 342 965 150]
              [630 803 746 422 111]
              [537 699 497 121 956]
              [805 732 524 37 331]]
   :height   5
   :width    5})

(defn- read-matrix
  [path]
  (let [m      (->> path
                    io/resource
                    io/reader
                    line-seq
                    (mapv #(mapv read-string (str/split % #","))))
        height (count m)
        width  (count (first m))]
    {:elements m
     :width    width
     :height   height}))

(defn- find-shortest-distance
  [{:keys [elements width height]} [sx sy] [ex ey] neighbor-fn]
  (let [index-in-bounds? (fn [[x y]]
                           (and (< -1 x height)
                                (< -1 y width)))
        visited          (atom #{})
        distances        (atom {[sx sy] (get-in elements [sx sy])})]
    (loop [[x y] [sx sy]]
      (let [neighbors (filter index-in-bounds?
                              (neighbor-fn [x y]))]
        (if (= [x y] [ex ey])
          ;; If we are at destination, return
          (get @distances [ex ey])
          ;; Else, maybe update all neighbours' distances
          (do
            (doseq [nbr neighbors]
              (swap! distances
                     assoc
                     nbr
                     (min (get @distances nbr Long/MAX_VALUE)
                          (+ (get @distances [x y])
                             (get-in elements nbr)))))
            (swap! visited
                   conj
                   [x y])
            (let [nxt (first (apply min-key
                                    second
                                    (remove #(contains? @visited (first %))
                                            (seq @distances))))]
              (recur nxt))))))))

(defn- four-neighbors
  [[x y]]
  [[(dec x) y] [x (inc y)] [(inc x) y] [x (dec y)]])
