(ns fuchi.methods.test-displacement-surface
  "Displacement surface tests — portable under nbb and bb (ADR-2607173000)."
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.displacement-surface :as d]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.edn :as edn]))

(defn- seed []
  (edn/load-data "seed-sustenance-graph.kotoba.edn"))

(deftest test-seed-displacement-facts
  (let [rows (d/public-displacement-facts (seed))
        by (into {} (map (fn [r] [(:displacing-actor r) r]) rows))]
    (is (= 2 (count rows)))
    (is (true? (:funded (get by "sanae"))))
    (is (true? (:admissible (get by "sanae"))))
    (is (false? (:funded (get by "hataori"))))
    (is (false? (:admissible (get by "hataori"))))
    (is (= 0 (:cash-usd-micros (get by "sanae"))))
    (is (= [] (:score-surface (get by "sanae"))))
    (doseq [r rows] (pp/assert-no-public-scores! r))))

(deftest test-summary
  (let [s (d/summary (d/public-displacement-facts (seed)))]
    (is (= 2 (:displacement-events s)))
    (is (= 1 (:funded-admissible s)))
    (is (= 1 (:refused s)))
    (is (= 0 (:cash-usd-micros s)))
    (is (= [] (:score-surface s)))))
