(ns fuchi.methods.test-itonami-surplus-ledger
  "itonami surplus ledger tests — portable under bb and nbb."
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.itonami-surplus-ledger :as led]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.edn :as edn]))

#?(:cljs
   (def ^:private fs (js/require "node:fs")))

(defn- path-exists? [p]
  #?(:clj (.exists (java.io.File. (str p)))
     :cljs (.existsSync fs (str p))))

(deftest test-build-ledger-g2
  (let [itonami (edn/load-data "itonami-displacement-events.edn")
        fuchi (edn/load-data "seed-sustenance-graph.kotoba.edn")
        entries (led/build-ledger itonami fuchi)
        by (into {} (map (fn [e] [(:ledger/displacing-actor e) e]) entries))
        sum (led/ledger-summary entries)]
    (is (= 4 (count entries)))
    (is (true? (:ledger/admissible (get by "sanae"))))
    (is (true? (:ledger/admissible (get by "itonami-robotics"))))
    (is (false? (:ledger/admissible (get by "hataori"))))
    (is (false? (:ledger/admissible (get by "warehouse-amr"))))
    (is (= 0 (:cash-to-workers-usd-micros sum)))
    (is (= 0 (:ledger/cash-to-workers-usd-micros (get by "sanae"))))
    (is (false? (:live sum)))
    (is (= [] (:score-surface sum)))
    (doseq [e entries] (pp/assert-no-public-scores! e))))

(deftest test-write-ledger
  (let [paths (led/write-ledger!)]
    (is (path-exists? (:path paths)))
    (is (false? (:live paths)))
    (is (= 0 (:cash-usd-micros paths)))
    (is (= 0 (:cash-to-workers-usd-micros paths)))
    (is (false? (:deployed paths)))
    (is (= 4 (get-in paths [:summary :events])))))
