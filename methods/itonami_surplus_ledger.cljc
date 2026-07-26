(ns fuchi.methods.itonami-surplus-ledger
  "itonami_surplus_ledger.cljc — offline append-only surplus ledger for robotics/itonami.

  Projects itonami displacement seed events into a public facts ledger:
  - surplus funds Public Fund earmarks only (never cash to workers)
  - G2: unfunded surplus → refused entry
  - no personal scores; live=false; no live itonami API

  Portable .cljc; file write under bb + nbb (ADR-2607173000)."
  (:require [fuchi.methods.itonami-bridge :as bridge]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.edn :as edn]
            #?(:clj [clojure.java.io :as io])))

#?(:cljs
   (do
     (def ^:private fs (js/require "node:fs"))
     (def ^:private path (js/require "node:path"))))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn ledger-entry-from-public-fact
  "One ledger line from an itonami public fact row."
  [row]
  (let [entry {:ledger/id (str "itonami-surplus/" (:displacing-actor row) "/" (:cohort-id row))
               :ledger/source "itonami-bridge-offline"
               :ledger/displacing-actor (:displacing-actor row)
               :ledger/cohort-id (:cohort-id row)
               :ledger/displaced-count (:displaced-count row)
               :ledger/surplus-usd-micros-yr (or (:surplus-usd-micros-yr row)
                                                 (:earmark-usd-micros-yr row)
                                                 0)
               :ledger/earmark-usd-micros-yr (or (:earmark-usd-micros-yr row) 0)
               :ledger/funded (boolean (:funded row))
               :ledger/admissible (boolean (:admissible row))
               :ledger/cash-to-workers-usd-micros 0
               :ledger/cash-usd-micros 0
               :ledger/live false
               :ledger/score-surface []
               :ledger/priority-stack PRIORITY-STACK
               :ledger/note (if (:admissible row)
                              "funded surplus → Public Fund earmark only"
                              "G2 refuse: displacement without funded surplus")}]
    (pp/assert-no-public-scores! entry)
    entry))

(defn build-ledger
  "itonami seed (+ optional fuchi seed) → ordered ledger entries (offline)."
  ([itonami-seed]
   (build-ledger itonami-seed nil))
  ([itonami-seed fuchi-seed]
   (let [rows (bridge/public-facts-from-itonami itonami-seed fuchi-seed)]
     (mapv ledger-entry-from-public-fact rows))))

(defn ledger-summary
  [entries]
  {:events (count entries)
   :funded-admissible (count (filter :ledger/admissible entries))
   :refused (count (remove :ledger/admissible entries))
   :total-displaced (reduce + 0 (map :ledger/displaced-count entries))
   :total-earmark-usd-micros-yr (reduce + 0 (map :ledger/earmark-usd-micros-yr entries))
   :cash-to-workers-usd-micros 0
   :live false
   :score-surface []
   :priority-stack PRIORITY-STACK})

(defn- actor-dir
  []
  (edn/actor-dir))

(defn- join-path [& parts]
  #?(:clj (str (apply io/file parts))
     :cljs (.apply (.-join path) path (to-array parts))))

(defn- ensure-dir! [dir]
  #?(:clj (.mkdirs (io/file dir))
     :cljs (when-not (.existsSync fs dir)
             (.mkdirSync fs dir #js {:recursive true}))))

(defn- write-text! [file-path content]
  #?(:clj (spit (io/file file-path) content)
     :cljs (.writeFileSync fs file-path (str content) "utf8")))

(defn write-ledger!
  "Write out/itonami-surplus-ledger.edn from seed files. Never deploys live.
   Portable under bb and nbb. cash≡0; no scores; cash-to-workers always 0."
  []
  (let [itonami (edn/load-data "itonami-displacement-events.edn")
        fuchi (edn/load-data "seed-sustenance-graph.kotoba.edn")
        entries (build-ledger itonami fuchi)
        summary (ledger-summary entries)
        body {:ledger/id "com.etzhayyim.fuchi.itonami-surplus-ledger"
              :ledger/adr ["2606032130" "2607177000"]
              :ledger/live false
              :ledger/cash-usd-micros 0
              :ledger/score-surface []
              :ledger/priority-stack PRIORITY-STACK
              :ledger/summary summary
              :ledger/entries entries}
        outd (join-path (actor-dir) "out")
        out-path (join-path outd "itonami-surplus-ledger.edn")]
    (ensure-dir! outd)
    (doseq [e entries] (pp/assert-no-public-scores! e))
    (when-not (zero? (long (or (:cash-to-workers-usd-micros summary) 0)))
      (throw (ex-info "cash-to-workers must be 0" summary)))
    (write-text! out-path (pr-str body))
    {:path out-path
     :summary summary
     :live false
     :cash-usd-micros 0
     :cash-to-workers-usd-micros 0
     :score-surface []
     :deployed false}))
