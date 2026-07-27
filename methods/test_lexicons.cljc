(ns fuchi.methods.test-lexicons
  "Lexicon well-formedness tests for 扶持 (fuchi).
  Portable under bb and nbb (ADR-2607173000)."
  (:require [clojure.test :refer [deftest is run-tests]]
            [clojure.string :as str]
            [fuchi.methods.edn :as edn]))

#?(:cljs
   (def ^:private fs (js/require "node:fs")))
#?(:cljs
   (def ^:private path (js/require "node:path")))

(defn- actor-root
  []
  (or (edn/actor-dir)
      #?(:clj "."
         :cljs ".")))

(defn- lex-dir
  []
  #?(:clj (java.io.File. (actor-root) "lex")
     :cljs (.join path (actor-root) "lex")))

(defn- list-edn-names
  []
  #?(:clj
     (set (map #(.getName %)
               (filter #(str/ends-with? (.getName %) ".edn")
                       (.listFiles (lex-dir)))))
     :cljs
     (set (filter #(str/ends-with? % ".edn")
                  (seq (.readdirSync fs (lex-dir)))))))

(defn- load-lex [fname]
  (edn/load-edn
   #?(:clj (str (java.io.File. (lex-dir) fname))
      :cljs (.join path (lex-dir) fname))))

;; EXPECTED = {filename → lexicon id}
(def ^:private expected
  (array-map
   "maintainerCovenant.edn" "com.etzhayyim.fuchi.maintainerCovenant"
   "sustenanceEnvelope.edn" "com.etzhayyim.fuchi.sustenanceEnvelope"
   "allocationIntent.edn"   "com.etzhayyim.fuchi.allocationIntent"
   "routingPlan.edn"        "com.etzhayyim.fuchi.routingPlan"
   "governanceDecision.edn" "com.etzhayyim.fuchi.governanceDecision"
   "provisioningIntent.edn" "com.etzhayyim.fuchi.provisioningIntent"
   "voteBallot.edn"         "com.etzhayyim.fuchi.voteBallot"
   "sustenanceBooking.edn"  "com.etzhayyim.fuchi.sustenanceBooking"
   "cohortEarmark.edn"      "com.etzhayyim.fuchi.cohortEarmark"
   "disclosureAttestation.edn" "com.etzhayyim.fuchi.disclosureAttestation"
   "commitmentVow.edn" "com.etzhayyim.fuchi.commitmentVow"
   "mitsuhoRailDispatch.edn" "com.etzhayyim.fuchi.mitsuhoRailDispatch"
   "hikariRailDispatch.edn" "com.etzhayyim.fuchi.hikariRailDispatch"
   "housingCommonsRailDispatch.edn" "com.etzhayyim.fuchi.housingCommonsRailDispatch"
   "toolingOkaimonoRailDispatch.edn" "com.etzhayyim.fuchi.toolingOkaimonoRailDispatch"
   "computeMurakumoRailDispatch.edn" "com.etzhayyim.fuchi.computeMurakumoRailDispatch"
   "liquidityWarifuRailDispatch.edn" "com.etzhayyim.fuchi.liquidityWarifuRailDispatch"))

(deftest test-all-five-lexicons-present
  (let [files (list-edn-names)]
    (is (every? files (keys expected))
        (str "missing: " (remove files (keys expected))))))

(deftest test-each-lexicon-well-formed
  (doseq [[fname lid] expected]
    (let [lex (load-lex fname)]
      (is (= 1 (get lex ":lexicon")) fname)
      (is (= lid (get lex ":id")) fname)
      (let [rec (get-in lex [":defs" ":main"])]
        (is (= "record" (get rec ":type")) fname)
        (is (contains? rec ":record") fname)
        (is (= "object" (get-in rec [":record" ":type"])) fname)
        (is (seq (get-in rec [":record" ":required"])) fname)))))

(deftest test-namespace-prefix-is-fuchi
  (doseq [lid (vals expected)]
    (is (str/starts-with? lid "com.etzhayyim.fuchi.") lid)))

(defn -main [& _]
  (run-tests 'fuchi.methods.test-lexicons))
