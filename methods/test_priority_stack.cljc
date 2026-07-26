(ns fuchi.methods.test-priority-stack
  "Priority offline stack (1)L0 (2)disclosure (3)mitsuho+hikari DESIGN — portable nbb+bb."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.priority-stack :as ps]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.edn :as edn]))

(deftest test-run-offline-priority-123
  (let [s (ps/run-offline
           {:subject-did "did:web:etzhayyim.com:member:priority-test"
            :vow-text "悔い改め・バプテスマ・得度 — priority stack test"
            :member-signature "sig-priority-stack-test"
            :food-imputed-usd-micros-yr 2000000000
            :energy-imputed-usd-micros-yr 1500000000
            :care-imputed-usd-micros-yr 1000000000})
        f (ps/public-facts s)]
    (is (true? (:ok s)))
    (is (false? (:live s)))
    (is (zero? (:cash-usd-micros s)))
    (is (= [] (:score-surface s)))
    (is (= ps/PRIORITY-STACK (:priority-stack s)))
    (is (= 3 (count (:priority-order s))))
    (is (map? (:design s)))
    (is (= "fuchi.priority-stack-offline" (get-in s [:design :design-id])))
    (is (= 3 (get-in s [:design :order-count])))
    (is (= "fuchi.priority-stack-offline" (:design-id f)))
    ;; l0 path catalog design
    (is (= "fuchi.l0-offline-priority-paths" (get-in s [:l0-paths-design :design-id])))
    (is (= 9 (get-in s [:l0-paths-design :path-count])))
    (is (true? (get-in s [:l0-paths-design :all-paths-held-stress-embed])))
    (is (= 9 (:l0-paths-count f)))
    (is (true? (:l0-paths-all-held-stress f)))
    ;; (1) L0
    (is (= "L0" (get-in s [:l0 :stage])))
    (is (false? (get-in s [:l0 :published])))
    (is (true? (get-in s [:l0 :token-stub])))
    ;; (2) disclosure
    (is (true? (get-in s [:disclosure :open-may-flow])))
    (is (true? (get-in s [:disclosure :stale-held])))
    (is (false? (get-in s [:disclosure :stale-may-flow])))
    (is (= "open" (get-in s [:disclosure :final-state])))
    (is (pos? (get-in s [:disclosure :continuity-held-steps])))
    ;; (3) care-housing multi-gen substrate then mitsuho + hikari
    (is (= "care-housing-first-path" (get-in s [:care-housing :api-path])))
    (is (true? (get-in s [:care-housing :care-housing-both-refused])))
    (is (false? (get-in s [:care-housing :land-grant-executed])))
    (is (true? (get-in s [:care-housing :held-stress-ladder-refused])))
    (is (= "care-housing-first-path" (:care-housing-api-path f)))
    (is (true? (:care-housing-both-refused f)))
    (is (false? (:care-housing-land-grant-executed f)))
    (is (true? (:care-housing-held-stress-ladder-refused f)))
    ;; (3) all-seven capstone
    (is (= "all-seven-substrate-path" (get-in s [:all-seven :api-path])))
    (is (true? (get-in s [:all-seven :all-inkind-full-chain-refused])))
    (is (true? (get-in s [:all-seven :all-seven-membrane-refused])))
    (is (true? (get-in s [:all-seven :liquidity-receive-refused])))
    (is (false? (get-in s [:all-seven :liquidity-loan-executed])))
    (is (false? (get-in s [:all-seven :land-grant-executed])))
    (is (true? (get-in s [:all-seven :held-stress-ladder-refused])))
    (is (= "all-seven-substrate-path" (:all-seven-api-path f)))
    (is (true? (:all-seven-membrane-refused f)))
    (is (false? (:all-seven-loan-executed f)))
    (is (= "R1-dry" (get-in s [:mitsuho :r1-phase])))
    (is (= "refused" (get-in s [:mitsuho :gated-phase])))
    (is (false? (get-in s [:mitsuho :produce-executed])))
    (is (= "care-first-mitsuho-path" (get-in s [:mitsuho :care-first-api-path])))
    (is (= ["care" "housing"] (get-in s [:mitsuho :care-first-before-rails])))
    (is (true? (get-in s [:mitsuho :held-stress-ladder-refused])))
    (is (zero? (get-in s [:mitsuho :design-edn-cash])))
    (is (false? (get-in s [:mitsuho :design-edn-live])))
    (is (= "R1-dry" (get-in s [:hikari :r1-phase])))
    (is (= "refused" (get-in s [:hikari :gated-phase])))
    (is (false? (get-in s [:hikari :produce-executed])))
    (is (= "care-first-hikari-path" (get-in s [:hikari :care-first-api-path])))
    (is (= ["care" "housing"] (get-in s [:hikari :care-first-before-rails])))
    (is (true? (get-in s [:hikari :held-stress-ladder-refused])))
    (is (zero? (get-in s [:hikari :design-edn-cash])))
    (is (false? (get-in s [:hikari :design-edn-live])))
    ;; public facts strip nested bodies
    (is (= "L0" (:l0-stage f)))
    (is (= "refused" (:mitsuho-gated-phase f)))
    (is (= "refused" (:hikari-gated-phase f)))
    (is (false? (:live f)))
    (is (zero? (:cash-usd-micros f)))
    (is (= [] (:score-surface f)))
    (is (true? (ps/assert-public-facts! f)))
    (pp/assert-no-public-scores! (dissoc f :priority-order :mitsuho-care-first-before-rails
                                         :hikari-care-first-before-rails
                                         :priority-stack))))

(deftest test-slices-independently
  (let [l0 (ps/run-l0 {})
        d2 (ps/run-disclosure l0)
        d3ch (ps/run-care-housing-substrate {})
        d3m (ps/run-mitsuho-design {})
        d3h (ps/run-hikari-design {})
        d3a7 (ps/run-all-seven-substrate {})]
    (is (= 1 (:priority l0)))
    (is (= 2 (:priority d2)))
    (is (= 3 (:priority d3ch)))
    (is (= 3 (:priority d3m)))
    (is (= 3 (:priority d3h)))
    (is (= 3 (:priority d3a7)))
    (is (= "care-housing-first-path" (:api-path d3ch)))
    (is (true? (:care-housing-both-refused d3ch)))
    (is (= "energy-hikari" (:rail d3h)))
    (is (= "all-seven-substrate-path" (:api-path d3a7)))
    (is (true? (:all-seven-membrane-refused d3a7)))
    (is (false? (:live l0)))
    (is (false? (:live d2)))
    (is (false? (:live d3ch)))
    (is (false? (:live d3m)))
    (is (false? (:live d3h)))
    (is (false? (:live d3a7)))))

(deftest test-design-edn-invariants
  "data/priority-stack-design.edn: cash≡0, live false, order 1→2→3."
  (let [inv (ps/design-edn-invariants)]
    (is (= "fuchi.priority-stack-offline" (:design-id inv)))
    (is (zero? (:cash-usd-micros inv)))
    (is (false? (:live inv)))
    (is (= [] (:score-surface inv)))
    (is (= [1 2 3] (:priority-order-ns inv)))
    (is (= 3 (:order-count inv)))
    (is (= "methods/priority_stack.cljc" (:module inv)))
    (is (= "run-offline" (:api inv)))))

(deftest test-l0-paths-design-invariants
  "data/l0-offline-priority-paths-design.edn: 9 paths, all held-stress-embed."
  (let [inv (ps/l0-paths-design-invariants)]
    (is (= "fuchi.l0-offline-priority-paths" (:design-id inv)))
    (is (zero? (:cash-usd-micros inv)))
    (is (false? (:live inv)))
    (is (= 9 (:path-count inv)))
    (is (true? (:all-paths-held-stress-embed inv)))
    (is (= [1 2 3] (:priority-order-ns inv)))))

(deftest test-assert-public-facts-on-run-offline
  (let [f (ps/public-facts (ps/run-offline {}))]
    (is (true? (ps/assert-public-facts! f)))))

(deftest test-static-priority-stack-offline-edn-if-present
  "Hand-seeded public/priority-stack-offline.edn must satisfy invariants when present."
  (let [p "public/priority-stack-offline.edn"
        exists? #?(:clj (.exists (java.io.File. p))
                   :cljs (.existsSync (js/require "node:fs") p))]
    (when exists?
      (is (true? (ps/assert-public-facts! (ps/load-public-facts-file p)))))))

(deftest test-readiness-layer-priority-stack-ssot
  "data/itonami-offline-ss-readiness.edn declares :priority-stack-ssot layer."
  (let [r (edn/load-data "itonami-offline-ss-readiness.edn")
        layers (or (get r ":readiness/layers") (get r :readiness/layers) [])
        ids (map str (map #(or (get % ":id") (get % :id)) layers))
        status (or (get r ":readiness/status") (get r :readiness/status))]
    (is (some #(str/includes? % "priority-stack-ssot") ids))
    (is (str/includes? (str status) "code-complete-unlanded"))
    (let [inv (or (get r ":readiness/invariants") (get r :readiness/invariants) {})]
      (is (zero? (long (or (get inv ":cash-usd-micros") (get inv :cash-usd-micros) 1))))
      (is (false? (or (get inv ":live") (get inv :live) true)))
      (is (= [] (or (get inv ":score-surface") (get inv :score-surface) [:x]))))))
