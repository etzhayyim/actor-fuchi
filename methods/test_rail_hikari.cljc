(ns fuchi.methods.test-rail-hikari
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.rail-hikari :as h]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:seth"
   :covenant "vowed"
   :rails [{:kind "energy" :active? true}]
   :floor-usd-micros-yr 2000000000
   :disclosure d
   :exit-suspended? false})

(deftest test-r1-hikari-provider
  (let [i (h/r1-dry-intent "a" 2000000000)]
    (is (= "energy-hikari" (:rail-kind i)))
    (is (= "did:web:etzhayyim.com:actor:hikari" (:provider-did i)))
    (is (= 0 (:cash-usd-micros i)))
    (is (false? (:published i)))))

(deftest test-r1-package-and-hold
  (let [pkg (h/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})]
    (is (= :R1-dry (:phase pkg)))
    (is (= [] (:score-surface pkg)))
    (pp/assert-no-public-scores! pkg))
  (let [p (person {:wage-labor-band :stale :state-benefits? false
                   :wellbecoming-attest-fact :stale :related-party-edges []
                   :rider-s2-self-report :none})
        pkg (h/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1
                               :person p :hold-machine (dh/initial p)})]
    (is (= :refused (:phase pkg)))))

(deftest test-gated-default-refuse-and-full-plan
  (is (false? (get (h/default-refuse-status) "admissible")))
  (let [pkg (h/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})
        st (h/gated-live-status pkg)
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (h/gated-live-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :refused (:phase st)))
    (is (false? (:admissible st)))
    (is (false? (:generate-executed st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (is (= "care-first-hikari-path" (:care-first-api-path st)))
    (is (= ["care" "housing"] (:care-first-before-rails st)))
    (is (some #{"energy-after-care-housing-for-mago-ko-substrate"} (:multi-gen-facts st)))
    (is (= :gated-live-plan (:phase plan)))
    (is (true? (:authorized-to-publish plan)))
    (is (false? (:live plan)))
    (is (false? (:published plan)))
    (pp/assert-no-public-scores! (dissoc st :multi-gen-facts :care-first-before-rails))))

(deftest test-design-public-facts-care-first
  "Priority (3) design SSoT: energy after care/housing for 孫/子; generate never live."
  (let [d (h/design-public-facts)]
    (is (= "energy-hikari" (:rail-kind d)))
    (is (= "care-first-hikari-path" (:care-first-api-path d)))
    (is (= ["care" "housing"] (:care-first-before-rails d)))
    (is (false? (:live-produce d)))
    (is (false? (:generate-executed d)))
    (is (false? (:live d)))
    (is (zero? (:cash-usd-micros d)))
    (is (= [] (:score-surface d)))
    (is (some #{"energy-after-care-housing-for-mago-ko-substrate"} (:multi-gen-facts d)))
    (is (= pp/PRIORITY-STACK (:priority-stack d)))))

(deftest test-design-edn-invariants-cash-zero-live-refuse
  (let [inv (h/design-edn-invariants)]
    (is (= "energy-hikari" (:rail-kind inv)))
    (is (zero? (:cash-usd-micros inv)))
    (is (= [] (:score-surface inv)))
    (is (false? (:live-produce inv)))
    (is (false? (:design-live inv)))
    (is (= ["care" "housing"] (:care-first-before-rails inv)))
    (is (= "care-first-hikari-path" (:care-first-api-path inv)))))
