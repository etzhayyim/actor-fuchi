(ns fuchi.methods.test-rail-housing-commons
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.rail-housing-commons :as h]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:abel" :covenant "vowed"
   :rails [{:kind "housing" :active? true}] :floor-usd-micros-yr 12000000000
   :disclosure d :exit-suspended? false})

(deftest test-housing-r1
  (let [i (h/r1-dry-intent "a" 12000000000)]
    (is (= "housing-commons" (:rail-kind i)))
    (is (= "commons-land" (:provider-did i)))
    (is (= 0 (:cash-usd-micros i)))
    (is (false? (:published i)))))

(deftest test-housing-package-and-hold
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

(deftest test-design-public-facts-after-care
  "Priority (3) design SSoT: housing after care for 孫/子; land-grant never live."
  (let [d (h/design-public-facts)]
    (is (= "housing-commons" (:rail-kind d)))
    (is (= "care-housing-first-path" (:care-first-api-path d)))
    (is (= 2 (:care-first-order-rank d)))
    (is (= ["care"] (:care-first-before-rails d)))
    (is (true? (:multi-gen-first d)))
    (is (false? (:live-produce d)))
    (is (false? (:land-grant-executed d)))
    (is (false? (:live d)))
    (is (zero? (:cash-usd-micros d)))
    (is (= [] (:score-surface d)))
    (is (some #{"land-grant-not-invoked"} (:multi-gen-facts d)))
    (is (= pp/PRIORITY-STACK (:priority-stack d)))))

(deftest test-gated-plan
  (is (false? (get (h/default-refuse-status) "admissible")))
  (let [pkg (h/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})
        st (h/gated-live-status pkg)
        held (h/gated-live-status pkg :council-housing-held? true)
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (h/gated-live-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :refused (:phase st)))
    (is (false? (:admissible st)))
    (is (false? (:land-grant-executed st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (is (= "care-housing-first-path" (:care-first-api-path st)))
    (is (= ["care"] (:care-first-before-rails st)))
    (is (= :refused (:phase held)))
    (is (true? (:council-housing-held? held)))
    (is (false? (:land-grant-executed held)))
    (is (str/includes? (str (:refusal-reason held)) "council"))
    (is (= :gated-live-plan (:phase plan)))
    (is (false? (:live plan)))
    (is (false? (:published plan)))
    (pp/assert-no-public-scores! (dissoc st :multi-gen-facts :care-first-before-rails))
    (pp/assert-no-public-scores! (dissoc held :multi-gen-facts :care-first-before-rails))))
