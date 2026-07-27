(ns fuchi.methods.test-rail-care-iyashi
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.rail-care-iyashi :as c]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.public-person :as pp]))

(def ^:private fresh
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(defn- person [d]
  {:did "did:web:etzhayyim.com:member:eve" :covenant "vowed"
   :rails [{:kind "care" :active? true}] :floor-usd-micros-yr 2000000000
   :disclosure d :exit-suspended? false})

(deftest test-care-r1
  (let [i (c/r1-dry-intent "a" 2000000000)]
    (is (= "care-iyashi" (:rail-kind i)))
    (is (= "did:web:etzhayyim.com:actor:iyashi" (:provider-did i)))
    (is (= 0 (:cash-usd-micros i)))
    (is (false? (:published i)))))

(deftest test-care-package-and-hold
  (let [pkg (c/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})]
    (is (= :R1-dry (:phase pkg)))
    (is (= [] (:score-surface pkg)))
    (pp/assert-no-public-scores! pkg))
  (let [p (person {:wage-labor-band :stale :state-benefits? false
                   :wellbecoming-attest-fact :stale :related-party-edges []
                   :rider-s2-self-report :none})
        pkg (c/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1
                               :person p :hold-machine (dh/initial p)})]
    (is (= :refused (:phase pkg)))))

(deftest test-design-public-facts-multi-gen-first
  "Priority (3) design SSoT: care is multi-gen substrate #1 for 孫/子; delivery never live."
  (let [d (c/design-public-facts)]
    (is (= "care-iyashi" (:rail-kind d)))
    (is (= "care-housing-first-path" (:care-first-api-path d)))
    (is (= 1 (:care-first-order-rank d)))
    (is (true? (:multi-gen-first d)))
    (is (= [] (:care-first-before-rails d)))
    (is (false? (:live-produce d)))
    (is (false? (:care-delivery-executed d)))
    (is (false? (:live d)))
    (is (zero? (:cash-usd-micros d)))
    (is (= [] (:score-surface d)))
    (is (some #{"care-is-wellbecoming-substrate"} (:multi-gen-facts d)))
    (is (= pp/PRIORITY-STACK (:priority-stack d)))))

(deftest test-gated-plan
  (is (false? (get (c/default-refuse-status) "admissible")))
  (let [pkg (c/r1-dry-package {:alloc-id "a" :imputed-usd-micros-yr 1 :person (person fresh)})
        st (c/gated-live-status pkg)
        gate (live-gate/make-live-gate
              {:leg "provision" :operator-did "did:op:x" :council-level 6
               :member-signature "member-cap-ok"})
        plan (c/gated-live-plan pkg gate :env {"FUCHI_ALLOW_LIVE_PROVISION" "1"})]
    (is (= :refused (:phase st)))
    (is (false? (:admissible st)))
    (is (false? (:care-delivery-executed st)))
    (is (false? (:live st)))
    (is (= 0 (:cash-usd-micros st)))
    (is (= "care-iyashi" (:rail-kind st)))
    (is (= "care-housing-first-path" (:care-first-api-path st)))
    (is (= 1 (:care-first-order-rank st)))
    (is (some #{"care-is-wellbecoming-substrate"} (:multi-gen-facts st)))
    (is (= :gated-live-plan (:phase plan)))
    (is (false? (:live plan)))
    (is (false? (:published plan)))
    (pp/assert-no-public-scores! (dissoc st :multi-gen-facts))))
