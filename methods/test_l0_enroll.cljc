(ns fuchi.methods.test-l0-enroll
  "Offline L0 enrollment scaffold tests (ADR-2605302357 §1.16.3a + 2607177000).
   Priorities: (1) L0 enroll (2) disclosure hold+continuity (3) mitsuho R1 membrane."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-continuity :as disc]))

(def ^:private base
  {:subject-did "did:web:etzhayyim.com:member:lot"
   :vow-text "悔い改め・バプテスマ・得度 — permanent commitment for descendant wellbecoming"
   :member-signature "sig-representative-lot-2026q2"
   :covenant "outreach"})

(def ^:private stale-disc
  {:wage-labor-band :stale
   :state-benefits? false
   :wellbecoming-attest-fact :stale
   :related-party-edges []
   :rider-s2-self-report :none})

(deftest test-priority-path-catalog
  "Discovery surface: offline ladder paths indexed; invariants cash≡0 no scores."
  (let [cat (l0/priority-path-catalog)
        ids (set (map :id (:paths cat)))]
    (is (= "fuchi.l0-offline-priority-paths" (:catalog-id cat)))
    (is (= (count l0/PRIORITY-PATH-CATALOG) (:path-count cat)))
    (is (= (:path-count cat) (:held-stress-embed-count cat)))
    (is (contains? ids "care-first-mitsuho"))
    (is (contains? ids "vocation-recovery"))
    (is (contains? ids "liquidity-residual"))
    (is (contains? ids "all-seven-substrate"))
    (is (contains? ids "full-inkind-substrate"))
    (is (false? (:live cat)))
    (is (zero? (:cash-usd-micros cat)))
    (is (= [] (:score-surface cat)))
    (is (= l0/PRIORITY-STACK (:priority-stack cat)))
    (is (true? (get-in cat [:invariants :loan-never])))
    (is (true? (get-in cat [:invariants :land-grant-never])))
    (is (true? (get-in cat [:invariants :held-stress-embed-all])))
    (doseq [p (:paths cat)]
      (is (false? (:live p)))
      (is (zero? (:cash-usd-micros p)))
      (is (= [] (:score-surface p)))
      (is (true? (:held-stress-embed p)))
      (pp/assert-no-public-scores!
       (dissoc p :rails :role :api :id :held-stress-embed)))))

(deftest test-draft-requires-signature
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (l0/draft-vow (dissoc base :member-signature)))))

(deftest test-triple-permanent-offline
  (let [d (l0/draft-vow base)
        c (l0/triple-permanent d)]
    (is (= :committed-offline (:phase c)))
    (is (= "L0" (:stage c)))
    (is (= "vowed" (:covenant c)))
    (is (false? (:published c)))
    (is (= 0 (:cash-usd-micros c)))
    (is (false? (:server-held-key c)))
    (is (str/starts-with? (:kotoba-cid c) "bafy-offline-"))
    (is (str/starts-with? (:ipfs-cid c) "bafy-offline-"))
    (is (str/starts-with? (:token-id c) "sbt-offline-"))))

(deftest test-enroll-public-person-no-scores
  (let [e (l0/enroll base)
        surf (:public-person e)]
    (is (true? (:public-person? surf)))
    (is (= "L0" (:stage surf)))
    (is (= [] (:score-surface surf)))
    (is (nil? (:priority-rank surf)))
    (is (nil? (:score surf)))
    (is (= 0 (get-in e [:entitlement :cash-usd-micros])))
    (is (false? (get-in e [:entitlement :published])))
    (is (= l0/PRIORITY-STACK (:priority-stack e)))
    (is (= 0 (:cash-usd-micros e)))
    (is (false? (:live e)))
    (pp/assert-no-public-scores! surf)))

(deftest test-enroll-disclosure-hold-and-continuity
  "Priority (2): enroll attaches hold machine + continuity tick; fresh → open."
  (let [e (l0/enroll base)]
    (is (map? (:disclosure-hold e)))
    (is (= :open (:disclosure-state e)))
    (is (false? (:disclosure-held? e)))
    (is (true? (:entitlements-may-flow? e)))
    (is (true? (disc/entitlements-may-flow? (:disclosure-hold e))))
    (is (= :open (get-in e [:disclosure-hold :state])))
    (is (map? (:disclosure-continuity e)))
    (is (false? (get-in e [:disclosure-continuity :held?])))
    (is (some #{"wellbecoming-over-mago-over-ko"} (:multi-gen-facts e)))
    (is (= "care" (get-in e [:entitlement :rails 0 :kind])))))

(deftest test-enroll-stale-disclosure-holds
  "Stale disclosure at enroll → held machine; public-person may remain; no cash."
  (let [e (l0/enroll (assoc base :disclosure stale-disc))]
    (is (= :held (:disclosure-state e)))
    (is (true? (:disclosure-held? e)))
    (is (false? (:entitlements-may-flow? e)))
    (is (= :hold (get-in e [:disclosure-gate :action])))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (is (= [] (:score-surface e)))
    (pp/assert-no-public-scores! (:public-person e))))

(deftest test-apply-disclosure-tick-and-continuity-stress
  "Priority (2): re-tick + open→held→open series; ladder refuse when held."
  (let [e0 (l0/enroll base)
        held (l0/apply-disclosure-tick e0 l0/STALE-DISC :reason "stale-test")
        open (l0/apply-disclosure-tick held l0/FRESH-DISC :reason "redisclose-test")
        stress (l0/continuity-stress e0)
        lad-open (l0/try-ladder-advance e0)
        lad-held (l0/try-ladder-advance held)]
    (is (= :held (:disclosure-state held)))
    (is (false? (:entitlements-may-flow? held)))
    (is (= :open (:disclosure-state open)))
    (is (true? (:entitlements-may-flow? open)))
    (is (= "open" (:final-state stress)))
    (is (pos? (:held-steps stress)))
    (is (= :advanced-offline (:phase lad-open)))
    (is (= :refused (:phase lad-held)))
    (is (false? (:live held)))
    (is (zero? (:cash-usd-micros held)))))

(deftest test-exit-suspend-and-re-affirm
  "Priority (2): exit freezes ladder; re-affirm restores open + ladder advance."
  (let [e0 (l0/enroll base)
        exited (l0/exit-suspend e0)
        lad-x (l0/try-ladder-advance exited)
        restored (l0/re-affirm exited)
        lad-r (l0/try-ladder-advance restored)
        stress (l0/exit-reaffirm-stress e0)]
    (is (= :exit-suspended (:disclosure-state exited)))
    (is (true? (:exit-suspended? exited)))
    (is (false? (:entitlements-may-flow? exited)))
    (is (= :refused (:phase lad-x)))
    (is (= :open (:disclosure-state restored)))
    (is (false? (:exit-suspended? restored)))
    (is (true? (:entitlements-may-flow? restored)))
    (is (= :advanced-offline (:phase lad-r)))
    (is (= "exit-suspended" (:exit-state stress)))
    (is (true? (:exit-ladder-refused stress)))
    (is (= "open" (:reaffirm-state stress)))
    (is (false? (:reaffirm-ladder-refused stress)))
    (is (false? (:live stress)))
    (is (zero? (:cash-usd-micros stress)))))

(deftest test-falsehood-lift-and-care-first-paths
  "Priority (2) falsehood/lift + (3) care-first mitsuho/hikari + dual food+energy + care+housing."
  (let [e0 (l0/enroll base)
        held (l0/report-falsehood e0)
        lad-h (l0/try-ladder-advance held)
        lifted (l0/lift-hold held)
        lad-l (l0/try-ladder-advance lifted)
        fl (l0/falsehood-lift-stress e0)
        cf (l0/care-first-mitsuho-path base)
        ch (l0/care-first-hikari-path base)
        cfh (l0/care-first-mitsuho-hikari-path base)
        cfh-held (l0/care-first-mitsuho-hikari-path (assoc base :disclosure stale-disc))
        chs (l0/care-housing-first-path base)
        mgs (l0/multi-gen-substrate-path base)
        mgs-held (l0/multi-gen-substrate-path (assoc base :disclosure stale-disc))
        fis (l0/full-inkind-substrate-path base)
        fis-held (l0/full-inkind-substrate-path (assoc base :disclosure stale-disc))
        voc (l0/vocation-recovery-path base)
        voc-held (l0/vocation-recovery-path (assoc base :disclosure stale-disc))
        liq (l0/liquidity-residual-path base)
        liq-held (l0/liquidity-residual-path (assoc base :disclosure stale-disc))
        a7 (l0/all-seven-substrate-path base)
        a7-held (l0/all-seven-substrate-path (assoc base :disclosure stale-disc))]
    (is (= :held (:disclosure-state held)))
    (is (false? (:entitlements-may-flow? held)))
    (is (= :refused (:phase lad-h)))
    (is (= :open (:disclosure-state lifted)))
    (is (true? (:entitlements-may-flow? lifted)))
    (is (= :advanced-offline (:phase lad-l)))
    (is (true? (:falsehood-ladder-refused fl)))
    (is (false? (:lift-ladder-refused fl)))
    (is (= "open" (:disclosure-state cf)))
    (is (true? (:care-mitsuho-both-refused cf)))
    (is (false? (:ladder-advance-refused cf)))
    (is (some #{"wellbecoming-over-mago-over-ko"} (:multi-gen-facts cf)))
    ;; priority (3) DESIGN facts embedded on care-first mitsuho
    (is (map? (:mitsuho-design cf)))
    (is (= "food-mitsuho" (get-in cf [:mitsuho-design :rail-kind])))
    (is (= "care-first-mitsuho-path" (:care-first-api-path cf)))
    (is (= ["care" "housing"] (:care-first-before-rails cf)))
    (is (false? (:mitsuho-live-produce cf)))
    (is (false? (:mitsuho-produce-executed cf)))
    (is (false? (:care-delivery-executed cf)))
    (is (map? (:care-design cf)))
    (is (= "care-iyashi" (get-in cf [:care-design :rail-kind])))
    ;; priority (2) held-stress embedded on care-first mitsuho/hikari
    (is (map? (:held-stress cf)))
    (is (= "held" (get-in cf [:held-stress :disclosure-state])))
    (is (true? (get-in cf [:held-stress :disclosure-held])))
    (is (false? (get-in cf [:held-stress :entitlements-may-flow])))
    (is (true? (get-in cf [:held-stress :care-mitsuho-both-refused])))
    (is (true? (get-in cf [:held-stress :ladder-advance-refused])))
    (is (true? (:held-stress-ladder-refused cf)))
    (is (true? (:held-stress-both-refused cf)))
    (is (false? (get-in cf [:held-stress :live])))
    (is (zero? (get-in cf [:held-stress :cash-usd-micros])))
    (is (true? (:care-hikari-both-refused ch)))
    (is (false? (:ladder-advance-refused ch)))
    (is (map? (:hikari-design ch)))
    (is (= "energy-hikari" (get-in ch [:hikari-design :rail-kind])))
    (is (= "care-first-hikari-path" (:care-first-api-path ch)))
    (is (false? (:hikari-live-produce ch)))
    (is (false? (:hikari-generate-executed ch)))
    (is (map? (:held-stress ch)))
    (is (= "held" (get-in ch [:held-stress :disclosure-state])))
    (is (true? (get-in ch [:held-stress :care-hikari-both-refused])))
    (is (true? (get-in ch [:held-stress :ladder-advance-refused])))
    (is (true? (:held-stress-ladder-refused ch)))
    (is (true? (:held-stress-both-refused ch)))
    ;; care-first then mitsuho+hikari (priority 3 dual rail) + held-stress embed
    (is (= "open" (:disclosure-state cfh)))
    (is (true? (:care-mitsuho-hikari-all-refused cfh)))
    (is (true? (:mitsuho-hikari-both-refused cfh)))
    (is (true? (:care-full-chain-refused cfh)))
    (is (true? (:mitsuho-full-chain-refused cfh)))
    (is (map? (:mitsuho-design cfh)))
    (is (map? (:hikari-design cfh)))
    (is (false? (:mitsuho-live-produce cfh)))
    (is (false? (:hikari-live-produce cfh)))
    (is (false? (:mitsuho-produce-executed cfh)))
    (is (false? (:hikari-generate-executed cfh)))
    (is (true? (:hikari-full-chain-refused cfh)))
    (is (false? (:ladder-advance-refused cfh)))
    (is (false? (:live cfh)))
    (is (zero? (:cash-usd-micros cfh)))
    (is (= [] (:score-surface cfh)))
    (is (= l0/PRIORITY-STACK (:priority-stack cfh)))
    (is (map? (:held-stress cfh)))
    (is (= "held" (get-in cfh [:held-stress :disclosure-state])))
    (is (true? (get-in cfh [:held-stress :care-mitsuho-hikari-all-refused])))
    (is (true? (get-in cfh [:held-stress :ladder-advance-refused])))
    (is (true? (:held-stress-ladder-refused cfh)))
    (is (true? (:held-stress-all-refused cfh)))
    (is (false? (get-in cfh [:held-stress :live])))
    (is (zero? (get-in cfh [:held-stress :cash-usd-micros])))
    ;; held disclosure as primary path → membranes refuse + ladder refuse
    (is (= "held" (:disclosure-state cfh-held)))
    (is (true? (:disclosure-held cfh-held)))
    (is (false? (:entitlements-may-flow cfh-held)))
    (is (true? (:care-mitsuho-hikari-all-refused cfh-held)))
    (is (true? (:ladder-advance-refused cfh-held)))
    (is (false? (:live cfh-held)))
    (is (zero? (:cash-usd-micros cfh-held)))
    (is (true? (:care-housing-both-refused chs)))
    (is (false? (:land-grant-executed chs)))
    (is (false? (:ladder-advance-refused chs)))
    (is (false? (:live chs)))
    (is (zero? (:cash-usd-micros chs)))
    (is (map? (:held-stress chs)))
    (is (= "held" (get-in chs [:held-stress :disclosure-state])))
    (is (true? (get-in chs [:held-stress :care-housing-both-refused])))
    (is (true? (get-in chs [:held-stress :ladder-advance-refused])))
    (is (false? (get-in chs [:held-stress :land-grant-executed])))
    (is (true? (:held-stress-ladder-refused chs)))
    (is (true? (:held-stress-both-refused chs)))
    ;; L4 multi-gen substrate + food/energy dual rail + held-stress embed
    (is (= "open" (:disclosure-state mgs)))
    (is (true? (:care-housing-mitsuho-hikari-all-refused mgs)))
    (is (true? (:care-housing-both-refused mgs)))
    (is (true? (:mitsuho-hikari-both-refused mgs)))
    (is (false? (:land-grant-executed mgs)))
    (is (false? (:ladder-advance-refused mgs)))
    (is (false? (:live mgs)))
    (is (zero? (:cash-usd-micros mgs)))
    (is (= [] (:score-surface mgs)))
    (is (= l0/PRIORITY-STACK (:priority-stack mgs)))
    (is (map? (:held-stress mgs)))
    (is (= "held" (get-in mgs [:held-stress :disclosure-state])))
    (is (true? (get-in mgs [:held-stress :care-housing-mitsuho-hikari-all-refused])))
    (is (true? (get-in mgs [:held-stress :ladder-advance-refused])))
    (is (false? (get-in mgs [:held-stress :land-grant-executed])))
    (is (true? (:held-stress-ladder-refused mgs)))
    (is (true? (:held-stress-all-refused mgs)))
    (is (= "held" (:disclosure-state mgs-held)))
    (is (true? (:disclosure-held mgs-held)))
    (is (false? (:entitlements-may-flow mgs-held)))
    (is (true? (:care-housing-mitsuho-hikari-all-refused mgs-held)))
    (is (true? (:ladder-advance-refused mgs-held)))
    (is (false? (:land-grant-executed mgs-held)))
    (is (false? (:live mgs-held)))
    (is (zero? (:cash-usd-micros mgs-held)))
    ;; six in-kind (multi-gen + vocation) for itonami recovery + held-stress
    (is (= "open" (:disclosure-state fis)))
    (is (true? (:all-inkind-produce-rails-full-chain-refused fis)))
    (is (true? (:tooling-compute-both-refused fis)))
    (is (true? (:care-housing-both-refused fis)))
    (is (true? (:mitsuho-hikari-both-refused fis)))
    (is (false? (:land-grant-executed fis)))
    (is (false? (:fulfillment-executed fis)))
    (is (false? (:quota-executed fis)))
    (is (false? (:ladder-advance-refused fis)))
    (is (false? (:live fis)))
    (is (zero? (:cash-usd-micros fis)))
    (is (= [] (:score-surface fis)))
    (is (= l0/PRIORITY-STACK (:priority-stack fis)))
    (is (map? (:held-stress fis)))
    (is (= "held" (get-in fis [:held-stress :disclosure-state])))
    (is (true? (get-in fis [:held-stress :all-inkind-produce-rails-full-chain-refused])))
    (is (true? (get-in fis [:held-stress :ladder-advance-refused])))
    (is (false? (get-in fis [:held-stress :land-grant-executed])))
    (is (true? (:held-stress-ladder-refused fis)))
    (is (true? (:held-stress-all-refused fis)))
    (is (= "held" (:disclosure-state fis-held)))
    (is (true? (:disclosure-held fis-held)))
    (is (false? (:entitlements-may-flow fis-held)))
    (is (true? (:all-inkind-produce-rails-full-chain-refused fis-held)))
    (is (true? (:ladder-advance-refused fis-held)))
    (is (false? (:land-grant-executed fis-held)))
    (is (false? (:live fis-held)))
    (is (zero? (:cash-usd-micros fis-held)))
    ;; vocation-only recovery (tooling+compute) for robotics/itonami job-loss + held-stress
    (is (= "open" (:disclosure-state voc)))
    (is (true? (:tooling-compute-both-refused voc)))
    (is (true? (:tooling-full-chain-refused voc)))
    (is (true? (:compute-full-chain-refused voc)))
    (is (false? (:fulfillment-executed voc)))
    (is (false? (:quota-executed voc)))
    (is (false? (:ladder-advance-refused voc)))
    (is (false? (:live voc)))
    (is (zero? (:cash-usd-micros voc)))
    (is (= [] (:score-surface voc)))
    (is (= l0/PRIORITY-STACK (:priority-stack voc)))
    (is (map? (:held-stress voc)))
    (is (= "held" (get-in voc [:held-stress :disclosure-state])))
    (is (true? (get-in voc [:held-stress :tooling-compute-both-refused])))
    (is (true? (get-in voc [:held-stress :ladder-advance-refused])))
    (is (false? (get-in voc [:held-stress :fulfillment-executed])))
    (is (false? (get-in voc [:held-stress :quota-executed])))
    (is (true? (:held-stress-ladder-refused voc)))
    (is (true? (:held-stress-both-refused voc)))
    (is (= "held" (:disclosure-state voc-held)))
    (is (true? (:disclosure-held voc-held)))
    (is (false? (:entitlements-may-flow voc-held)))
    (is (true? (:tooling-compute-both-refused voc-held)))
    (is (true? (:ladder-advance-refused voc-held)))
    (is (false? (:live voc-held)))
    (is (zero? (:cash-usd-micros voc-held)))
    ;; liquidity residual (member-principal warifu; loan never) + held-stress
    (is (= "open" (:disclosure-state liq)))
    (is (true? (:liquidity-receive-full-chain-refused liq)))
    (is (true? (:liquidity-member-principal liq)))
    (is (false? (:liquidity-loan-executed liq)))
    (is (zero? (:liquidity-cash-usd-micros liq)))
    (is (false? (:ladder-advance-refused liq)))
    (is (false? (:live liq)))
    (is (zero? (:cash-usd-micros liq)))
    (is (= [] (:score-surface liq)))
    (is (= l0/PRIORITY-STACK (:priority-stack liq)))
    (is (map? (:held-stress liq)))
    (is (= "held" (get-in liq [:held-stress :disclosure-state])))
    (is (true? (get-in liq [:held-stress :liquidity-receive-full-chain-refused])))
    (is (true? (get-in liq [:held-stress :ladder-advance-refused])))
    (is (false? (get-in liq [:held-stress :liquidity-loan-executed])))
    (is (true? (:held-stress-ladder-refused liq)))
    (is (true? (:held-stress-receive-refused liq)))
    (is (= "held" (:disclosure-state liq-held)))
    (is (true? (:disclosure-held liq-held)))
    (is (false? (:entitlements-may-flow liq-held)))
    (is (true? (:liquidity-receive-full-chain-refused liq-held)))
    (is (true? (:liquidity-member-principal liq-held)))
    (is (false? (:liquidity-loan-executed liq-held)))
    (is (true? (:ladder-advance-refused liq-held)))
    (is (false? (:live liq-held)))
    (is (zero? (:cash-usd-micros liq-held)))
    ;; all-seven capstone (six in-kind + liquidity residual) + held-stress
    (is (= "open" (:disclosure-state a7)))
    (is (true? (:all-inkind-produce-rails-full-chain-refused a7)))
    (is (true? (:liquidity-receive-full-chain-refused a7)))
    (is (true? (:all-seven-rails-receive-membrane-refused a7)))
    (is (true? (:liquidity-member-principal a7)))
    (is (false? (:liquidity-loan-executed a7)))
    (is (zero? (:liquidity-cash-usd-micros a7)))
    (is (false? (:land-grant-executed a7)))
    (is (false? (:fulfillment-executed a7)))
    (is (false? (:quota-executed a7)))
    (is (false? (:ladder-advance-refused a7)))
    (is (false? (:live a7)))
    (is (zero? (:cash-usd-micros a7)))
    (is (= [] (:score-surface a7)))
    (is (= l0/PRIORITY-STACK (:priority-stack a7)))
    (is (map? (:held-stress a7)))
    (is (= "held" (get-in a7 [:held-stress :disclosure-state])))
    (is (true? (get-in a7 [:held-stress :all-seven-rails-receive-membrane-refused])))
    (is (true? (get-in a7 [:held-stress :ladder-advance-refused])))
    (is (false? (get-in a7 [:held-stress :liquidity-loan-executed])))
    (is (false? (get-in a7 [:held-stress :land-grant-executed])))
    (is (true? (:held-stress-ladder-refused a7)))
    (is (true? (:held-stress-membrane-refused a7)))
    (is (= "held" (:disclosure-state a7-held)))
    (is (true? (:disclosure-held a7-held)))
    (is (false? (:entitlements-may-flow a7-held)))
    (is (true? (:all-seven-rails-receive-membrane-refused a7-held)))
    (is (true? (:ladder-advance-refused a7-held)))
    (is (false? (:liquidity-loan-executed a7-held)))
    (is (false? (:live a7-held)))
    (is (zero? (:cash-usd-micros a7-held)))))

(deftest test-enroll-with-mitsuho-default-refuse
  "Priority (3): mitsuho R1→gated DESIGN on fresh enroll; full-chain refuse offline."
  (let [e (l0/enroll-with-mitsuho base)]
    (is (= :R1-dry (get-in e [:food-package :phase])))
    (is (= :refused (get-in e [:food-gated-live-status :phase])))
    (is (false? (get-in e [:food-gated-live-status :admissible])))
    (is (= :refused (get-in e [:food-gated-receive-status :phase])))
    (is (false? (get-in e [:food-gated-receive-status :admissible])))
    (is (= :refused (get-in e [:food-gated-produce-status :phase])))
    (is (false? (get-in e [:food-gated-produce-status :produce-executed])))
    (is (true? (:mitsuho-full-chain-refused e)))
    (is (true? (get-in e [:mitsuho-membrane :mitsuho-full-chain-refused])))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (is (= [] (:score-surface e)))
    (pp/assert-no-public-scores! (:mitsuho-membrane e))))

(deftest test-enroll-with-mitsuho-when-held
  "Held disclosure → mitsuho R1 refused via hold-machine; still cash≡0."
  (let [e (l0/enroll-with-mitsuho (assoc base :disclosure stale-disc))]
    (is (= :held (:disclosure-state e)))
    (is (= :refused (get-in e [:food-package :phase])))
    (is (true? (:mitsuho-full-chain-refused e)))
    (is (nil? (:food-gated-live-status e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))))

(deftest test-enroll-with-hikari-default-refuse
  "Priority (3 energy): hikari R1→gated DESIGN on fresh enroll; full-chain refuse."
  (let [e (l0/enroll-with-hikari base)]
    (is (= :R1-dry (get-in e [:energy-package :phase])))
    (is (= :refused (get-in e [:energy-gated-live-status :phase])))
    (is (false? (get-in e [:energy-gated-live-status :admissible])))
    (is (= :refused (get-in e [:energy-gated-receive-status :phase])))
    (is (false? (get-in e [:energy-gated-produce-status :generate-executed] false)))
    (is (true? (:hikari-full-chain-refused e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (pp/assert-no-public-scores! (:hikari-membrane e))))

(deftest test-enroll-with-mitsuho-hikari
  "Food+energy membranes both refuse offline; mitsuho-hikari full-chain true."
  (let [e (l0/enroll-with-mitsuho-hikari base)]
    (is (true? (:mitsuho-full-chain-refused e)))
    (is (true? (:hikari-full-chain-refused e)))
    (is (true? (:mitsuho-hikari-full-chain-refused e)))
    (is (true? (get-in e [:hikari-membrane :mitsuho-hikari-full-chain-refused])))
    (is (= :open (:disclosure-state e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (is (= [] (:score-surface e)))))

(deftest test-enroll-with-care-default-refuse
  "Priority (3 care 孫/子): care-iyashi R1→gated DESIGN full-chain refuse offline."
  (let [e (l0/enroll-with-care base)]
    (is (= :R1-dry (get-in e [:care-package :phase])))
    (is (= :refused (get-in e [:care-gated-live-status :phase])))
    (is (false? (get-in e [:care-gated-live-status :admissible])))
    (is (= :refused (get-in e [:care-gated-receive-status :phase])))
    (is (false? (get-in e [:care-gated-produce-status :care-delivery-executed] false)))
    (is (true? (:care-full-chain-refused e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (pp/assert-no-public-scores! (:care-membrane e))))

(deftest test-enroll-with-care-mitsuho-hikari
  "Care-first substrate: care+food+energy all refuse offline."
  (let [e (l0/enroll-with-care-mitsuho-hikari base)]
    (is (true? (:care-full-chain-refused e)))
    (is (true? (:mitsuho-full-chain-refused e)))
    (is (true? (:hikari-full-chain-refused e)))
    (is (true? (:care-mitsuho-hikari-full-chain-refused e)))
    (is (= :open (:disclosure-state e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (is (= [] (:score-surface e)))))

(deftest test-enroll-with-housing-default-refuse
  "Priority (3 housing 孫/子): housing-commons R1→gated; land-grant never executed."
  (let [e (l0/enroll-with-housing base)]
    (is (= :R1-dry (get-in e [:housing-package :phase])))
    (is (= :refused (get-in e [:housing-gated-live-status :phase])))
    (is (false? (get-in e [:housing-gated-live-status :admissible])))
    (is (false? (get-in e [:housing-gated-live-status :land-grant-executed] false)))
    (is (= :refused (get-in e [:housing-gated-receive-status :phase])))
    (is (false? (get-in e [:housing-gated-produce-status :land-grant-executed] false)))
    (is (true? (:housing-full-chain-refused e)))
    (is (false? (:land-grant-executed e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (pp/assert-no-public-scores! (:housing-membrane e))))

(deftest test-enroll-with-care-housing
  "L4 multi-gen substrate first: care+housing both refuse; land-grant false."
  (let [e (l0/enroll-with-care-housing base)]
    (is (true? (:care-full-chain-refused e)))
    (is (true? (:housing-full-chain-refused e)))
    (is (true? (:care-housing-full-chain-refused e)))
    (is (false? (:land-grant-executed e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))))

(deftest test-enroll-with-multi-gen-substrate
  "care+housing+food+energy all refuse offline (孫/子 substrate + food/energy)."
  (let [e (l0/enroll-with-multi-gen-substrate base)]
    (is (true? (:care-full-chain-refused e)))
    (is (true? (:housing-full-chain-refused e)))
    (is (true? (:mitsuho-full-chain-refused e)))
    (is (true? (:hikari-full-chain-refused e)))
    (is (true? (:care-housing-full-chain-refused e)))
    (is (true? (:care-housing-mitsuho-hikari-full-chain-refused e)))
    (is (false? (:land-grant-executed e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (is (= [] (:score-surface e)))))

(deftest test-enroll-with-tooling-compute
  "Vocation rails after displacement: tooling+compute full-chain refuse offline."
  (let [e (-> (l0/enroll base)
              (l0/attach-tooling-r1-scaffold)
              (l0/attach-compute-r1-scaffold))]
    (is (true? (:tooling-full-chain-refused e)))
    (is (true? (:compute-full-chain-refused e)))
    (is (true? (:tooling-compute-full-chain-refused e)))
    (is (false? (get-in e [:tooling-gated-produce-status :fulfillment-executed] false)))
    (is (false? (get-in e [:compute-gated-produce-status :quota-executed] false)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))))

(deftest test-enroll-with-full-inkind-rails
  "Six in-kind rails (multi-gen + vocation) all refuse offline."
  (let [e (l0/enroll-with-full-inkind-rails base)]
    (is (true? (:care-full-chain-refused e)))
    (is (true? (:housing-full-chain-refused e)))
    (is (true? (:mitsuho-full-chain-refused e)))
    (is (true? (:hikari-full-chain-refused e)))
    (is (true? (:tooling-full-chain-refused e)))
    (is (true? (:compute-full-chain-refused e)))
    (is (true? (:all-inkind-produce-rails-full-chain-refused e)))
    (is (true? (:tooling-compute-full-chain-refused e)))
    (is (false? (:land-grant-executed e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (is (= [] (:score-surface e)))))

(deftest test-enroll-with-liquidity-residual
  "Liquidity residual member-principal; loan never; receive full-chain refuse."
  (let [e (l0/enroll-with-liquidity base)]
    (is (= :R1-dry (get-in e [:liquidity-package :phase])))
    (is (true? (get-in e [:liquidity-package :member-principal])))
    (is (zero? (get-in e [:liquidity-package :cash-usd-micros])))
    (is (= :refused (get-in e [:liquidity-gated-live-status :phase])))
    (is (= :refused (get-in e [:liquidity-gated-receive-status :phase])))
    (is (false? (get-in e [:liquidity-gated-receive-status :loan-invoked])))
    (is (true? (:liquidity-receive-full-chain-refused e)))
    (is (false? (:liquidity-loan-executed e)))
    (is (zero? (:liquidity-cash-usd-micros e)))
    (is (false? (:live e)))
    (pp/assert-no-public-scores! (:liquidity-membrane e))))

(deftest test-enroll-with-all-seven-rails
  "Six in-kind + liquidity residual: all-seven receive-membrane refuse offline."
  (let [e (l0/enroll-with-all-seven-rails base)]
    (is (true? (:all-inkind-produce-rails-full-chain-refused e)))
    (is (true? (:liquidity-receive-full-chain-refused e)))
    (is (true? (:all-seven-rails-receive-membrane-refused e)))
    (is (true? (:liquidity-member-principal e)))
    (is (false? (:liquidity-loan-executed e)))
    (is (zero? (:liquidity-cash-usd-micros e)))
    (is (false? (:land-grant-executed e)))
    (is (false? (:live e)))
    (is (zero? (:cash-usd-micros e)))
    (is (= [] (:score-surface e)))))

(deftest test-enroll-batch
  (let [batch (l0/enroll-batch
               [(assoc base :subject-did "did:web:etzhayyim.com:displaced:w0"
                       :member-signature "sig-w0")
                (assoc base :subject-did "did:web:etzhayyim.com:displaced:w1"
                       :member-signature "sig-w1"
                       :disclosure stale-disc)]
               :with-all-seven-rails? true)]
    (is (= 2 (:count batch)))
    (is (= 1 (:disclosure-open batch)))
    (is (= 1 (:disclosure-held batch)))
    (is (= 2 (:care-full-chain-refused-n batch)))
    (is (= 2 (:housing-full-chain-refused-n batch)))
    (is (= 2 (:tooling-full-chain-refused-n batch)))
    (is (= 2 (:compute-full-chain-refused-n batch)))
    (is (= 2 (:liquidity-receive-full-chain-refused-n batch)))
    (is (= 2 (:all-inkind-produce-rails-full-chain-refused-n batch)))
    (is (= 2 (:all-seven-rails-receive-membrane-refused-n batch)))
    (is (false? (:any-land-grant-executed? batch)))
    (is (false? (:any-liquidity-loan-executed? batch)))
    (is (zero? (:total-liquidity-cash-usd-micros batch)))
    (is (false? (:live batch)))
    (is (zero? (:cash-usd-micros batch)))
    (is (= [] (:score-surface batch)))))

(deftest test-enroll-record-lexicon-shape
  (let [rec (l0/enroll-record (l0/enroll base))]
    (is (= false (:published rec)))
    (is (= 0 (:cashUsdMicros rec)))
    (is (= false (:serverHeldKey rec)))
    (is (= "L0" (:stage rec)))
    (is (= "open" (:disclosureState rec)))
    (is (true? (:entitlementsMayFlow rec)))
    (is (false? (:live rec)))))

(deftest test-cash-invariant
  (is (thrown? #?(:clj clojure.lang.ExceptionInfo :cljs js/Error)
               (l0/assert-no-cash! {:cash-usd-micros 1}))))
