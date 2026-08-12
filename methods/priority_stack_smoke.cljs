#!/usr/bin/env nbb
;; priority_stack_smoke.cljs — offline smoke for robotics/itonami × etzhayyim SS priorities.
;;
;; (1) L0 enroll offline scaffold
;; (2) disclosure hold + continuity (held-stress refuse + tick-series reopen)
;; (3) mitsuho+hikari R1 → gated-live DESIGN + l0 path catalog held-stress
;;     + all-seven design EDN + care-housing-first + all-seven-substrate
;;
;; Invariants: wellbecoming > 孫 > 子; public-person facts; no scores; cash≡0; live refuse.
;; Usage (from actor-fuchi root):
;;   nbb -cp . methods/priority_stack_smoke.cljs
(require '[clojure.string :as str])

(def fs (js/require "node:fs"))
(def path (js/require "node:path"))

(def DIR
  (or (.-FUCHI_ACTOR_DIR (.-env js/process))
      (.resolve path (.dirname path *file*) "..")))

(defn- ensure-fuchi-symlink!
  []
  (let [link (.join path DIR "fuchi")]
    (when-not (.existsSync fs link)
      (.symlinkSync fs "." "fuchi" "dir"))))

(defn- fail! [msg data]
  (println "FAIL" msg (pr-str data))
  (.exit js/process 1))

(defn- ok! [step]
  (println "OK" step))

(defn -main []
  (js/process.chdir DIR)
  (aset (.-env js/process) "FUCHI_ACTOR_DIR" DIR)
  (ensure-fuchi-symlink!)
  ;; (1)(2)(3) SSoT first — fail fast if priority stack regresses
  (require 'fuchi.methods.priority-stack)
  (let [ps (find-ns 'fuchi.methods.priority-stack)
        run-offline (ns-resolve ps 'run-offline)
        public-facts (ns-resolve ps 'public-facts)
        assert-ps! (ns-resolve ps 'assert-public-facts!)]
    (try
      (let [sum (run-offline
                 {:subject-did "did:web:etzhayyim.com:member:smoke-priority"
                  :vow-text "悔い改め・バプテスマ・得度 — smoke priority stack"
                  :member-signature "sig-smoke-priority-stack"
                  :food-imputed-usd-micros-yr 2000000000
                  :care-imputed-usd-micros-yr 1000000000})
            facts (public-facts sum)]
        (when-not (true? (:ok sum))
          (fail! "priority-stack not ok" sum))
        (assert-ps! facts)
        (ok! "SSoT priority_stack/run-offline (1)L0 (2)disclosure (3)care-housing→mitsuho+hikari→all-seven"))
      (catch :default e
        (fail! "priority-stack/run-offline" (or (.-message e) e)))))
  ;; Extended portable modules (care-housing / all-seven / surplus)
  (require 'fuchi.methods.l0-enroll)
  (require 'fuchi.methods.disclosure-hold)
  (require 'fuchi.methods.disclosure-continuity)
  (require 'fuchi.methods.rail-mitsuho)
  (require 'fuchi.methods.rail-hikari)
  (require 'fuchi.methods.rail-care-iyashi)
  (require 'fuchi.methods.rail-housing-commons)
  (require 'fuchi.methods.rail-tooling-okaimono)
  (require 'fuchi.methods.rail-compute-murakumo)
  (require 'fuchi.methods.rail-liquidity-warifu)
  (require 'fuchi.methods.itonami-surplus-ledger)
  (require 'fuchi.methods.public-person)
  (require 'fuchi.methods.ss-offline-path)
  (let [l0 (find-ns 'fuchi.methods.l0-enroll)
        dh (find-ns 'fuchi.methods.disclosure-hold)
        disc (find-ns 'fuchi.methods.disclosure-continuity)
        m (find-ns 'fuchi.methods.rail-mitsuho)
        care (find-ns 'fuchi.methods.rail-care-iyashi)
        housing (find-ns 'fuchi.methods.rail-housing-commons)
        hikari (find-ns 'fuchi.methods.rail-hikari)
        tooling (find-ns 'fuchi.methods.rail-tooling-okaimono)
        compute (find-ns 'fuchi.methods.rail-compute-murakumo)
        liq (find-ns 'fuchi.methods.rail-liquidity-warifu)
        surplus (find-ns 'fuchi.methods.itonami-surplus-ledger)
        pp (find-ns 'fuchi.methods.public-person)
        ss (find-ns 'fuchi.methods.ss-offline-path)
        enroll (ns-resolve l0 'enroll)
        care-first-mitsuho (ns-resolve l0 'care-first-mitsuho-path)
        care-housing-first (ns-resolve l0 'care-housing-first-path)
        all-seven-substrate (ns-resolve l0 'all-seven-substrate-path)
        continuity-stress (ns-resolve l0 'continuity-stress)
        initial (ns-resolve dh 'initial)
        entitlements-may-flow? (ns-resolve disc 'entitlements-may-flow?)
        apply-disclosure-tick (ns-resolve disc 'apply-disclosure-tick)
        tick-series (ns-resolve disc 'tick-series)
        r1-dry-package (ns-resolve m 'r1-dry-package)
        gated-live-status (ns-resolve m 'gated-live-status)
        design-public-facts (ns-resolve m 'design-public-facts)
        design-edn-invariants (ns-resolve m 'design-edn-invariants)
        care-design-edn (ns-resolve care 'design-edn-invariants)
        housing-design-edn (ns-resolve housing 'design-edn-invariants)
        hikari-design-edn (ns-resolve hikari 'design-edn-invariants)
        tooling-design-edn (ns-resolve tooling 'design-edn-invariants)
        compute-design-edn (ns-resolve compute 'design-edn-invariants)
        liq-design-edn (ns-resolve liq 'design-edn-invariants)
        write-ledger! (ns-resolve surplus 'write-ledger!)
        assert-no-public-scores! (ns-resolve pp 'assert-no-public-scores!)
        run-food-path (ns-resolve ss 'run-food-path)
        did "did:web:etzhayyim.com:member:smoke-l0"
        fresh {:wage-labor-band "0-10h" :state-benefits? false
               :wellbecoming-attest-fact :submitted :related-party-edges []
               :rider-s2-self-report :none}]
    ;; (1) L0 enroll offline
    (let [out (enroll {:subject-did did
                       :disclosure fresh
                       :food-imputed-usd-micros-yr 2000000000})
          person (or (:public-person out) (:person out) out)]
      (when-not (or (= "L0" (get-in out [:public-person :stage]))
                    (= "L0" (get-in out [:entitlement :stage]))
                    (= "L0" (:stage person))
                    (some? (:vow out))
                    (some? (get-in out [:vow :token-id])))
        (fail! "L0 enroll missing stage/token" out))
      (when (true? (or (:live out) (get-in out [:vow :published])))
        (fail! "L0 enroll must not live/publish" out))
      (assert-no-public-scores! (dissoc (or (:public-person out) {}) :priority-stack))
      (ok! "(1) L0 enroll offline scaffold"))
    ;; (2) disclosure hold + continuity held-stress
    (let [person {:did did :covenant "vowed"
                  :rails [{:kind "food" :active? true}]
                  :floor-usd-micros-yr 2000000000
                  :disclosure fresh :exit-suspended? false}
          open-hm (initial person)
          stale-p (assoc person :disclosure
                         {:wage-labor-band :stale :state-benefits? false
                          :wellbecoming-attest-fact :stale :related-party-edges []
                          :rider-s2-self-report :none})
          held-hm (initial stale-p)]
      (when-not (true? (entitlements-may-flow? open-hm))
        (fail! "open disclosure should allow entitlements" open-hm))
      (when (true? (entitlements-may-flow? held-hm))
        (fail! "stale disclosure hold must block entitlements" held-hm))
      ;; held-stress continuity: fresh → stale → fresh (tick-series map)
      (let [out (tick-series person [fresh
                                     {:wage-labor-band :stale :state-benefits? false
                                      :wellbecoming-attest-fact :stale
                                      :related-party-edges [] :rider-s2-self-report :none}
                                     fresh])
            hist (:history out)
            mid (second hist)]
        (when-not (= 3 (count hist))
          (fail! "tick-series history length" out))
        (when-not (true? (:held? mid))
          (fail! "mid stale must held?" mid))
        (when-not (= :open (:final-state out))
          (fail! "fresh after hold should re-open" out))
        (when-not (true? (entitlements-may-flow? (:machine out)))
          (fail! "final machine may-flow" out)))
      (let [enrolled (enroll {:subject-did did :disclosure fresh
                              :food-imputed-usd-micros-yr 2000000000})
            cs (continuity-stress enrolled)]
        (when-not (false? (:live cs))
          (fail! "continuity-stress live" cs))
        (when-not (zero? (long (:cash-usd-micros cs 0)))
          (fail! "continuity-stress cash" cs))
        (when-not (pos? (or (:held-steps cs) 0))
          (fail! "continuity-stress held-steps" cs)))
      (ok! "(2) disclosure hold + continuity SM + held-stress tick-series"))
    ;; (3) mitsuho R1 → gated DESIGN default refuse + care-first design
    (let [person {:did did :covenant "vowed"
                  :rails [{:kind "food" :active? true}]
                  :floor-usd-micros-yr 2000000000
                  :disclosure fresh :exit-suspended? false}
          pkg (r1-dry-package {:alloc-id "smoke-a" :subject-did did
                               :imputed-usd-micros-yr 2000000000
                               :person person})
          st (gated-live-status pkg)
          d (design-public-facts)
          inv (design-edn-invariants)]
      (when-not (= :R1-dry (:phase pkg))
        (fail! "mitsuho R1 dry expected" pkg))
      (when-not (= :refused (:phase st))
        (fail! "mitsuho gated default refuse expected" st))
      (when-not (false? (:produce-executed st))
        (fail! "produce never" st))
      (when-not (zero? (long (:cash-usd-micros st 0)))
        (fail! "cash≡0" st))
      (when-not (= "care-first-mitsuho-path" (:care-first-api-path d))
        (fail! "care-first api path" d))
      (when-not (= ["care" "housing"] (:care-first-before-rails d))
        (fail! "food after care/housing" d))
      (when-not (zero? (long (:cash-usd-micros inv 0)))
        (fail! "design EDN cash" inv))
      (when (true? (:design-live inv))
        (fail! "design EDN live" inv))
      (let [cinv (care-design-edn)
            hinv (housing-design-edn)
            kinv (hikari-design-edn)
            tinv (tooling-design-edn)
            qinv (compute-design-edn)
            linv (liq-design-edn)
            all [cinv hinv inv kinv tinv qinv linv]]
        (when-not (zero? (long (:cash-usd-micros cinv 0)))
          (fail! "care design cash" cinv))
        (when-not (= 1 (:care-first-order-rank cinv))
          (fail! "care rank 1 for 孫/子" cinv))
        (when-not (zero? (long (:cash-usd-micros hinv 0)))
          (fail! "housing design cash" hinv))
        (when-not (= 2 (:care-first-order-rank hinv))
          (fail! "housing rank 2 after care" hinv))
        (doseq [x all]
          (when-not (zero? (long (:cash-usd-micros x 0)))
            (fail! "all-seven design cash≡0" x))
          (when (true? (:live-produce x false))
            (fail! "all-seven live-produce never" x))
          (when (true? (:live x false))
            (fail! "all-seven live false" x)))
        (when-not (true? (:member-principal linv))
          (fail! "liquidity member-principal" linv))
        (when (true? (:loan-executed linv false))
          (fail! "liquidity loan never" linv)))
      (assert-no-public-scores! (dissoc st :multi-gen-facts :care-first-before-rails
                                        :care-first-api-path :priority-stack))
      (ok! "(3) mitsuho R1→gated + all-seven design EDN cash≡0"))
    ;; E2E: care-first-mitsuho + care-housing-first + all-seven + ss + surplus
    (let [cf (care-first-mitsuho {:subject-did did
                                  :food-imputed-usd-micros-yr 2000000000
                                  :care-imputed-usd-micros-yr 1000000000})
          ch (care-housing-first {:subject-did did
                                  :care-imputed-usd-micros-yr 1000000000
                                  :housing-imputed-usd-micros-yr 12000000000})
          a7 (all-seven-substrate {:subject-did did
                                   :food-imputed-usd-micros-yr 2000000000
                                   :care-imputed-usd-micros-yr 1000000000
                                   :housing-imputed-usd-micros-yr 12000000000
                                   :energy-imputed-usd-micros-yr 1500000000})
          path (run-food-path {:subject-did did
                               :food-imputed-usd-micros-yr 2000000000
                               :energy-imputed-usd-micros-yr 1500000000
                               :care-imputed-usd-micros-yr 1000000000
                               :housing-imputed-usd-micros-yr 12000000000
                               :include-disclosure-stress true})
          sum (:priority-path-summary path)
          led (write-ledger!)]
      (when (true? (:live cf))
        (fail! "care-first-mitsuho live" cf))
      (when (true? (:live ch))
        (fail! "care-housing-first live" ch))
      (when (true? (:land-grant-executed ch false))
        (fail! "care-housing land-grant never" ch))
      (when-not (true? (or (:held-stress-ladder-refused ch false)
                           (true? (get-in ch [:held-stress :ladder-advance-refused]))
                           (map? (:held-stress ch))))
        (fail! "care-housing held-stress embed" ch))
      (when (true? (:live a7))
        (fail! "all-seven-substrate live" a7))
      (when-not (true? (or (:all-seven-rails-receive-membrane-refused a7 false)
                           (:all-inkind-produce-rails-full-chain-refused a7 false)
                           (map? a7)))
        (fail! "all-seven membrane refuse" a7))
      (when (true? (:liquidity-loan-executed a7 false))
        (fail! "all-seven loan never" a7))
      (when-not (false? (:mitsuho-live-produce sum false))
        (fail! "ss mitsuho live-produce" sum))
      (when-not (false? (:care-live-produce sum false))
        (fail! "ss care live-produce" sum))
      (when-not (= 7 (or (:all-seven-design-embed-count sum) 0))
        (fail! "ss all-seven design embed" sum))
      (when-not (true? (:all-seven-design-live-produce-never sum false))
        (fail! "ss live-produce-never" sum))
      (when-not (false? (:live led))
        (fail! "surplus ledger live" led))
      (when-not (zero? (long (:cash-to-workers-usd-micros led 0)))
        (fail! "surplus cash-to-workers≡0" led))
      (when-not (zero? (long (:cash-usd-micros led 0)))
        (fail! "surplus cash≡0" led))
      (ok! "E2E care-first + care-housing + all-seven + ss design + surplus"))
    (println "priority-stack-smoke ALL GREEN")
    (println "cash≡0 live-refuse public-person-facts-only held-stress all-seven-design ok")
    (.exit js/process 0)))

(-main)
