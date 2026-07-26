(ns fuchi.methods.test-pipeline-audit-ledger
  "Pipeline audit ledger tests — portable under bb and nbb."
  (:require [clojure.test :refer [deftest is]]
            [fuchi.methods.pipeline-audit-ledger :as audit]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.displacement-pipeline :as pipe]))

#?(:cljs
   (def ^:private fs (js/require "node:fs")))

(defn- path-exists? [p]
  #?(:clj (.exists (java.io.File. (str p)))
     :cljs (.existsSync fs (str p))))

(defn- now-ms []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(deftest test-event-from-pipeline-shape
  (let [fake {:pipeline "displacement-ss-offline"
              :admissible-cohorts 1
              :tenure-subjects 2
              :all-live-refused true
              :scorecard {:scorecard/admissible-cohorts 1
                          :scorecard/refused-cohorts 1
                          :scorecard/enrolled-subjects 2
                          :scorecard/tenure-subjects 2
                          :scorecard/tenure-stage-counts {"L6" 2}
                          :scorecard/committed-usd-micros-yr 100
                          :scorecard/headroom-usd-micros-yr 50
                          :scorecard/booked-entries 12
                          :scorecard/tenure-booked-entries 12
                          :scorecard/all-live-refused true
                          :scorecard/gov-flowable-committed-usd-micros 40
                          :scorecard/gov-post-ratify-committed-usd-micros 100
                          :scorecard/tenure-gov-flowable-committed-usd-micros 40
                          :scorecard/tenure-gov-post-ratify-committed-usd-micros 100
                          :scorecard/l4-disclosure-open 2
                          :scorecard/l4-disclosure-held 0
                          :scorecard/tenure-disclosure-open 2
                          :scorecard/tenure-disclosure-held 0
                          :scorecard/mitsuho-r1-dry 2
                          :scorecard/mitsuho-gated-refused 2
                          :scorecard/mitsuho-produce-executed 0
                          :scorecard/hikari-r1-dry 2
                          :scorecard/hikari-gated-refused 2
                          :scorecard/hikari-generate-executed 0
                          :scorecard/care-r1-dry 2
                          :scorecard/care-gated-refused 2
                          :scorecard/care-delivery-executed 0
                          :scorecard/housing-r1-dry 2
                          :scorecard/housing-gated-refused 2
                          :scorecard/housing-land-grant-executed 0
                          :scorecard/housing-council-held 2
                          :scorecard/displacement-membrane-subjects 2
                          :scorecard/displacement-held-stress-subjects 2
                          :scorecard/displacement-held-stress-ladder-refused 2
                          :scorecard/tenure-held-stress-subjects 2
                          :scorecard/tenure-held-stress-ladder-refused 2
                          :scorecard/tenure-held-stress-carried 2
                          :scorecard/gov-held-stress-subjects 2
                          :scorecard/gov-held-stress-ladder-refused 2
                          :scorecard/tenure-gov-held-stress-subjects 2
                          :scorecard/tenure-gov-held-stress-ladder-refused 2
                          :scorecard/displacement-care-housing-full-chain-refused 2
                          :scorecard/displacement-all-inkind-full-chain-refused 2
                          :scorecard/displacement-all-seven-receive-membrane-refused 2
                          :scorecard/displacement-liquidity-recv-refused 2
                          :scorecard/r2-status-count 12
                          :scorecard/r2-refused 12
                          :scorecard/r2-executed 0
                          :scorecard/all-r2-not-executed true
                          :scorecard/priority-stack-offline
                          {:ok true
                           :l0-stage "L0"
                           :l0-published false
                           :disclosure-open-may-flow true
                           :disclosure-stale-held true
                           :disclosure-tick-final "open"
                           :care-housing-api-path "care-housing-first-path"
                           :care-housing-both-refused true
                           :care-housing-land-grant-executed false
                           :care-housing-held-stress-ladder-refused true
                           :all-seven-api-path "all-seven-substrate-path"
                           :all-seven-inkind-refused true
                           :all-seven-membrane-refused true
                           :all-seven-liquidity-refused true
                           :all-seven-loan-executed false
                           :all-seven-land-grant-executed false
                           :all-seven-held-stress-ladder-refused true
                           :mitsuho-r1-phase "R1-dry"
                           :mitsuho-gated-phase "refused"
                           :mitsuho-produce-executed false
                           :mitsuho-care-first-api-path "care-first-mitsuho-path"
                           :mitsuho-held-stress-ladder-refused true
                           :hikari-r1-phase "R1-dry"
                           :hikari-gated-phase "refused"
                           :hikari-produce-executed false
                           :hikari-care-first-api-path "care-first-hikari-path"
                           :hikari-held-stress-ladder-refused true
                           :l0-paths-count 9
                           :l0-paths-all-held-stress true
                           :live false
                           :cash-usd-micros 0
                           :score-surface []}
                          :scorecard/ss-priority-path
                          {:rails-gated-count 7
                           :rails-gated-admissible-count 0
                           :all-rails-gated-refused true
                           :r2-status-count 7
                           :r2-executed-count 0
                           :all-r2-not-executed true
                           :l0-published false
                           :disclosure-state "open"
                           :housing-land-grant-executed false
                           :care-live-produce false
                           :housing-live-produce false
                           :care-care-first-api-path "care-housing-first-path"
                           :housing-care-first-api-path "care-housing-first-path"
                           :care-design-rail-kind "care-iyashi"
                           :housing-design-rail-kind "housing-commons"
                           :mitsuho-live-produce false
                           :hikari-live-produce false
                           :mitsuho-care-first-api-path "care-first-mitsuho-path"
                           :hikari-care-first-api-path "care-first-hikari-path"
                           :tooling-live-produce false
                           :compute-live-produce false
                           :liquidity-live-produce false
                           :tooling-care-first-api-path "vocation-recovery-path"
                           :compute-care-first-api-path "vocation-recovery-path"
                           :liquidity-care-first-api-path "liquidity-residual-path"
                           :all-seven-design-embed-count 7
                           :all-seven-design-live-produce-never true}
                          :scorecard/l0-care-first-mitsuho
                          {:disclosure-state "open"
                           :care-mitsuho-both-refused true
                           :mitsuho-live-produce false
                           :mitsuho-produce-executed false
                           :care-delivery-executed false
                           :care-first-api-path "care-first-mitsuho-path"
                           :ladder-advance-refused false
                           :held-stress-ladder-refused true}
                          :scorecard/l0-care-first-hikari
                          {:care-hikari-both-refused true
                           :hikari-live-produce false
                           :hikari-generate-executed false
                           :care-first-api-path "care-first-hikari-path"
                           :held-stress-ladder-refused true}
                          :scorecard/l0-care-first-mitsuho-hikari
                          {:care-mitsuho-hikari-all-refused true
                           :mitsuho-live-produce false
                           :hikari-live-produce false
                           :held-stress-ladder-refused true}
                          :scorecard/rail-design-catalog
                          {:catalog-id "fuchi.rail-design-catalog"
                           :rail-count 7
                           :ok-count 7
                           :live-produce-never true
                           :all-cash-zero true
                           :all-live-false true
                           :invariants {:all-seven-design true
                                        :live-produce-never true
                                        :loan-never true
                                        :land-grant-never true}}}
        ev (audit/event-from-pipeline fake :run-id "test-run-1")]
    (is (= "test-run-1" (:audit/id ev)))
    (is (true? (:audit/all-live-refused ev)))
    (is (= 40 (:audit/gov-flowable-committed-usd-micros ev)))
    (is (= 100 (:audit/gov-post-ratify-committed-usd-micros ev)))
    (is (= 2 (:audit/l4-disclosure-open ev)))
    (is (= 0 (:audit/l4-disclosure-held ev)))
    (is (= 2 (:audit/tenure-disclosure-open ev)))
    (is (= 2 (:audit/mitsuho-gated-refused ev)))
    (is (= 2 (:audit/care-gated-refused ev)))
    (is (= 2 (:audit/housing-gated-refused ev)))
    (is (= 0 (:audit/housing-land-grant-executed ev)))
    (is (= 0 (:audit/mitsuho-produce-executed ev)))
    (is (= 2 (:audit/displacement-membrane-subjects ev)))
    (is (= 2 (:audit/displacement-held-stress-subjects ev)))
    (is (= 2 (:audit/displacement-held-stress-ladder-refused ev)))
    (is (= 2 (:audit/tenure-held-stress-subjects ev)))
    (is (= 2 (:audit/tenure-held-stress-ladder-refused ev)))
    (is (= 2 (:audit/tenure-held-stress-carried ev)))
    (is (= 2 (:audit/gov-held-stress-subjects ev)))
    (is (= 2 (:audit/gov-held-stress-ladder-refused ev)))
    (is (= 2 (:audit/tenure-gov-held-stress-subjects ev)))
    (is (= 2 (:audit/tenure-gov-held-stress-ladder-refused ev)))
    (is (= 2 (:audit/displacement-all-inkind-full-chain-refused ev)))
    (is (= 2 (:audit/displacement-all-seven-receive-membrane-refused ev)))
    (is (= 2 (:audit/displacement-liquidity-recv-refused ev)))
    (is (= 12 (:audit/r2-status-count ev)))
    (is (= 12 (:audit/r2-refused ev)))
    (is (= 0 (:audit/r2-executed ev)))
    (is (true? (:audit/all-r2-not-executed ev)))
    (is (= 7 (:audit/ss-rails-gated-count ev)))
    (is (true? (:audit/ss-all-rails-gated-refused ev)))
    (is (= 7 (:audit/ss-r2-status-count ev)))
    (is (zero? (:audit/ss-r2-executed-count ev)))
    (is (true? (:audit/ss-all-r2-not-executed ev)))
    (is (false? (:audit/ss-l0-published ev)))
    (is (= "open" (:audit/ss-disclosure-state ev)))
    (is (false? (:audit/ss-housing-land-grant-executed ev)))
    ;; priority stack SSoT (1)(2)(3) on audit event
    (is (true? (:audit/priority-stack-ok ev)))
    (is (= "L0" (:audit/priority-stack-l0-stage ev)))
    (is (false? (:audit/priority-stack-l0-published ev)))
    (is (true? (:audit/priority-stack-disclosure-open-may-flow ev)))
    (is (true? (:audit/priority-stack-disclosure-stale-held ev)))
    (is (= "open" (:audit/priority-stack-disclosure-tick-final ev)))
    (is (= "care-housing-first-path" (:audit/priority-stack-care-housing-api ev)))
    (is (true? (:audit/priority-stack-care-housing-both-refused ev)))
    (is (false? (:audit/priority-stack-care-housing-land-grant ev)))
    (is (true? (:audit/priority-stack-care-housing-held-stress ev)))
    (is (= "all-seven-substrate-path" (:audit/priority-stack-all-seven-api ev)))
    (is (true? (:audit/priority-stack-all-seven-membrane-refused ev)))
    (is (false? (:audit/priority-stack-all-seven-loan-executed ev)))
    (is (false? (:audit/priority-stack-all-seven-land-grant ev)))
    (is (true? (:audit/priority-stack-all-seven-held-stress ev)))
    (is (= "R1-dry" (:audit/priority-stack-mitsuho-r1-phase ev)))
    (is (= "refused" (:audit/priority-stack-mitsuho-gated-phase ev)))
    (is (false? (:audit/priority-stack-mitsuho-produce-executed ev)))
    (is (= "care-first-mitsuho-path" (:audit/priority-stack-mitsuho-care-first-api ev)))
    (is (true? (:audit/priority-stack-mitsuho-held-stress-ladder-refused ev)))
    (is (= "R1-dry" (:audit/priority-stack-hikari-r1-phase ev)))
    (is (= "refused" (:audit/priority-stack-hikari-gated-phase ev)))
    (is (false? (:audit/priority-stack-hikari-produce-executed ev)))
    (is (= "care-first-hikari-path" (:audit/priority-stack-hikari-care-first-api ev)))
    (is (true? (:audit/priority-stack-hikari-held-stress-ladder-refused ev)))
    (is (= 9 (:audit/priority-stack-l0-paths-count ev)))
    (is (true? (:audit/priority-stack-l0-paths-all-held-stress ev)))
    (is (= 7 (:audit/rail-design-rail-count ev)))
    (is (= 7 (:audit/rail-design-ok-count ev)))
    (is (true? (:audit/rail-design-live-produce-never ev)))
    (is (true? (:audit/rail-design-all-cash-zero ev)))
    (is (true? (:audit/rail-design-all-live-false ev)))
    (is (true? (:audit/rail-design-all-seven ev)))
    (is (false? (:audit/ss-care-live-produce ev)))
    (is (false? (:audit/ss-housing-live-produce ev)))
    (is (= "care-housing-first-path" (:audit/ss-care-care-first-api-path ev)))
    (is (= "care-housing-first-path" (:audit/ss-housing-care-first-api-path ev)))
    (is (= "care-iyashi" (:audit/ss-care-design-rail-kind ev)))
    (is (= "housing-commons" (:audit/ss-housing-design-rail-kind ev)))
    (is (false? (:audit/ss-mitsuho-live-produce ev)))
    (is (false? (:audit/ss-hikari-live-produce ev)))
    (is (= "care-first-mitsuho-path" (:audit/ss-mitsuho-care-first-api-path ev)))
    (is (= "care-first-hikari-path" (:audit/ss-hikari-care-first-api-path ev)))
    (is (false? (:audit/ss-tooling-live-produce ev)))
    (is (false? (:audit/ss-compute-live-produce ev)))
    (is (false? (:audit/ss-liquidity-live-produce ev)))
    (is (= 7 (:audit/ss-all-seven-design-embed-count ev)))
    (is (true? (:audit/ss-all-seven-design-live-produce-never ev)))
    (is (false? (:audit/l0-care-first-mitsuho-live-produce ev)))
    (is (false? (:audit/l0-care-first-mitsuho-produce-executed ev)))
    (is (false? (:audit/l0-care-first-care-delivery-executed ev)))
    (is (= "care-first-mitsuho-path" (:audit/l0-care-first-api-path ev)))
    (is (false? (:audit/l0-care-first-hikari-live-produce ev)))
    (is (false? (:audit/l0-care-first-hikari-generate-executed ev)))
    (is (false? (:audit/l0-care-first-mitsuho-hikari-mitsuho-live-produce ev)))
    (is (false? (:audit/l0-care-first-mitsuho-hikari-hikari-live-produce ev)))
    (is (= 0 (:audit/cash-usd-micros ev)))
    (is (= 0 (:audit/cash-to-workers-usd-micros ev)))
    (is (false? (:audit/live ev)))
    (is (= [] (:audit/score-surface ev)))
    (pp/assert-no-public-scores! ev)))

(deftest test-append-and-summary
  (let [result (pipe/run! :max-slots 1)
        a (audit/append-from-pipeline! result :run-id (str "t-" (now-ms)))
        events (audit/read-all)
        sum (audit/summary)
        last-ev (last events)]
    (is (path-exists? (:path a)))
    (is (seq events))
       (is (pos? (:runs sum)))
       (is (true? (:all-runs-live-refused sum)))
       (is (pos? (:total-l4-disclosure-open sum)))
       (is (pos? (:total-mitsuho-gated-refused sum)))
       (is (pos? (:total-care-gated-refused sum)))
       (is (pos? (:total-housing-gated-refused sum)))
       (is (false? (:any-land-grant-executed? sum)))
       (is (pos? (get last-ev :audit/gov-flowable-committed-usd-micros 0)))
       (is (pos? (get last-ev :audit/mitsuho-gated-refused 0)))
       (is (zero? (get last-ev :audit/housing-land-grant-executed 0)))
       ;; last-run post-ratify / flowable parity (USD micros not summed across runs)
       (is (pos? (:last-run-gov-flowable-committed-usd-micros sum)))
       (is (pos? (:last-run-gov-post-ratify-committed-usd-micros sum)))
       (is (>= (:last-run-gov-post-ratify-committed-usd-micros sum)
               (:last-run-gov-flowable-committed-usd-micros sum)))
       (is (pos? (:last-run-tenure-gov-post-ratify-committed-usd-micros sum)))
       (is (zero? (:last-run-housing-land-grant-executed sum)))
       (is (pos? (:last-run-r2-refused sum)))
       (is (zero? (:last-run-r2-executed sum)))
       (is (true? (:last-run-all-r2-not-executed sum)))
       (is (pos? (:last-run-ss-rails-gated-count sum)))
       (is (true? (:last-run-ss-all-rails-gated-refused sum)))
       (is (true? (:last-run-ss-all-r2-not-executed sum)))
       (is (false? (:last-run-ss-l0-published sum)))
       (is (= "open" (:last-run-ss-l0-disclosure-state sum)))
       (is (false? (:last-run-ss-l0-disclosure-held sum)))
       (is (true? (:last-run-ss-l0-entitlements-may-flow sum)))
       (is (= "l0-enroll-offline" (:last-run-ss-l0-path sum)))
       (is (true? (:last-run-l0-all-seven-membrane-refused sum)))
       (is (true? (:last-run-l0-all-seven-all-inkind-refused sum)))
       (is (true? (:last-run-l0-all-seven-liquidity-receive-refused sum)))
       (is (= "open" (:last-run-l0-all-seven-disclosure-state sum)))
       (is (true? (:last-run-l0-all-seven-liquidity-member-principal sum)))
       (is (false? (:last-run-l0-all-seven-liquidity-loan-executed sum)))
       (is (zero? (:last-run-l0-all-seven-liquidity-cash-usd-micros sum)))
       (is (false? (:last-run-l0-all-seven-land-grant-executed sum)))
       (is (true? (get-in sum [:last-run :l0-all-seven-membrane-refused])))
       (is (= "open" (:last-run-l0-all-seven-continuity-final-state sum)))
       (is (pos? (:last-run-l0-all-seven-continuity-held-steps sum)))
       (is (= "advanced-offline" (:last-run-l0-all-seven-ladder-advance-phase sum)))
       (is (false? (:last-run-l0-all-seven-ladder-advance-refused sum)))
       (is (true? (:last-run-l0-held-all-seven-membrane-refused sum)))
       (is (true? (:last-run-l0-held-all-seven-disclosure-held sum)))
       (is (false? (:last-run-l0-held-all-seven-entitlements-may-flow sum)))
       (is (true? (:last-run-l0-held-all-seven-ladder-advance-refused sum)))
       (is (= "refused" (:last-run-l0-held-all-seven-ladder-advance-phase sum)))
       (is (false? (:last-run-l0-held-all-seven-liquidity-loan-executed sum)))
       (is (false? (:last-run-l0-held-all-seven-land-grant-executed sum)))
       (is (= "exit-suspended" (:last-run-l0-exit-state sum)))
       (is (true? (:last-run-l0-exit-suspended sum)))
       (is (false? (:last-run-l0-exit-entitlements-may-flow sum)))
       (is (true? (:last-run-l0-exit-ladder-refused sum)))
       (is (= "open" (:last-run-l0-reaffirm-state sum)))
       (is (false? (:last-run-l0-reaffirm-exit-suspended sum)))
       (is (true? (:last-run-l0-reaffirm-entitlements-may-flow sum)))
       (is (= "advanced-offline" (:last-run-l0-reaffirm-ladder-phase sum)))
       (is (false? (:last-run-l0-reaffirm-ladder-refused sum)))
       (is (true? (:last-run-l0-falsehood-held sum)))
       (is (true? (:last-run-l0-falsehood-ladder-refused sum)))
       (is (= "open" (:last-run-l0-lift-state sum)))
       (is (= "advanced-offline" (:last-run-l0-lift-ladder-phase sum)))
       (is (false? (:last-run-l0-lift-ladder-refused sum)))
       (is (true? (:last-run-l0-care-first-both-refused sum)))
       (is (= "advanced-offline" (:last-run-l0-care-first-ladder-phase sum)))
       (is (false? (:last-run-l0-care-first-ladder-refused sum)))
       (is (true? (:last-run-l0-care-first-hikari-both-refused sum)))
       (is (= "advanced-offline" (:last-run-l0-care-first-hikari-ladder-phase sum)))
       (is (false? (:last-run-l0-care-first-hikari-ladder-refused sum)))
       (is (true? (:last-run-l0-care-housing-both-refused sum)))
       (is (false? (:last-run-l0-care-housing-land-grant-executed sum)))
       (is (= "advanced-offline" (:last-run-l0-care-housing-ladder-phase sum)))
       (is (false? (:last-run-l0-care-housing-ladder-refused sum)))
       (is (= "L4" (:last-run-ss-ladder-to sum)))
       (is (= "care" (:last-run-ss-stage-rails-first sum)))
       (is (= "housing" (:last-run-ss-stage-rails-second sum)))
       (is (pos? (:last-run-ss-stage-gated-count sum)))
       (is (true? (:last-run-ss-stage-all-gated-refused sum)))
       (is (true? (:last-run-ss-stage-r2-all-refused sum)))
       (is (false? (:last-run-ss-stage-care-gated-admissible sum)))
       (is (false? (:last-run-ss-stage-mitsuho-gated-admissible sum)))
       (is (false? (:last-run-ss-stage-hikari-gated-admissible sum)))
       (is (false? (:last-run-ss-stage-land-grant-executed sum)))
       (is (false? (:last-run-ss-mitsuho-gated-receive-admissible sum)))
       (is (false? (:last-run-ss-hikari-gated-receive-admissible sum)))
       (is (false? (:last-run-ss-care-gated-receive-admissible sum)))
       (is (true? (:last-run-ss-mitsuho-hikari-receive-both-refused sum)))
       (is (true? (:last-run-ss-care-mitsuho-hikari-receive-all-refused sum)))
       (is (false? (:last-run-ss-mitsuho-gated-produce-admissible sum)))
       (is (false? (:last-run-ss-hikari-gated-produce-admissible sum)))
       (is (true? (:last-run-ss-mitsuho-hikari-produce-both-refused sum)))
       (is (true? (:last-run-ss-mitsuho-hikari-full-chain-refused sum)))
       (is (false? (:last-run-ss-care-gated-produce-admissible sum)))
       (is (true? (:last-run-ss-care-mitsuho-hikari-produce-all-refused sum)))
       (is (true? (:last-run-ss-care-mitsuho-hikari-full-chain-refused sum)))
       (is (false? (:last-run-ss-housing-gated-receive-admissible sum)))
       (is (false? (:last-run-ss-housing-gated-produce-admissible sum)))
       (is (true? (:last-run-ss-housing-full-chain-refused sum)))
       (is (true? (:last-run-ss-care-housing-mitsuho-hikari-receive-all-refused sum)))
       (is (true? (:last-run-ss-care-housing-mitsuho-hikari-produce-all-refused sum)))
       (is (true? (:last-run-ss-care-housing-mitsuho-hikari-full-chain-refused sum)))
       (is (false? (:last-run-ss-tooling-gated-receive-admissible sum)))
       (is (false? (:last-run-ss-tooling-gated-produce-admissible sum)))
       (is (true? (:last-run-ss-tooling-full-chain-refused sum)))
       (is (false? (:last-run-ss-compute-gated-receive-admissible sum)))
       (is (false? (:last-run-ss-compute-gated-produce-admissible sum)))
       (is (true? (:last-run-ss-compute-full-chain-refused sum)))
       (is (true? (:last-run-ss-tooling-compute-full-chain-refused sum)))
       (is (true? (:last-run-ss-all-inkind-produce-rails-full-chain-refused sum)))
       (is (false? (:last-run-ss-liquidity-gated-receive-admissible sum)))
       (is (true? (:last-run-ss-liquidity-receive-full-chain-refused sum)))
       (is (true? (:last-run-ss-all-seven-rails-receive-membrane-refused sum)))
       (is (pos? (:last-run-displacement-membrane-subjects sum)))
       (is (pos? (:last-run-displacement-held-stress-subjects sum)))
       (is (pos? (:last-run-displacement-held-stress-ladder-refused sum)))
       (is (pos? (:last-run-tenure-held-stress-subjects sum)))
       (is (pos? (:last-run-tenure-held-stress-ladder-refused sum)))
       (is (pos? (:last-run-tenure-held-stress-carried sum)))
       (is (= (:last-run-tenure-held-stress-subjects sum)
              (:last-run-tenure-held-stress-carried sum)))
       (is (pos? (:last-run-gov-held-stress-subjects sum)))
       (is (pos? (:last-run-gov-held-stress-ladder-refused sum)))
       (is (pos? (:last-run-tenure-gov-held-stress-subjects sum)))
       (is (pos? (:last-run-tenure-gov-held-stress-ladder-refused sum)))
       (is (= (:last-run-displacement-held-stress-subjects sum)
              (:last-run-gov-held-stress-subjects sum)))
       (is (pos? (:last-run-displacement-all-inkind-full-chain-refused sum)))
       (is (pos? (:last-run-displacement-all-seven-receive-membrane-refused sum)))
       (is (pos? (:last-run-displacement-liquidity-recv-refused sum)))
       (is (map? (:last-run sum)))
       (is (true? (get-in sum [:last-run :ss-stage-all-gated-refused])))
       (is (false? (get-in sum [:last-run :ss-stage-care-gated-admissible])))
       (is (true? (get-in sum [:last-run :ss-care-mitsuho-hikari-receive-all-refused])))
       (is (pos? (get-in sum [:last-run :displacement-membrane-subjects] 0)))
       (is (pos? (get-in sum [:last-run :displacement-held-stress-subjects] 0)))
       (is (pos? (get-in sum [:last-run :tenure-held-stress-carried] 0)))
       (is (pos? (get-in sum [:last-run :displacement-all-seven-receive-membrane-refused] 0)))
       (is (true? (get-in sum [:last-run :ss-mitsuho-hikari-full-chain-refused])))
       (is (true? (get-in sum [:last-run :ss-care-mitsuho-hikari-full-chain-refused])))
       (is (true? (get-in sum [:last-run :ss-housing-full-chain-refused])))
       (is (true? (get-in sum [:last-run :ss-care-housing-mitsuho-hikari-full-chain-refused])))
       (is (true? (get-in sum [:last-run :ss-tooling-full-chain-refused])))
       (is (true? (get-in sum [:last-run :ss-compute-full-chain-refused])))
       (is (true? (get-in sum [:last-run :ss-all-inkind-produce-rails-full-chain-refused])))
       (is (true? (get-in sum [:last-run :ss-liquidity-receive-full-chain-refused])))
       (is (true? (get-in sum [:last-run :ss-all-seven-rails-receive-membrane-refused])))
       (is (zero? (get-in sum [:last-run :housing-land-grant-executed])))
    (is (zero? (get-in sum [:last-run :r2-executed])))
    (is (false? (get-in sum [:last-run :live])))
    (is (zero? (:total-liquidity-cash-usd-micros sum)))
    (is (= 0 (:cash-usd-micros sum)))
    (is (false? (:live sum)))))
