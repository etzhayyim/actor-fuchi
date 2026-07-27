(ns fuchi.methods.pipeline-audit-ledger
  "pipeline_audit_ledger.cljc — append-only offline audit of displacement SS pipeline runs.

  Each line is one EDN map (facts only). No personal scores. cash≡0. live=false.
  Does not execute produce/book/couple live. Portable .cljc;
  file I/O at #?(:clj | :cljs/nbb) edge (ADR-2607173000)."
  (:require [fuchi.methods.public-person :as pp]
            [clojure.string :as str]
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

#?(:cljs
   (do
     (def ^:private fs (js/require "node:fs"))
     (def ^:private path (js/require "node:path"))))

(defn- now-ms
  []
  #?(:clj (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn event-from-pipeline
  "Project a pipeline run! result into one audit event (no nested batch bodies)."
  [pipeline-result & {:keys [run-id note]}]
  (let [sc (:scorecard pipeline-result)
        ts (now-ms)
        ev {:audit/id (or run-id (str "run-" (hash (str ts
                                                          (:scorecard/committed-usd-micros-yr sc)))))
            :audit/pipeline (or (:pipeline pipeline-result) "displacement-ss-offline")
            :audit/ts-ms ts
            :audit/admissible-cohorts (or (:admissible-cohorts pipeline-result)
                                          (:scorecard/admissible-cohorts sc) 0)
            :audit/refused-cohorts (or (:scorecard/refused-cohorts sc) 0)
            :audit/enrolled-subjects (or (:scorecard/enrolled-subjects sc) 0)
            :audit/tenure-subjects (or (:tenure-subjects pipeline-result)
                                       (:scorecard/tenure-subjects sc) 0)
            :audit/tenure-stages (or (:scorecard/tenure-stage-counts sc) {})
            :audit/committed-usd-micros-yr (or (:scorecard/committed-usd-micros-yr sc) 0)
            :audit/headroom-usd-micros-yr (or (:scorecard/headroom-usd-micros-yr sc) 0)
            :audit/booked-entries (or (:scorecard/booked-entries sc) 0)
            :audit/tenure-booked-entries (or (:scorecard/tenure-booked-entries sc) 0)
            :audit/all-live-refused (boolean (or (:all-live-refused pipeline-result)
                                                 (:scorecard/all-live-refused sc)))
            :audit/gov-route-counts (or (:gov-route-counts pipeline-result)
                                        (:scorecard/gov-route-counts sc)
                                        {})
            :audit/gov-flowable-committed-usd-micros
            (or (:scorecard/gov-flowable-committed-usd-micros sc) 0)
            :audit/gov-post-ratify-committed-usd-micros
            (or (:scorecard/gov-post-ratify-committed-usd-micros sc) 0)
            :audit/tenure-gov-flowable-committed-usd-micros
            (or (:scorecard/tenure-gov-flowable-committed-usd-micros sc) 0)
            :audit/tenure-gov-post-ratify-committed-usd-micros
            (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros sc) 0)
            :audit/l4-disclosure-open (or (:scorecard/l4-disclosure-open sc) 0)
            :audit/l4-disclosure-held (or (:scorecard/l4-disclosure-held sc) 0)
            :audit/tenure-disclosure-open (or (:scorecard/tenure-disclosure-open sc) 0)
            :audit/tenure-disclosure-held (or (:scorecard/tenure-disclosure-held sc) 0)
            ;; multi-gen substrate R1→gated-live design facts (executed always 0 offline)
            :audit/mitsuho-r1-dry (or (:scorecard/mitsuho-r1-dry sc) 0)
            :audit/mitsuho-gated-refused (or (:scorecard/mitsuho-gated-refused sc) 0)
            :audit/mitsuho-produce-executed (or (:scorecard/mitsuho-produce-executed sc) 0)
            :audit/hikari-r1-dry (or (:scorecard/hikari-r1-dry sc) 0)
            :audit/hikari-gated-refused (or (:scorecard/hikari-gated-refused sc) 0)
            :audit/hikari-generate-executed (or (:scorecard/hikari-generate-executed sc) 0)
            :audit/care-r1-dry (or (:scorecard/care-r1-dry sc) 0)
            :audit/care-gated-refused (or (:scorecard/care-gated-refused sc) 0)
            :audit/care-delivery-executed (or (:scorecard/care-delivery-executed sc) 0)
            :audit/housing-r1-dry (or (:scorecard/housing-r1-dry sc) 0)
            :audit/housing-gated-refused (or (:scorecard/housing-gated-refused sc) 0)
            :audit/housing-land-grant-executed (or (:scorecard/housing-land-grant-executed sc) 0)
            :audit/housing-council-held (or (:scorecard/housing-council-held sc) 0)
            :audit/tooling-r1-dry (or (:scorecard/tooling-r1-dry sc) 0)
            :audit/tooling-gated-refused (or (:scorecard/tooling-gated-refused sc) 0)
            :audit/tooling-fulfillment-executed (or (:scorecard/tooling-fulfillment-executed sc) 0)
            :audit/compute-r1-dry (or (:scorecard/compute-r1-dry sc) 0)
            :audit/compute-gated-refused (or (:scorecard/compute-gated-refused sc) 0)
            :audit/compute-quota-executed (or (:scorecard/compute-quota-executed sc) 0)
            :audit/liquidity-r1-dry (or (:scorecard/liquidity-r1-dry sc) 0)
            :audit/liquidity-gated-refused (or (:scorecard/liquidity-gated-refused sc) 0)
            :audit/liquidity-loan-executed (or (:scorecard/liquidity-loan-executed sc) 0)
            :audit/liquidity-member-principal (or (:scorecard/liquidity-member-principal sc) 0)
            :audit/liquidity-cash-usd-micros (or (:scorecard/liquidity-cash-usd-micros sc) 0)
            :audit/r2-status-count (or (:scorecard/r2-status-count sc) 0)
            :audit/r2-refused (or (:scorecard/r2-refused sc) 0)
            :audit/r2-executed (or (:scorecard/r2-executed sc) 0)
            :audit/all-r2-not-executed
            (boolean (or (:scorecard/all-r2-not-executed sc)
                         (zero? (or (:scorecard/r2-executed sc) 0))))
            ;; SS priority path (L0 + disclosure + all-rails gated) embedded facts
            :audit/ss-rails-gated-count
            (or (get-in sc [:scorecard/ss-priority-path :rails-gated-count]) 0)
            :audit/ss-rails-gated-admissible-count
            (or (get-in sc [:scorecard/ss-priority-path :rails-gated-admissible-count]) 0)
            :audit/ss-all-rails-gated-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :all-rails-gated-refused] true))
            :audit/ss-r2-status-count
            (or (get-in sc [:scorecard/ss-priority-path :r2-status-count]) 0)
            :audit/ss-r2-executed-count
            (or (get-in sc [:scorecard/ss-priority-path :r2-executed-count]) 0)
            :audit/ss-all-r2-not-executed
            (boolean (get-in sc [:scorecard/ss-priority-path :all-r2-not-executed] true))
            :audit/ss-l0-published
            (boolean (get-in sc [:scorecard/ss-priority-path :l0-published]))
            :audit/ss-l0-disclosure-state
            (or (get-in sc [:scorecard/ss-priority-path :l0-disclosure-state]) "n/a")
            :audit/ss-l0-disclosure-held
            (boolean (get-in sc [:scorecard/ss-priority-path :l0-disclosure-held]))
            :audit/ss-l0-entitlements-may-flow
            (boolean (get-in sc [:scorecard/ss-priority-path :l0-entitlements-may-flow] true))
            :audit/ss-l0-path
            (or (get-in sc [:scorecard/ss-priority-path :l0-path]) "l0-enroll-offline")
            :audit/ss-ladder-to
            (or (get-in sc [:scorecard/ss-priority-path :ladder-to]) "n/a")
            :audit/ss-ladder-steps
            (or (get-in sc [:scorecard/ss-priority-path :ladder-steps]) 0)
            :audit/ss-ladder-rails-hint-first
            (or (get-in sc [:scorecard/ss-priority-path :ladder-rails-hint-first]) "n/a")
            :audit/ss-ladder-published
            (boolean (get-in sc [:scorecard/ss-priority-path :ladder-published]))
            :audit/ss-held-stress-ladder-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :held-stress-ladder-refused]))
            :audit/ss-stage-rails-first
            (or (get-in sc [:scorecard/ss-priority-path :stage-rails-first]) "n/a")
            :audit/ss-stage-rails-second
            (or (get-in sc [:scorecard/ss-priority-path :stage-rails-second]) "n/a")
            :audit/ss-stage-r2-all-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-r2-all-refused] true))
            :audit/ss-stage-all-gated-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-all-gated-refused] true))
            :audit/ss-stage-gated-count
            (or (get-in sc [:scorecard/ss-priority-path :stage-gated-count]) 0)
            :audit/ss-stage-care-gated-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-care-gated-admissible]))
            :audit/ss-stage-mitsuho-gated-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-mitsuho-gated-admissible]))
            :audit/ss-stage-hikari-gated-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-hikari-gated-admissible]))
            :audit/ss-stage-land-grant-executed
            (boolean (get-in sc [:scorecard/ss-priority-path :stage-land-grant-executed]))
            :audit/ss-disclosure-state
            (or (get-in sc [:scorecard/ss-priority-path :disclosure-state]) "n/a")
            :audit/ss-housing-land-grant-executed
            (boolean (get-in sc [:scorecard/ss-priority-path :housing-land-grant-executed]))
            :audit/ss-mitsuho-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :mitsuho-gated-receive-admissible]))
            :audit/ss-hikari-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :hikari-gated-receive-admissible]))
            :audit/ss-care-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path :care-gated-receive-admissible]))
            :audit/ss-mitsuho-hikari-receive-both-refused
            (boolean (get-in sc [:scorecard/ss-priority-path :mitsuho-hikari-receive-both-refused]
                             true))
            :audit/ss-care-mitsuho-hikari-receive-all-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-mitsuho-hikari-receive-all-refused]
                             true))
            :audit/ss-mitsuho-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :mitsuho-gated-produce-admissible]))
            :audit/ss-hikari-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :hikari-gated-produce-admissible]))
            :audit/ss-mitsuho-hikari-produce-both-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :mitsuho-hikari-produce-both-refused]
                             true))
            :audit/ss-mitsuho-hikari-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :mitsuho-hikari-full-chain-refused]
                             true))
            :audit/ss-care-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-gated-produce-admissible]))
            :audit/ss-care-mitsuho-hikari-produce-all-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-mitsuho-hikari-produce-all-refused]
                             true))
            :audit/ss-care-mitsuho-hikari-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-mitsuho-hikari-full-chain-refused]
                             true))
            :audit/ss-housing-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :housing-gated-receive-admissible]))
            :audit/ss-housing-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :housing-gated-produce-admissible]))
            :audit/ss-housing-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :housing-full-chain-refused]
                             true))
            :audit/ss-care-housing-mitsuho-hikari-receive-all-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-housing-mitsuho-hikari-receive-all-refused]
                             true))
            :audit/ss-care-housing-mitsuho-hikari-produce-all-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-housing-mitsuho-hikari-produce-all-refused]
                             true))
            :audit/ss-care-housing-mitsuho-hikari-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :care-housing-mitsuho-hikari-full-chain-refused]
                             true))
            :audit/ss-tooling-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :tooling-gated-receive-admissible]))
            :audit/ss-tooling-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :tooling-gated-produce-admissible]))
            :audit/ss-tooling-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :tooling-full-chain-refused]
                             true))
            :audit/ss-compute-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :compute-gated-receive-admissible]))
            :audit/ss-compute-gated-produce-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :compute-gated-produce-admissible]))
            :audit/ss-compute-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :compute-full-chain-refused]
                             true))
            :audit/ss-tooling-compute-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :tooling-compute-full-chain-refused]
                             true))
            :audit/ss-all-inkind-produce-rails-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :all-inkind-produce-rails-full-chain-refused]
                             true))
            :audit/ss-liquidity-gated-receive-admissible
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :liquidity-gated-receive-admissible]))
            :audit/ss-liquidity-receive-full-chain-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :liquidity-receive-full-chain-refused]
                             true))
            :audit/ss-all-seven-rails-receive-membrane-refused
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :all-seven-rails-receive-membrane-refused]
                             true))
            ;; Displacement→L0 subject membranes (gated DESIGN; default refuse)
            :audit/displacement-membrane-subjects
            (or (:scorecard/displacement-membrane-subjects sc) 0)
            :audit/displacement-held-stress-subjects
            (or (:scorecard/displacement-held-stress-subjects sc) 0)
            :audit/displacement-held-stress-ladder-refused
            (or (:scorecard/displacement-held-stress-ladder-refused sc) 0)
            :audit/tenure-held-stress-subjects
            (or (:scorecard/tenure-held-stress-subjects sc) 0)
            :audit/tenure-held-stress-ladder-refused
            (or (:scorecard/tenure-held-stress-ladder-refused sc) 0)
            :audit/tenure-held-stress-carried
            (or (:scorecard/tenure-held-stress-carried sc) 0)
            :audit/gov-held-stress-subjects
            (or (:scorecard/gov-held-stress-subjects sc) 0)
            :audit/gov-held-stress-ladder-refused
            (or (:scorecard/gov-held-stress-ladder-refused sc) 0)
            :audit/tenure-gov-held-stress-subjects
            (or (:scorecard/tenure-gov-held-stress-subjects sc) 0)
            :audit/tenure-gov-held-stress-ladder-refused
            (or (:scorecard/tenure-gov-held-stress-ladder-refused sc) 0)
            :audit/displacement-care-housing-full-chain-refused
            (or (:scorecard/displacement-care-housing-full-chain-refused sc) 0)
            :audit/displacement-all-inkind-full-chain-refused
            (or (:scorecard/displacement-all-inkind-full-chain-refused sc) 0)
            :audit/displacement-all-seven-receive-membrane-refused
            (or (:scorecard/displacement-all-seven-receive-membrane-refused sc) 0)
            :audit/displacement-liquidity-recv-refused
            (or (:scorecard/displacement-liquidity-recv-refused sc) 0)
            ;; L0 enroll all-seven smoke (priority 1+2+3; facts from scorecard)
            :audit/l0-all-seven-all-inkind-refused
            (boolean (get-in sc [:scorecard/l0-all-seven-enroll
                                 :all-inkind-produce-rails-full-chain-refused]
                             true))
            :audit/l0-all-seven-liquidity-receive-refused
            (boolean (get-in sc [:scorecard/l0-all-seven-enroll
                                 :liquidity-receive-full-chain-refused]
                             true))
            :audit/l0-all-seven-membrane-refused
            (boolean (get-in sc [:scorecard/l0-all-seven-enroll
                                 :all-seven-rails-receive-membrane-refused]
                             true))
            :audit/l0-all-seven-disclosure-state
            (or (get-in sc [:scorecard/l0-all-seven-enroll :disclosure-state]) "n/a")
            :audit/l0-all-seven-liquidity-member-principal
            (boolean (get-in sc [:scorecard/l0-all-seven-enroll
                                 :liquidity-member-principal]
                             true))
            :audit/l0-all-seven-liquidity-loan-executed
            (boolean (get-in sc [:scorecard/l0-all-seven-enroll
                                 :liquidity-loan-executed]))
            :audit/l0-all-seven-liquidity-cash-usd-micros
            (or (get-in sc [:scorecard/l0-all-seven-enroll
                            :liquidity-cash-usd-micros])
                0)
            :audit/l0-all-seven-land-grant-executed
            (boolean (get-in sc [:scorecard/l0-all-seven-enroll
                                 :land-grant-executed]))
            :audit/l0-all-seven-continuity-final-state
            (or (get-in sc [:scorecard/l0-all-seven-enroll :continuity-final-state])
                "n/a")
            :audit/l0-all-seven-continuity-held-steps
            (or (get-in sc [:scorecard/l0-all-seven-enroll :continuity-held-steps]) 0)
            :audit/l0-all-seven-ladder-advance-phase
            (or (get-in sc [:scorecard/l0-all-seven-enroll :ladder-advance-phase])
                "n/a")
            :audit/l0-all-seven-ladder-advance-refused
            (boolean (get-in sc [:scorecard/l0-all-seven-enroll
                                 :ladder-advance-refused]))
            ;; L0 held (stale disclosure) all-seven stress
            :audit/l0-held-all-seven-membrane-refused
            (boolean (get-in sc [:scorecard/l0-held-all-seven-enroll
                                 :all-seven-rails-receive-membrane-refused]
                             true))
            :audit/l0-held-all-seven-disclosure-state
            (or (get-in sc [:scorecard/l0-held-all-seven-enroll :disclosure-state])
                "n/a")
            :audit/l0-held-all-seven-disclosure-held
            (boolean (get-in sc [:scorecard/l0-held-all-seven-enroll
                                 :disclosure-held]
                             true))
            :audit/l0-held-all-seven-entitlements-may-flow
            (boolean (get-in sc [:scorecard/l0-held-all-seven-enroll
                                 :entitlements-may-flow]))
            :audit/l0-held-all-seven-ladder-advance-phase
            (or (get-in sc [:scorecard/l0-held-all-seven-enroll
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-held-all-seven-ladder-advance-refused
            (boolean (get-in sc [:scorecard/l0-held-all-seven-enroll
                                 :ladder-advance-refused]
                             true))
            :audit/l0-held-all-seven-liquidity-loan-executed
            (boolean (get-in sc [:scorecard/l0-held-all-seven-enroll
                                 :liquidity-loan-executed]))
            :audit/l0-held-all-seven-land-grant-executed
            (boolean (get-in sc [:scorecard/l0-held-all-seven-enroll
                                 :land-grant-executed]))
            ;; L0 exit→re-affirm disclosure SM stress
            :audit/l0-exit-state
            (or (get-in sc [:scorecard/l0-exit-reaffirm :exit-state]) "n/a")
            :audit/l0-exit-suspended
            (boolean (get-in sc [:scorecard/l0-exit-reaffirm :exit-suspended?]))
            :audit/l0-exit-entitlements-may-flow
            (boolean (get-in sc [:scorecard/l0-exit-reaffirm
                                 :exit-entitlements-may-flow]))
            :audit/l0-exit-ladder-phase
            (or (get-in sc [:scorecard/l0-exit-reaffirm :exit-ladder-phase]) "n/a")
            :audit/l0-exit-ladder-refused
            (boolean (get-in sc [:scorecard/l0-exit-reaffirm :exit-ladder-refused]
                             true))
            :audit/l0-reaffirm-state
            (or (get-in sc [:scorecard/l0-exit-reaffirm :reaffirm-state]) "n/a")
            :audit/l0-reaffirm-exit-suspended
            (boolean (get-in sc [:scorecard/l0-exit-reaffirm
                                 :reaffirm-exit-suspended?]))
            :audit/l0-reaffirm-entitlements-may-flow
            (boolean (get-in sc [:scorecard/l0-exit-reaffirm
                                 :reaffirm-entitlements-may-flow]
                             true))
            :audit/l0-reaffirm-ladder-phase
            (or (get-in sc [:scorecard/l0-exit-reaffirm :reaffirm-ladder-phase])
                "n/a")
            :audit/l0-reaffirm-ladder-refused
            (boolean (get-in sc [:scorecard/l0-exit-reaffirm
                                 :reaffirm-ladder-refused]))
            ;; L0 falsehood→lift-hold stress
            :audit/l0-falsehood-held
            (boolean (get-in sc [:scorecard/l0-falsehood-lift :falsehood-held?]
                             true))
            :audit/l0-falsehood-entitlements-may-flow
            (boolean (get-in sc [:scorecard/l0-falsehood-lift
                                 :falsehood-entitlements-may-flow]))
            :audit/l0-falsehood-ladder-phase
            (or (get-in sc [:scorecard/l0-falsehood-lift :falsehood-ladder-phase])
                "n/a")
            :audit/l0-falsehood-ladder-refused
            (boolean (get-in sc [:scorecard/l0-falsehood-lift
                                 :falsehood-ladder-refused]
                             true))
            :audit/l0-lift-state
            (or (get-in sc [:scorecard/l0-falsehood-lift :lift-state]) "n/a")
            :audit/l0-lift-entitlements-may-flow
            (boolean (get-in sc [:scorecard/l0-falsehood-lift
                                 :lift-entitlements-may-flow]
                             true))
            :audit/l0-lift-ladder-phase
            (or (get-in sc [:scorecard/l0-falsehood-lift :lift-ladder-phase])
                "n/a")
            :audit/l0-lift-ladder-refused
            (boolean (get-in sc [:scorecard/l0-falsehood-lift
                                 :lift-ladder-refused]))
            ;; L0 care-first + mitsuho priority path
            :audit/l0-care-first-disclosure-state
            (or (get-in sc [:scorecard/l0-care-first-mitsuho :disclosure-state])
                "n/a")
            :audit/l0-care-first-care-full-chain-refused
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho
                                 :care-full-chain-refused]
                             true))
            :audit/l0-care-first-mitsuho-full-chain-refused
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho
                                 :mitsuho-full-chain-refused]
                             true))
            :audit/l0-care-first-both-refused
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho
                                 :care-mitsuho-both-refused]
                             true))
            :audit/l0-care-first-ladder-phase
            (or (get-in sc [:scorecard/l0-care-first-mitsuho
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-care-first-ladder-refused
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho
                                 :ladder-advance-refused]))
            :audit/l0-care-first-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-care-first-mitsuho
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-care-first-mitsuho
                                     :held-stress :ladder-advance-refused])
                         true))
            ;; priority (3) DESIGN facts on care-first mitsuho path
            :audit/l0-care-first-mitsuho-live-produce
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho
                                 :mitsuho-live-produce]))
            :audit/l0-care-first-mitsuho-produce-executed
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho
                                 :mitsuho-produce-executed]))
            :audit/l0-care-first-care-delivery-executed
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho
                                 :care-delivery-executed]))
            :audit/l0-care-first-api-path
            (or (get-in sc [:scorecard/l0-care-first-mitsuho
                            :care-first-api-path])
                "care-first-mitsuho-path")
            :audit/l0-care-first-hikari-both-refused
            (boolean (get-in sc [:scorecard/l0-care-first-hikari
                                 :care-hikari-both-refused]
                             true))
            :audit/l0-care-first-hikari-ladder-phase
            (or (get-in sc [:scorecard/l0-care-first-hikari
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-care-first-hikari-ladder-refused
            (boolean (get-in sc [:scorecard/l0-care-first-hikari
                                 :ladder-advance-refused]))
            :audit/l0-care-first-hikari-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-care-first-hikari
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-care-first-hikari
                                     :held-stress :ladder-advance-refused])
                         true))
            :audit/l0-care-first-hikari-live-produce
            (boolean (get-in sc [:scorecard/l0-care-first-hikari
                                 :hikari-live-produce]))
            :audit/l0-care-first-hikari-generate-executed
            (boolean (get-in sc [:scorecard/l0-care-first-hikari
                                 :hikari-generate-executed]))
            :audit/l0-care-first-hikari-api-path
            (or (get-in sc [:scorecard/l0-care-first-hikari
                            :care-first-api-path])
                "care-first-hikari-path")
            :audit/l0-care-first-mitsuho-hikari-all-refused
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho-hikari
                                 :care-mitsuho-hikari-all-refused]
                             true))
            :audit/l0-care-first-mitsuho-hikari-mitsuho-hikari-both-refused
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho-hikari
                                 :mitsuho-hikari-both-refused]
                             true))
            :audit/l0-care-first-mitsuho-hikari-ladder-phase
            (or (get-in sc [:scorecard/l0-care-first-mitsuho-hikari
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-care-first-mitsuho-hikari-ladder-refused
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho-hikari
                                 :ladder-advance-refused]))
            :audit/l0-care-first-mitsuho-hikari-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-care-first-mitsuho-hikari
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-care-first-mitsuho-hikari
                                     :held-stress :ladder-advance-refused])
                         true))
            :audit/l0-care-first-mitsuho-hikari-mitsuho-live-produce
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho-hikari
                                 :mitsuho-live-produce]))
            :audit/l0-care-first-mitsuho-hikari-hikari-live-produce
            (boolean (get-in sc [:scorecard/l0-care-first-mitsuho-hikari
                                 :hikari-live-produce]))
            :audit/l0-care-housing-both-refused
            (boolean (get-in sc [:scorecard/l0-care-housing-first
                                 :care-housing-both-refused]
                             true))
            :audit/l0-care-housing-land-grant-executed
            (boolean (get-in sc [:scorecard/l0-care-housing-first
                                 :land-grant-executed]))
            :audit/l0-care-housing-ladder-phase
            (or (get-in sc [:scorecard/l0-care-housing-first
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-care-housing-ladder-refused
            (boolean (get-in sc [:scorecard/l0-care-housing-first
                                 :ladder-advance-refused]))
            :audit/l0-care-housing-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-care-housing-first
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-care-housing-first
                                     :held-stress :ladder-advance-refused])
                         true))
            :audit/l0-multi-gen-substrate-all-refused
            (boolean (get-in sc [:scorecard/l0-multi-gen-substrate
                                 :care-housing-mitsuho-hikari-all-refused]
                             true))
            :audit/l0-multi-gen-substrate-care-housing-both-refused
            (boolean (get-in sc [:scorecard/l0-multi-gen-substrate
                                 :care-housing-both-refused]
                             true))
            :audit/l0-multi-gen-substrate-mitsuho-hikari-both-refused
            (boolean (get-in sc [:scorecard/l0-multi-gen-substrate
                                 :mitsuho-hikari-both-refused]
                             true))
            :audit/l0-multi-gen-substrate-land-grant-executed
            (boolean (get-in sc [:scorecard/l0-multi-gen-substrate
                                 :land-grant-executed]))
            :audit/l0-multi-gen-substrate-ladder-phase
            (or (get-in sc [:scorecard/l0-multi-gen-substrate
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-multi-gen-substrate-ladder-refused
            (boolean (get-in sc [:scorecard/l0-multi-gen-substrate
                                 :ladder-advance-refused]))
            :audit/l0-multi-gen-substrate-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-multi-gen-substrate
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-multi-gen-substrate
                                     :held-stress :ladder-advance-refused])
                         true))
            :audit/l0-full-inkind-all-refused
            (boolean (get-in sc [:scorecard/l0-full-inkind-substrate
                                 :all-inkind-produce-rails-full-chain-refused]
                             true))
            :audit/l0-full-inkind-tooling-compute-both-refused
            (boolean (get-in sc [:scorecard/l0-full-inkind-substrate
                                 :tooling-compute-both-refused]
                             true))
            :audit/l0-full-inkind-land-grant-executed
            (boolean (get-in sc [:scorecard/l0-full-inkind-substrate
                                 :land-grant-executed]))
            :audit/l0-full-inkind-fulfillment-executed
            (boolean (get-in sc [:scorecard/l0-full-inkind-substrate
                                 :fulfillment-executed]))
            :audit/l0-full-inkind-quota-executed
            (boolean (get-in sc [:scorecard/l0-full-inkind-substrate
                                 :quota-executed]))
            :audit/l0-full-inkind-ladder-phase
            (or (get-in sc [:scorecard/l0-full-inkind-substrate
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-full-inkind-ladder-refused
            (boolean (get-in sc [:scorecard/l0-full-inkind-substrate
                                 :ladder-advance-refused]))
            :audit/l0-full-inkind-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-full-inkind-substrate
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-full-inkind-substrate
                                     :held-stress :ladder-advance-refused])
                         true))
            :audit/l0-vocation-recovery-both-refused
            (boolean (get-in sc [:scorecard/l0-vocation-recovery
                                 :tooling-compute-both-refused]
                             true))
            :audit/l0-vocation-recovery-fulfillment-executed
            (boolean (get-in sc [:scorecard/l0-vocation-recovery
                                 :fulfillment-executed]))
            :audit/l0-vocation-recovery-quota-executed
            (boolean (get-in sc [:scorecard/l0-vocation-recovery
                                 :quota-executed]))
            :audit/l0-vocation-recovery-ladder-phase
            (or (get-in sc [:scorecard/l0-vocation-recovery
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-vocation-recovery-ladder-refused
            (boolean (get-in sc [:scorecard/l0-vocation-recovery
                                 :ladder-advance-refused]))
            :audit/l0-vocation-recovery-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-vocation-recovery
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-vocation-recovery
                                     :held-stress :ladder-advance-refused])
                         true))
            :audit/l0-liquidity-residual-receive-refused
            (boolean (get-in sc [:scorecard/l0-liquidity-residual
                                 :liquidity-receive-full-chain-refused]
                             true))
            :audit/l0-liquidity-residual-member-principal
            (boolean (get-in sc [:scorecard/l0-liquidity-residual
                                 :liquidity-member-principal]
                             true))
            :audit/l0-liquidity-residual-loan-executed
            (boolean (get-in sc [:scorecard/l0-liquidity-residual
                                 :liquidity-loan-executed]))
            :audit/l0-liquidity-residual-cash-usd-micros
            (or (get-in sc [:scorecard/l0-liquidity-residual
                            :liquidity-cash-usd-micros])
                0)
            :audit/l0-liquidity-residual-ladder-phase
            (or (get-in sc [:scorecard/l0-liquidity-residual
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-liquidity-residual-ladder-refused
            (boolean (get-in sc [:scorecard/l0-liquidity-residual
                                 :ladder-advance-refused]))
            :audit/l0-liquidity-residual-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-liquidity-residual
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-liquidity-residual
                                     :held-stress :ladder-advance-refused])
                         true))
            :audit/l0-all-seven-substrate-all-inkind-refused
            (boolean (get-in sc [:scorecard/l0-all-seven-substrate
                                 :all-inkind-produce-rails-full-chain-refused]
                             true))
            :audit/l0-all-seven-substrate-membrane-refused
            (boolean (get-in sc [:scorecard/l0-all-seven-substrate
                                 :all-seven-rails-receive-membrane-refused]
                             true))
            :audit/l0-all-seven-substrate-loan-executed
            (boolean (get-in sc [:scorecard/l0-all-seven-substrate
                                 :liquidity-loan-executed]))
            :audit/l0-all-seven-substrate-land-grant-executed
            (boolean (get-in sc [:scorecard/l0-all-seven-substrate
                                 :land-grant-executed]))
            :audit/l0-all-seven-substrate-ladder-phase
            (or (get-in sc [:scorecard/l0-all-seven-substrate
                            :ladder-advance-phase])
                "n/a")
            :audit/l0-all-seven-substrate-ladder-refused
            (boolean (get-in sc [:scorecard/l0-all-seven-substrate
                                 :ladder-advance-refused]))
            :audit/l0-all-seven-substrate-held-stress-ladder-refused
            (boolean (or (get-in sc [:scorecard/l0-all-seven-substrate
                                     :held-stress-ladder-refused])
                         (get-in sc [:scorecard/l0-all-seven-substrate
                                     :held-stress :ladder-advance-refused])
                         true))
            :audit/l0-priority-path-count
            (or (get-in sc [:scorecard/l0-priority-path-catalog :path-count]) 0)
            :audit/l0-priority-held-stress-embed-count
            (or (get-in sc [:scorecard/l0-priority-path-catalog
                            :held-stress-embed-count])
                0)
            :audit/l0-priority-held-stress-embed-all
            (boolean (or (get-in sc [:scorecard/l0-priority-path-catalog
                                     :invariants :held-stress-embed-all])
                         (let [pc (get-in sc [:scorecard/l0-priority-path-catalog
                                              :path-count])
                               hc (get-in sc [:scorecard/l0-priority-path-catalog
                                              :held-stress-embed-count])]
                           (and (number? pc) (number? hc) (pos? pc) (= pc hc)))
                         true))
            ;; Priority stack SSoT (1)L0 (2)disclosure (3)mitsuho+hikari — facts only
            :audit/priority-stack-ok
            (boolean (get-in sc [:scorecard/priority-stack-offline :ok]))
            :audit/priority-stack-l0-stage
            (or (get-in sc [:scorecard/priority-stack-offline :l0-stage]) "n/a")
            :audit/priority-stack-l0-published
            (boolean (get-in sc [:scorecard/priority-stack-offline :l0-published]))
            :audit/priority-stack-disclosure-open-may-flow
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :disclosure-open-may-flow]))
            :audit/priority-stack-disclosure-stale-held
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :disclosure-stale-held]))
            :audit/priority-stack-disclosure-tick-final
            (or (get-in sc [:scorecard/priority-stack-offline
                            :disclosure-tick-final])
                "n/a")
            :audit/priority-stack-care-housing-api
            (or (get-in sc [:scorecard/priority-stack-offline
                            :care-housing-api-path])
                "n/a")
            :audit/priority-stack-care-housing-both-refused
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :care-housing-both-refused]))
            :audit/priority-stack-care-housing-land-grant
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :care-housing-land-grant-executed]))
            :audit/priority-stack-care-housing-held-stress
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :care-housing-held-stress-ladder-refused]))
            :audit/priority-stack-all-seven-api
            (or (get-in sc [:scorecard/priority-stack-offline :all-seven-api-path])
                "n/a")
            :audit/priority-stack-all-seven-membrane-refused
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :all-seven-membrane-refused]))
            :audit/priority-stack-all-seven-loan-executed
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :all-seven-loan-executed]))
            :audit/priority-stack-all-seven-land-grant
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :all-seven-land-grant-executed]))
            :audit/priority-stack-all-seven-held-stress
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :all-seven-held-stress-ladder-refused]))
            :audit/priority-stack-mitsuho-r1-phase
            (or (get-in sc [:scorecard/priority-stack-offline :mitsuho-r1-phase])
                "n/a")
            :audit/priority-stack-mitsuho-gated-phase
            (or (get-in sc [:scorecard/priority-stack-offline :mitsuho-gated-phase])
                "n/a")
            :audit/priority-stack-mitsuho-produce-executed
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :mitsuho-produce-executed]))
            :audit/priority-stack-mitsuho-care-first-api
            (or (get-in sc [:scorecard/priority-stack-offline
                            :mitsuho-care-first-api-path])
                "n/a")
            :audit/priority-stack-mitsuho-held-stress-ladder-refused
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :mitsuho-held-stress-ladder-refused]))
            :audit/priority-stack-hikari-r1-phase
            (or (get-in sc [:scorecard/priority-stack-offline :hikari-r1-phase])
                "n/a")
            :audit/priority-stack-hikari-gated-phase
            (or (get-in sc [:scorecard/priority-stack-offline :hikari-gated-phase])
                "n/a")
            :audit/priority-stack-hikari-produce-executed
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :hikari-produce-executed]))
            :audit/priority-stack-hikari-care-first-api
            (or (get-in sc [:scorecard/priority-stack-offline
                            :hikari-care-first-api-path])
                "n/a")
            :audit/priority-stack-hikari-held-stress-ladder-refused
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :hikari-held-stress-ladder-refused]))
            :audit/priority-stack-l0-paths-count
            (or (get-in sc [:scorecard/priority-stack-offline :l0-paths-count]) 0)
            :audit/priority-stack-l0-paths-all-held-stress
            (boolean (get-in sc [:scorecard/priority-stack-offline
                                 :l0-paths-all-held-stress]))
            ;; All-seven single-rail DESIGN catalog (priority 3 discovery)
            :audit/rail-design-rail-count
            (or (get-in sc [:scorecard/rail-design-catalog :rail-count]) 0)
            :audit/rail-design-ok-count
            (or (get-in sc [:scorecard/rail-design-catalog :ok-count]) 0)
            :audit/rail-design-live-produce-never
            (boolean (get-in sc [:scorecard/rail-design-catalog :live-produce-never] true))
            :audit/rail-design-all-cash-zero
            (boolean (get-in sc [:scorecard/rail-design-catalog :all-cash-zero] true))
            :audit/rail-design-all-live-false
            (boolean (get-in sc [:scorecard/rail-design-catalog :all-live-false] true))
            :audit/rail-design-all-seven
            (boolean (or (get-in sc [:scorecard/rail-design-catalog
                                     :invariants :all-seven-design])
                         (= 7 (get-in sc [:scorecard/rail-design-catalog :rail-count]))
                         true))
            ;; SS offline path DESIGN (priority 3) from scorecard/ss-priority-path
            ;; multi-gen care/housing first, then food/energy (mitsuho/hikari)
            :audit/ss-care-live-produce
            (boolean (get-in sc [:scorecard/ss-priority-path :care-live-produce]))
            :audit/ss-housing-live-produce
            (boolean (get-in sc [:scorecard/ss-priority-path :housing-live-produce]))
            :audit/ss-care-care-first-api-path
            (or (get-in sc [:scorecard/ss-priority-path :care-care-first-api-path])
                "care-housing-first-path")
            :audit/ss-housing-care-first-api-path
            (or (get-in sc [:scorecard/ss-priority-path :housing-care-first-api-path])
                "care-housing-first-path")
            :audit/ss-care-design-rail-kind
            (or (get-in sc [:scorecard/ss-priority-path :care-design-rail-kind])
                "care-iyashi")
            :audit/ss-housing-design-rail-kind
            (or (get-in sc [:scorecard/ss-priority-path :housing-design-rail-kind])
                "housing-commons")
            :audit/ss-mitsuho-live-produce
            (boolean (get-in sc [:scorecard/ss-priority-path :mitsuho-live-produce]))
            :audit/ss-hikari-live-produce
            (boolean (get-in sc [:scorecard/ss-priority-path :hikari-live-produce]))
            :audit/ss-mitsuho-care-first-api-path
            (or (get-in sc [:scorecard/ss-priority-path :mitsuho-care-first-api-path])
                "care-first-mitsuho-path")
            :audit/ss-hikari-care-first-api-path
            (or (get-in sc [:scorecard/ss-priority-path :hikari-care-first-api-path])
                "care-first-hikari-path")
            :audit/ss-tooling-live-produce
            (boolean (get-in sc [:scorecard/ss-priority-path :tooling-live-produce]))
            :audit/ss-compute-live-produce
            (boolean (get-in sc [:scorecard/ss-priority-path :compute-live-produce]))
            :audit/ss-liquidity-live-produce
            (boolean (get-in sc [:scorecard/ss-priority-path :liquidity-live-produce]))
            :audit/ss-tooling-care-first-api-path
            (or (get-in sc [:scorecard/ss-priority-path :tooling-care-first-api-path])
                "vocation-recovery-path")
            :audit/ss-compute-care-first-api-path
            (or (get-in sc [:scorecard/ss-priority-path :compute-care-first-api-path])
                "vocation-recovery-path")
            :audit/ss-liquidity-care-first-api-path
            (or (get-in sc [:scorecard/ss-priority-path :liquidity-care-first-api-path])
                "liquidity-residual-path")
            :audit/ss-all-seven-design-embed-count
            (or (get-in sc [:scorecard/ss-priority-path :all-seven-design-embed-count]) 7)
            :audit/ss-all-seven-design-live-produce-never
            (boolean (get-in sc [:scorecard/ss-priority-path
                                 :all-seven-design-live-produce-never] true))
            :audit/all-held-stress-gov-flowable
            (or (get-in sc [:scorecard/all-held-stress :gov-flowable]) 0)
            :audit/all-held-stress-held-subjects
            (or (get-in sc [:scorecard/all-held-stress :held-subjects]) 0)
            :audit/cash-usd-micros 0
            :audit/cash-to-workers-usd-micros 0
            :audit/live false
            :audit/score-surface []
            :audit/priority-stack PRIORITY-STACK
            :audit/note (or note "offline pipeline audit — no live side-effects")}]
    (pp/assert-no-public-scores! ev)
    ev))

(defn- actor-dir
  []
  #?(:clj
     (or (System/getenv "FUCHI_ACTOR_DIR")
         (try (-> *file* java.io.File. .getParentFile .getParentFile .getCanonicalPath)
              (catch Exception _ ".")))
     :cljs
     (or (.-FUCHI_ACTOR_DIR (.-env js/process))
         (try (.resolve path (.dirname path *file*) "..")
              (catch :default _ ".")))))

(defn ledger-path
  "Path to out/pipeline-audit-ledger.ednl (string under cljs; File under clj)."
  ([]
   (ledger-path (actor-dir)))
  ([actor-dir]
   #?(:clj (io/file actor-dir "out" "pipeline-audit-ledger.ednl")
      :cljs (.join path actor-dir "out" "pipeline-audit-ledger.ednl"))))

(defn- ensure-parent!
  [f]
  #?(:clj (.mkdirs (.getParentFile (io/file f)))
     :cljs (let [dir (.dirname path (str f))]
             (when-not (.existsSync fs dir)
               (.mkdirSync fs dir #js {:recursive true})))))

(defn- file-exists?
  [f]
  #?(:clj (.exists (io/file f))
     :cljs (.existsSync fs (str f))))

(defn- read-text
  [f]
  #?(:clj (slurp (io/file f))
     :cljs (.readFileSync fs (str f) "utf8")))

(defn- append-text!
  [f line]
  #?(:clj (spit (io/file f) line :append true)
     :cljs (.appendFileSync fs (str f) line "utf8")))

(defn append!
  "Append one event line to out/pipeline-audit-ledger.ednl. Returns path + event."
  [event]
  (let [f (ledger-path)
        _ (ensure-parent! f)
        line (str (pr-str event) "\n")]
    (append-text! f line)
    {:path (str f)
     :event event
     :live false
     :cash-usd-micros 0
     :score-surface []
     :priority-stack PRIORITY-STACK
     :deployed false}))

(defn append-from-pipeline!
  "Append audit line from a pipeline run result."
  [pipeline-result & opts]
  (append! (apply event-from-pipeline pipeline-result opts)))

(defn read-all
  "Read all audit events (vector). Empty if missing."
  []
  (let [f (ledger-path)]
    (if-not (file-exists? f)
      []
      (->> (str/split-lines (read-text f))
           (remove str/blank?)
           (mapv read-string)))))

(defn- last-run-snapshot
  "Facts-only projection of the most recent audit event (no scores)."
  [ev]
  (when ev
    (let [out {:run-id (:audit/id ev)
               :enrolled-subjects (or (:audit/enrolled-subjects ev) 0)
               :tenure-subjects (or (:audit/tenure-subjects ev) 0)
               :gov-flowable-committed-usd-micros
               (or (:audit/gov-flowable-committed-usd-micros ev) 0)
               :gov-post-ratify-committed-usd-micros
               (or (:audit/gov-post-ratify-committed-usd-micros ev) 0)
               :tenure-gov-flowable-committed-usd-micros
               (or (:audit/tenure-gov-flowable-committed-usd-micros ev) 0)
               :tenure-gov-post-ratify-committed-usd-micros
               (or (:audit/tenure-gov-post-ratify-committed-usd-micros ev) 0)
               :housing-land-grant-executed
               (or (:audit/housing-land-grant-executed ev) 0)
               :housing-council-held (or (:audit/housing-council-held ev) 0)
               :liquidity-member-principal
               (or (:audit/liquidity-member-principal ev) 0)
               :liquidity-cash-usd-micros
               (or (:audit/liquidity-cash-usd-micros ev) 0)
               :r2-status-count (or (:audit/r2-status-count ev) 0)
               :r2-refused (or (:audit/r2-refused ev) 0)
               :r2-executed (or (:audit/r2-executed ev) 0)
               :all-r2-not-executed
               (boolean (or (:audit/all-r2-not-executed ev)
                            (zero? (or (:audit/r2-executed ev) 0))))
               :ss-rails-gated-count (or (:audit/ss-rails-gated-count ev) 0)
               :ss-all-rails-gated-refused
               (boolean (:audit/ss-all-rails-gated-refused ev true))
               :ss-r2-status-count (or (:audit/ss-r2-status-count ev) 0)
               :ss-r2-executed-count (or (:audit/ss-r2-executed-count ev) 0)
               :ss-all-r2-not-executed
               (boolean (:audit/ss-all-r2-not-executed ev true))
               :ss-l0-published (boolean (:audit/ss-l0-published ev))
               :ss-l0-disclosure-state
               (or (:audit/ss-l0-disclosure-state ev) "n/a")
               :ss-l0-disclosure-held
               (boolean (:audit/ss-l0-disclosure-held ev))
               :ss-l0-entitlements-may-flow
               (boolean (:audit/ss-l0-entitlements-may-flow ev true))
               :ss-l0-path (or (:audit/ss-l0-path ev) "l0-enroll-offline")
               :ss-ladder-to (or (:audit/ss-ladder-to ev) "n/a")
               :ss-ladder-steps (or (:audit/ss-ladder-steps ev) 0)
               :ss-ladder-rails-hint-first
               (or (:audit/ss-ladder-rails-hint-first ev) "n/a")
               :ss-held-stress-ladder-refused
               (boolean (:audit/ss-held-stress-ladder-refused ev))
               :ss-stage-rails-first
               (or (:audit/ss-stage-rails-first ev) "n/a")
               :ss-stage-rails-second
               (or (:audit/ss-stage-rails-second ev) "n/a")
               :ss-stage-gated-count
               (or (:audit/ss-stage-gated-count ev) 0)
               :ss-stage-all-gated-refused
               (boolean (:audit/ss-stage-all-gated-refused ev true))
               :ss-stage-r2-all-refused
               (boolean (:audit/ss-stage-r2-all-refused ev true))
               :ss-stage-care-gated-admissible
               (boolean (:audit/ss-stage-care-gated-admissible ev))
               :ss-stage-mitsuho-gated-admissible
               (boolean (:audit/ss-stage-mitsuho-gated-admissible ev))
               :ss-stage-hikari-gated-admissible
               (boolean (:audit/ss-stage-hikari-gated-admissible ev))
               :ss-stage-land-grant-executed
               (boolean (:audit/ss-stage-land-grant-executed ev))
               :ss-mitsuho-gated-receive-admissible
               (boolean (:audit/ss-mitsuho-gated-receive-admissible ev))
               :ss-hikari-gated-receive-admissible
               (boolean (:audit/ss-hikari-gated-receive-admissible ev))
               :ss-care-gated-receive-admissible
               (boolean (:audit/ss-care-gated-receive-admissible ev))
               :ss-mitsuho-hikari-receive-both-refused
               (boolean (:audit/ss-mitsuho-hikari-receive-both-refused ev true))
               :ss-care-mitsuho-hikari-receive-all-refused
               (boolean (:audit/ss-care-mitsuho-hikari-receive-all-refused ev true))
               :ss-mitsuho-gated-produce-admissible
               (boolean (:audit/ss-mitsuho-gated-produce-admissible ev))
               :ss-hikari-gated-produce-admissible
               (boolean (:audit/ss-hikari-gated-produce-admissible ev))
               :ss-mitsuho-hikari-produce-both-refused
               (boolean (:audit/ss-mitsuho-hikari-produce-both-refused ev true))
               :ss-mitsuho-hikari-full-chain-refused
               (boolean (:audit/ss-mitsuho-hikari-full-chain-refused ev true))
               :ss-care-gated-produce-admissible
               (boolean (:audit/ss-care-gated-produce-admissible ev))
               :ss-care-mitsuho-hikari-produce-all-refused
               (boolean (:audit/ss-care-mitsuho-hikari-produce-all-refused ev true))
               :ss-care-mitsuho-hikari-full-chain-refused
               (boolean (:audit/ss-care-mitsuho-hikari-full-chain-refused ev true))
               :ss-housing-gated-receive-admissible
               (boolean (:audit/ss-housing-gated-receive-admissible ev))
               :ss-housing-gated-produce-admissible
               (boolean (:audit/ss-housing-gated-produce-admissible ev))
               :ss-housing-full-chain-refused
               (boolean (:audit/ss-housing-full-chain-refused ev true))
               :ss-care-housing-mitsuho-hikari-receive-all-refused
               (boolean (:audit/ss-care-housing-mitsuho-hikari-receive-all-refused ev true))
               :ss-care-housing-mitsuho-hikari-produce-all-refused
               (boolean (:audit/ss-care-housing-mitsuho-hikari-produce-all-refused ev true))
               :ss-care-housing-mitsuho-hikari-full-chain-refused
               (boolean (:audit/ss-care-housing-mitsuho-hikari-full-chain-refused ev true))
               :ss-tooling-gated-receive-admissible
               (boolean (:audit/ss-tooling-gated-receive-admissible ev))
               :ss-tooling-gated-produce-admissible
               (boolean (:audit/ss-tooling-gated-produce-admissible ev))
               :ss-tooling-full-chain-refused
               (boolean (:audit/ss-tooling-full-chain-refused ev true))
               :ss-compute-gated-receive-admissible
               (boolean (:audit/ss-compute-gated-receive-admissible ev))
               :ss-compute-gated-produce-admissible
               (boolean (:audit/ss-compute-gated-produce-admissible ev))
               :ss-compute-full-chain-refused
               (boolean (:audit/ss-compute-full-chain-refused ev true))
               :ss-tooling-compute-full-chain-refused
               (boolean (:audit/ss-tooling-compute-full-chain-refused ev true))
               :ss-all-inkind-produce-rails-full-chain-refused
               (boolean (:audit/ss-all-inkind-produce-rails-full-chain-refused ev true))
               :ss-liquidity-gated-receive-admissible
               (boolean (:audit/ss-liquidity-gated-receive-admissible ev))
               :ss-liquidity-receive-full-chain-refused
               (boolean (:audit/ss-liquidity-receive-full-chain-refused ev true))
               :ss-all-seven-rails-receive-membrane-refused
               (boolean (:audit/ss-all-seven-rails-receive-membrane-refused ev true))
               :displacement-membrane-subjects
               (or (:audit/displacement-membrane-subjects ev) 0)
               :displacement-held-stress-subjects
               (or (:audit/displacement-held-stress-subjects ev) 0)
               :displacement-held-stress-ladder-refused
               (or (:audit/displacement-held-stress-ladder-refused ev) 0)
               :tenure-held-stress-subjects
               (or (:audit/tenure-held-stress-subjects ev) 0)
               :tenure-held-stress-ladder-refused
               (or (:audit/tenure-held-stress-ladder-refused ev) 0)
               :tenure-held-stress-carried
               (or (:audit/tenure-held-stress-carried ev) 0)
               :gov-held-stress-subjects
               (or (:audit/gov-held-stress-subjects ev) 0)
               :gov-held-stress-ladder-refused
               (or (:audit/gov-held-stress-ladder-refused ev) 0)
               :tenure-gov-held-stress-subjects
               (or (:audit/tenure-gov-held-stress-subjects ev) 0)
               :tenure-gov-held-stress-ladder-refused
               (or (:audit/tenure-gov-held-stress-ladder-refused ev) 0)
               :displacement-care-housing-full-chain-refused
               (or (:audit/displacement-care-housing-full-chain-refused ev) 0)
               :displacement-all-inkind-full-chain-refused
               (or (:audit/displacement-all-inkind-full-chain-refused ev) 0)
               :displacement-all-seven-receive-membrane-refused
               (or (:audit/displacement-all-seven-receive-membrane-refused ev) 0)
               :displacement-liquidity-recv-refused
               (or (:audit/displacement-liquidity-recv-refused ev) 0)
               :l0-all-seven-all-inkind-refused
               (boolean (:audit/l0-all-seven-all-inkind-refused ev true))
               :l0-all-seven-liquidity-receive-refused
               (boolean (:audit/l0-all-seven-liquidity-receive-refused ev true))
               :l0-all-seven-membrane-refused
               (boolean (:audit/l0-all-seven-membrane-refused ev true))
               :l0-all-seven-disclosure-state
               (or (:audit/l0-all-seven-disclosure-state ev) "n/a")
               :l0-all-seven-liquidity-member-principal
               (boolean (:audit/l0-all-seven-liquidity-member-principal ev true))
               :l0-all-seven-liquidity-loan-executed
               (boolean (:audit/l0-all-seven-liquidity-loan-executed ev))
               :l0-all-seven-liquidity-cash-usd-micros
               (or (:audit/l0-all-seven-liquidity-cash-usd-micros ev) 0)
               :l0-all-seven-land-grant-executed
               (boolean (:audit/l0-all-seven-land-grant-executed ev))
               :l0-all-seven-continuity-final-state
               (or (:audit/l0-all-seven-continuity-final-state ev) "n/a")
               :l0-all-seven-continuity-held-steps
               (or (:audit/l0-all-seven-continuity-held-steps ev) 0)
               :l0-all-seven-ladder-advance-phase
               (or (:audit/l0-all-seven-ladder-advance-phase ev) "n/a")
               :l0-all-seven-ladder-advance-refused
               (boolean (:audit/l0-all-seven-ladder-advance-refused ev))
               :l0-held-all-seven-membrane-refused
               (boolean (:audit/l0-held-all-seven-membrane-refused ev true))
               :l0-held-all-seven-disclosure-state
               (or (:audit/l0-held-all-seven-disclosure-state ev) "n/a")
               :l0-held-all-seven-disclosure-held
               (boolean (:audit/l0-held-all-seven-disclosure-held ev true))
               :l0-held-all-seven-entitlements-may-flow
               (boolean (:audit/l0-held-all-seven-entitlements-may-flow ev))
               :l0-held-all-seven-ladder-advance-phase
               (or (:audit/l0-held-all-seven-ladder-advance-phase ev) "n/a")
               :l0-held-all-seven-ladder-advance-refused
               (boolean (:audit/l0-held-all-seven-ladder-advance-refused ev true))
               :l0-held-all-seven-liquidity-loan-executed
               (boolean (:audit/l0-held-all-seven-liquidity-loan-executed ev))
               :l0-held-all-seven-land-grant-executed
               (boolean (:audit/l0-held-all-seven-land-grant-executed ev))
               :l0-exit-state (or (:audit/l0-exit-state ev) "n/a")
               :l0-exit-suspended (boolean (:audit/l0-exit-suspended ev))
               :l0-exit-entitlements-may-flow
               (boolean (:audit/l0-exit-entitlements-may-flow ev))
               :l0-exit-ladder-phase
               (or (:audit/l0-exit-ladder-phase ev) "n/a")
               :l0-exit-ladder-refused
               (boolean (:audit/l0-exit-ladder-refused ev true))
               :l0-reaffirm-state (or (:audit/l0-reaffirm-state ev) "n/a")
               :l0-reaffirm-exit-suspended
               (boolean (:audit/l0-reaffirm-exit-suspended ev))
               :l0-reaffirm-entitlements-may-flow
               (boolean (:audit/l0-reaffirm-entitlements-may-flow ev true))
               :l0-reaffirm-ladder-phase
               (or (:audit/l0-reaffirm-ladder-phase ev) "n/a")
               :l0-reaffirm-ladder-refused
               (boolean (:audit/l0-reaffirm-ladder-refused ev))
               :l0-falsehood-held (boolean (:audit/l0-falsehood-held ev true))
               :l0-falsehood-entitlements-may-flow
               (boolean (:audit/l0-falsehood-entitlements-may-flow ev))
               :l0-falsehood-ladder-phase
               (or (:audit/l0-falsehood-ladder-phase ev) "n/a")
               :l0-falsehood-ladder-refused
               (boolean (:audit/l0-falsehood-ladder-refused ev true))
               :l0-lift-state (or (:audit/l0-lift-state ev) "n/a")
               :l0-lift-entitlements-may-flow
               (boolean (:audit/l0-lift-entitlements-may-flow ev true))
               :l0-lift-ladder-phase
               (or (:audit/l0-lift-ladder-phase ev) "n/a")
               :l0-lift-ladder-refused
               (boolean (:audit/l0-lift-ladder-refused ev))
               :l0-care-first-disclosure-state
               (or (:audit/l0-care-first-disclosure-state ev) "n/a")
               :l0-care-first-care-full-chain-refused
               (boolean (:audit/l0-care-first-care-full-chain-refused ev true))
               :l0-care-first-mitsuho-full-chain-refused
               (boolean (:audit/l0-care-first-mitsuho-full-chain-refused ev true))
               :l0-care-first-both-refused
               (boolean (:audit/l0-care-first-both-refused ev true))
               :l0-care-first-ladder-phase
               (or (:audit/l0-care-first-ladder-phase ev) "n/a")
               :l0-care-first-ladder-refused
               (boolean (:audit/l0-care-first-ladder-refused ev))
               :l0-care-first-held-stress-ladder-refused
               (boolean (:audit/l0-care-first-held-stress-ladder-refused ev true))
               :l0-care-first-mitsuho-live-produce
               (boolean (:audit/l0-care-first-mitsuho-live-produce ev))
               :l0-care-first-mitsuho-produce-executed
               (boolean (:audit/l0-care-first-mitsuho-produce-executed ev))
               :l0-care-first-care-delivery-executed
               (boolean (:audit/l0-care-first-care-delivery-executed ev))
               :l0-care-first-api-path
               (or (:audit/l0-care-first-api-path ev) "care-first-mitsuho-path")
               :l0-care-first-hikari-both-refused
               (boolean (:audit/l0-care-first-hikari-both-refused ev true))
               :l0-care-first-hikari-ladder-phase
               (or (:audit/l0-care-first-hikari-ladder-phase ev) "n/a")
               :l0-care-first-hikari-ladder-refused
               (boolean (:audit/l0-care-first-hikari-ladder-refused ev))
               :l0-care-first-hikari-held-stress-ladder-refused
               (boolean (:audit/l0-care-first-hikari-held-stress-ladder-refused ev true))
               :l0-care-first-hikari-live-produce
               (boolean (:audit/l0-care-first-hikari-live-produce ev))
               :l0-care-first-hikari-generate-executed
               (boolean (:audit/l0-care-first-hikari-generate-executed ev))
               :l0-care-first-hikari-api-path
               (or (:audit/l0-care-first-hikari-api-path ev) "care-first-hikari-path")
               :l0-care-first-mitsuho-hikari-all-refused
               (boolean (:audit/l0-care-first-mitsuho-hikari-all-refused ev true))
               :l0-care-first-mitsuho-hikari-mitsuho-hikari-both-refused
               (boolean (:audit/l0-care-first-mitsuho-hikari-mitsuho-hikari-both-refused ev true))
               :l0-care-first-mitsuho-hikari-ladder-phase
               (or (:audit/l0-care-first-mitsuho-hikari-ladder-phase ev) "n/a")
               :l0-care-first-mitsuho-hikari-ladder-refused
               (boolean (:audit/l0-care-first-mitsuho-hikari-ladder-refused ev))
               :l0-care-first-mitsuho-hikari-held-stress-ladder-refused
               (boolean (:audit/l0-care-first-mitsuho-hikari-held-stress-ladder-refused ev true))
               :l0-care-first-mitsuho-hikari-mitsuho-live-produce
               (boolean (:audit/l0-care-first-mitsuho-hikari-mitsuho-live-produce ev))
               :l0-care-first-mitsuho-hikari-hikari-live-produce
               (boolean (:audit/l0-care-first-mitsuho-hikari-hikari-live-produce ev))
               :l0-care-housing-both-refused
               (boolean (:audit/l0-care-housing-both-refused ev true))
               :l0-care-housing-land-grant-executed
               (boolean (:audit/l0-care-housing-land-grant-executed ev))
               :l0-care-housing-ladder-phase
               (or (:audit/l0-care-housing-ladder-phase ev) "n/a")
               :l0-care-housing-ladder-refused
               (boolean (:audit/l0-care-housing-ladder-refused ev))
               :l0-care-housing-held-stress-ladder-refused
               (boolean (:audit/l0-care-housing-held-stress-ladder-refused ev true))
               :l0-multi-gen-substrate-all-refused
               (boolean (:audit/l0-multi-gen-substrate-all-refused ev true))
               :l0-multi-gen-substrate-care-housing-both-refused
               (boolean (:audit/l0-multi-gen-substrate-care-housing-both-refused ev true))
               :l0-multi-gen-substrate-mitsuho-hikari-both-refused
               (boolean (:audit/l0-multi-gen-substrate-mitsuho-hikari-both-refused ev true))
               :l0-multi-gen-substrate-land-grant-executed
               (boolean (:audit/l0-multi-gen-substrate-land-grant-executed ev))
               :l0-multi-gen-substrate-ladder-phase
               (or (:audit/l0-multi-gen-substrate-ladder-phase ev) "n/a")
               :l0-multi-gen-substrate-ladder-refused
               (boolean (:audit/l0-multi-gen-substrate-ladder-refused ev))
               :l0-multi-gen-substrate-held-stress-ladder-refused
               (boolean (:audit/l0-multi-gen-substrate-held-stress-ladder-refused ev true))
               :l0-full-inkind-all-refused
               (boolean (:audit/l0-full-inkind-all-refused ev true))
               :l0-full-inkind-tooling-compute-both-refused
               (boolean (:audit/l0-full-inkind-tooling-compute-both-refused ev true))
               :l0-full-inkind-land-grant-executed
               (boolean (:audit/l0-full-inkind-land-grant-executed ev))
               :l0-full-inkind-fulfillment-executed
               (boolean (:audit/l0-full-inkind-fulfillment-executed ev))
               :l0-full-inkind-quota-executed
               (boolean (:audit/l0-full-inkind-quota-executed ev))
               :l0-full-inkind-ladder-phase
               (or (:audit/l0-full-inkind-ladder-phase ev) "n/a")
               :l0-full-inkind-ladder-refused
               (boolean (:audit/l0-full-inkind-ladder-refused ev))
               :l0-full-inkind-held-stress-ladder-refused
               (boolean (:audit/l0-full-inkind-held-stress-ladder-refused ev true))
               :l0-vocation-recovery-both-refused
               (boolean (:audit/l0-vocation-recovery-both-refused ev true))
               :l0-vocation-recovery-fulfillment-executed
               (boolean (:audit/l0-vocation-recovery-fulfillment-executed ev))
               :l0-vocation-recovery-quota-executed
               (boolean (:audit/l0-vocation-recovery-quota-executed ev))
               :l0-vocation-recovery-ladder-phase
               (or (:audit/l0-vocation-recovery-ladder-phase ev) "n/a")
               :l0-vocation-recovery-ladder-refused
               (boolean (:audit/l0-vocation-recovery-ladder-refused ev))
               :l0-vocation-recovery-held-stress-ladder-refused
               (boolean (:audit/l0-vocation-recovery-held-stress-ladder-refused ev true))
               :l0-liquidity-residual-receive-refused
               (boolean (:audit/l0-liquidity-residual-receive-refused ev true))
               :l0-liquidity-residual-member-principal
               (boolean (:audit/l0-liquidity-residual-member-principal ev true))
               :l0-liquidity-residual-loan-executed
               (boolean (:audit/l0-liquidity-residual-loan-executed ev))
               :l0-liquidity-residual-cash-usd-micros
               (or (:audit/l0-liquidity-residual-cash-usd-micros ev) 0)
               :l0-liquidity-residual-ladder-phase
               (or (:audit/l0-liquidity-residual-ladder-phase ev) "n/a")
               :l0-liquidity-residual-ladder-refused
               (boolean (:audit/l0-liquidity-residual-ladder-refused ev))
               :l0-liquidity-residual-held-stress-ladder-refused
               (boolean (:audit/l0-liquidity-residual-held-stress-ladder-refused ev true))
               :l0-all-seven-substrate-all-inkind-refused
               (boolean (:audit/l0-all-seven-substrate-all-inkind-refused ev true))
               :l0-all-seven-substrate-membrane-refused
               (boolean (:audit/l0-all-seven-substrate-membrane-refused ev true))
               :l0-all-seven-substrate-loan-executed
               (boolean (:audit/l0-all-seven-substrate-loan-executed ev))
               :l0-all-seven-substrate-land-grant-executed
               (boolean (:audit/l0-all-seven-substrate-land-grant-executed ev))
               :l0-all-seven-substrate-ladder-phase
               (or (:audit/l0-all-seven-substrate-ladder-phase ev) "n/a")
               :l0-all-seven-substrate-ladder-refused
               (boolean (:audit/l0-all-seven-substrate-ladder-refused ev))
               :l0-all-seven-substrate-held-stress-ladder-refused
               (boolean (:audit/l0-all-seven-substrate-held-stress-ladder-refused ev true))
               :l0-priority-path-count
               (or (:audit/l0-priority-path-count ev) 0)
               :l0-priority-held-stress-embed-count
               (or (:audit/l0-priority-held-stress-embed-count ev) 0)
               :l0-priority-held-stress-embed-all
               (boolean (:audit/l0-priority-held-stress-embed-all ev true))
               :priority-stack-ok
               (boolean (:audit/priority-stack-ok ev))
               :priority-stack-l0-stage
               (or (:audit/priority-stack-l0-stage ev) "n/a")
               :priority-stack-l0-published
               (boolean (:audit/priority-stack-l0-published ev))
               :priority-stack-disclosure-open-may-flow
               (boolean (:audit/priority-stack-disclosure-open-may-flow ev))
               :priority-stack-disclosure-stale-held
               (boolean (:audit/priority-stack-disclosure-stale-held ev))
               :priority-stack-disclosure-tick-final
               (or (:audit/priority-stack-disclosure-tick-final ev) "n/a")
               :priority-stack-mitsuho-r1-phase
               (or (:audit/priority-stack-mitsuho-r1-phase ev) "n/a")
               :priority-stack-mitsuho-gated-phase
               (or (:audit/priority-stack-mitsuho-gated-phase ev) "n/a")
               :priority-stack-mitsuho-produce-executed
               (boolean (:audit/priority-stack-mitsuho-produce-executed ev))
               :priority-stack-mitsuho-care-first-api
               (or (:audit/priority-stack-mitsuho-care-first-api ev) "n/a")
               :priority-stack-mitsuho-held-stress-ladder-refused
               (boolean (:audit/priority-stack-mitsuho-held-stress-ladder-refused ev))
               :rail-design-rail-count
               (or (:audit/rail-design-rail-count ev) 0)
               :rail-design-ok-count
               (or (:audit/rail-design-ok-count ev) 0)
               :rail-design-live-produce-never
               (boolean (:audit/rail-design-live-produce-never ev true))
               :rail-design-all-cash-zero
               (boolean (:audit/rail-design-all-cash-zero ev true))
               :rail-design-all-live-false
               (boolean (:audit/rail-design-all-live-false ev true))
               :rail-design-all-seven
               (boolean (:audit/rail-design-all-seven ev true))
               :ss-care-live-produce
               (boolean (:audit/ss-care-live-produce ev))
               :ss-housing-live-produce
               (boolean (:audit/ss-housing-live-produce ev))
               :ss-care-care-first-api-path
               (or (:audit/ss-care-care-first-api-path ev) "care-housing-first-path")
               :ss-housing-care-first-api-path
               (or (:audit/ss-housing-care-first-api-path ev) "care-housing-first-path")
               :ss-care-design-rail-kind
               (or (:audit/ss-care-design-rail-kind ev) "care-iyashi")
               :ss-housing-design-rail-kind
               (or (:audit/ss-housing-design-rail-kind ev) "housing-commons")
               :ss-mitsuho-live-produce
               (boolean (:audit/ss-mitsuho-live-produce ev))
               :ss-hikari-live-produce
               (boolean (:audit/ss-hikari-live-produce ev))
               :ss-mitsuho-care-first-api-path
               (or (:audit/ss-mitsuho-care-first-api-path ev) "care-first-mitsuho-path")
               :ss-hikari-care-first-api-path
               (or (:audit/ss-hikari-care-first-api-path ev) "care-first-hikari-path")
               :ss-tooling-live-produce
               (boolean (:audit/ss-tooling-live-produce ev))
               :ss-compute-live-produce
               (boolean (:audit/ss-compute-live-produce ev))
               :ss-liquidity-live-produce
               (boolean (:audit/ss-liquidity-live-produce ev))
               :ss-all-seven-design-embed-count
               (or (:audit/ss-all-seven-design-embed-count ev) 7)
               :ss-all-seven-design-live-produce-never
               (boolean (:audit/ss-all-seven-design-live-produce-never ev true))
               :ss-disclosure-state (or (:audit/ss-disclosure-state ev) "n/a")
               :all-live-refused (boolean (:audit/all-live-refused ev))
               :l4-disclosure-open (or (:audit/l4-disclosure-open ev) 0)
               :l4-disclosure-held (or (:audit/l4-disclosure-held ev) 0)
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK}]
      (pp/assert-no-public-scores! out)
      out)))

(defn summary
  "Aggregate facts across ledger (no scores).
   Includes last-run post-ratify/flowable snapshot (USD micros are not summed across runs —
   each run is a full offline recompute, so last-run is the authoritative latest package).
   Portable: read-all I/O works under bb and nbb."
  ([]
   (summary (read-all)))
  ([events]
      (let [last-ev (last events)
            empty? (empty? events)
            out {:runs (count events)
                 :total-enrolled (reduce + 0 (map :audit/enrolled-subjects events))
                 :total-tenure (reduce + 0 (map :audit/tenure-subjects events))
                 :total-l4-disclosure-open
                 (reduce + 0 (map #(or (:audit/l4-disclosure-open %) 0) events))
                 :total-l4-disclosure-held
                 (reduce + 0 (map #(or (:audit/l4-disclosure-held %) 0) events))
                 :total-tenure-disclosure-open
                 (reduce + 0 (map #(or (:audit/tenure-disclosure-open %) 0) events))
                 :total-tenure-disclosure-held
                 (reduce + 0 (map #(or (:audit/tenure-disclosure-held %) 0) events))
                 :total-mitsuho-gated-refused
                 (reduce + 0 (map #(or (:audit/mitsuho-gated-refused %) 0) events))
                 :total-hikari-gated-refused
                 (reduce + 0 (map #(or (:audit/hikari-gated-refused %) 0) events))
                 :total-care-gated-refused
                 (reduce + 0 (map #(or (:audit/care-gated-refused %) 0) events))
                 :total-housing-gated-refused
                 (reduce + 0 (map #(or (:audit/housing-gated-refused %) 0) events))
                 :total-tooling-gated-refused
                 (reduce + 0 (map #(or (:audit/tooling-gated-refused %) 0) events))
                 :total-compute-gated-refused
                 (reduce + 0 (map #(or (:audit/compute-gated-refused %) 0) events))
                 :total-liquidity-gated-refused
                 (reduce + 0 (map #(or (:audit/liquidity-gated-refused %) 0) events))
                 :total-liquidity-member-principal
                 (reduce + 0 (map #(or (:audit/liquidity-member-principal %) 0) events))
                 :total-liquidity-cash-usd-micros
                 (reduce + 0 (map #(or (:audit/liquidity-cash-usd-micros %) 0) events))
                 :total-housing-land-grant-executed
                 (reduce + 0 (map #(or (:audit/housing-land-grant-executed %) 0) events))
                 :all-runs-live-refused (if empty? true (every? :audit/all-live-refused events))
                 :any-land-grant-executed?
                 (boolean (some #(pos? (or (:audit/housing-land-grant-executed %) 0)) events))
                 ;; last offline package facts (post-ratify vs flowable; land-grant stays 0)
                 :last-run (last-run-snapshot last-ev)
                 :last-run-gov-flowable-committed-usd-micros
                 (or (:audit/gov-flowable-committed-usd-micros last-ev) 0)
                 :last-run-gov-post-ratify-committed-usd-micros
                 (or (:audit/gov-post-ratify-committed-usd-micros last-ev) 0)
                 :last-run-tenure-gov-flowable-committed-usd-micros
                 (or (:audit/tenure-gov-flowable-committed-usd-micros last-ev) 0)
                 :last-run-tenure-gov-post-ratify-committed-usd-micros
                 (or (:audit/tenure-gov-post-ratify-committed-usd-micros last-ev) 0)
                 :last-run-housing-land-grant-executed
                 (or (:audit/housing-land-grant-executed last-ev) 0)
                 :last-run-r2-refused (or (:audit/r2-refused last-ev) 0)
                 :last-run-r2-executed (or (:audit/r2-executed last-ev) 0)
                 :last-run-all-r2-not-executed
                 (boolean (or (:audit/all-r2-not-executed last-ev)
                              (zero? (or (:audit/r2-executed last-ev) 0))))
                 :last-run-ss-rails-gated-count
                 (or (:audit/ss-rails-gated-count last-ev) 0)
                 :last-run-ss-all-rails-gated-refused
                 (boolean (:audit/ss-all-rails-gated-refused last-ev true))
                 :last-run-ss-r2-status-count
                 (or (:audit/ss-r2-status-count last-ev) 0)
                 :last-run-ss-all-r2-not-executed
                 (boolean (:audit/ss-all-r2-not-executed last-ev true))
                 :last-run-ss-l0-published
                 (boolean (:audit/ss-l0-published last-ev))
                 :last-run-ss-l0-disclosure-state
                 (or (:audit/ss-l0-disclosure-state last-ev) "n/a")
                 :last-run-ss-l0-disclosure-held
                 (boolean (:audit/ss-l0-disclosure-held last-ev))
                 :last-run-ss-l0-entitlements-may-flow
                 (boolean (:audit/ss-l0-entitlements-may-flow last-ev true))
                 :last-run-ss-l0-path
                 (or (:audit/ss-l0-path last-ev) "l0-enroll-offline")
                 :last-run-ss-ladder-to
                 (or (:audit/ss-ladder-to last-ev) "n/a")
                 :last-run-ss-ladder-rails-hint-first
                 (or (:audit/ss-ladder-rails-hint-first last-ev) "n/a")
                 :last-run-ss-held-stress-ladder-refused
                 (boolean (:audit/ss-held-stress-ladder-refused last-ev))
                 :last-run-ss-stage-rails-first
                 (or (:audit/ss-stage-rails-first last-ev) "n/a")
                 :last-run-ss-stage-rails-second
                 (or (:audit/ss-stage-rails-second last-ev) "n/a")
                 :last-run-ss-stage-gated-count
                 (or (:audit/ss-stage-gated-count last-ev) 0)
                 :last-run-ss-stage-all-gated-refused
                 (boolean (:audit/ss-stage-all-gated-refused last-ev true))
                 :last-run-ss-stage-r2-all-refused
                 (boolean (:audit/ss-stage-r2-all-refused last-ev true))
                 :last-run-ss-stage-care-gated-admissible
                 (boolean (:audit/ss-stage-care-gated-admissible last-ev))
                 :last-run-ss-stage-mitsuho-gated-admissible
                 (boolean (:audit/ss-stage-mitsuho-gated-admissible last-ev))
                 :last-run-ss-stage-hikari-gated-admissible
                 (boolean (:audit/ss-stage-hikari-gated-admissible last-ev))
                 :last-run-ss-stage-land-grant-executed
                 (boolean (:audit/ss-stage-land-grant-executed last-ev))
                 :last-run-ss-mitsuho-gated-receive-admissible
                 (boolean (:audit/ss-mitsuho-gated-receive-admissible last-ev))
                 :last-run-ss-hikari-gated-receive-admissible
                 (boolean (:audit/ss-hikari-gated-receive-admissible last-ev))
                 :last-run-ss-care-gated-receive-admissible
                 (boolean (:audit/ss-care-gated-receive-admissible last-ev))
                 :last-run-ss-mitsuho-hikari-receive-both-refused
                 (boolean (:audit/ss-mitsuho-hikari-receive-both-refused last-ev true))
                 :last-run-ss-care-mitsuho-hikari-receive-all-refused
                 (boolean (:audit/ss-care-mitsuho-hikari-receive-all-refused last-ev true))
                 :last-run-ss-mitsuho-gated-produce-admissible
                 (boolean (:audit/ss-mitsuho-gated-produce-admissible last-ev))
                 :last-run-ss-hikari-gated-produce-admissible
                 (boolean (:audit/ss-hikari-gated-produce-admissible last-ev))
                 :last-run-ss-mitsuho-hikari-produce-both-refused
                 (boolean (:audit/ss-mitsuho-hikari-produce-both-refused last-ev true))
                 :last-run-ss-mitsuho-hikari-full-chain-refused
                 (boolean (:audit/ss-mitsuho-hikari-full-chain-refused last-ev true))
                 :last-run-ss-care-gated-produce-admissible
                 (boolean (:audit/ss-care-gated-produce-admissible last-ev))
                 :last-run-ss-care-mitsuho-hikari-produce-all-refused
                 (boolean (:audit/ss-care-mitsuho-hikari-produce-all-refused last-ev true))
                 :last-run-ss-care-mitsuho-hikari-full-chain-refused
                 (boolean (:audit/ss-care-mitsuho-hikari-full-chain-refused last-ev true))
                 :last-run-ss-housing-gated-receive-admissible
                 (boolean (:audit/ss-housing-gated-receive-admissible last-ev))
                 :last-run-ss-housing-gated-produce-admissible
                 (boolean (:audit/ss-housing-gated-produce-admissible last-ev))
                 :last-run-ss-housing-full-chain-refused
                 (boolean (:audit/ss-housing-full-chain-refused last-ev true))
                 :last-run-ss-care-housing-mitsuho-hikari-receive-all-refused
                 (boolean (:audit/ss-care-housing-mitsuho-hikari-receive-all-refused last-ev true))
                 :last-run-ss-care-housing-mitsuho-hikari-produce-all-refused
                 (boolean (:audit/ss-care-housing-mitsuho-hikari-produce-all-refused last-ev true))
                 :last-run-ss-care-housing-mitsuho-hikari-full-chain-refused
                 (boolean (:audit/ss-care-housing-mitsuho-hikari-full-chain-refused last-ev true))
                 :last-run-ss-tooling-gated-receive-admissible
                 (boolean (:audit/ss-tooling-gated-receive-admissible last-ev))
                 :last-run-ss-tooling-gated-produce-admissible
                 (boolean (:audit/ss-tooling-gated-produce-admissible last-ev))
                 :last-run-ss-tooling-full-chain-refused
                 (boolean (:audit/ss-tooling-full-chain-refused last-ev true))
                 :last-run-ss-compute-gated-receive-admissible
                 (boolean (:audit/ss-compute-gated-receive-admissible last-ev))
                 :last-run-ss-compute-gated-produce-admissible
                 (boolean (:audit/ss-compute-gated-produce-admissible last-ev))
                 :last-run-ss-compute-full-chain-refused
                 (boolean (:audit/ss-compute-full-chain-refused last-ev true))
                 :last-run-ss-tooling-compute-full-chain-refused
                 (boolean (:audit/ss-tooling-compute-full-chain-refused last-ev true))
                 :last-run-ss-all-inkind-produce-rails-full-chain-refused
                 (boolean (:audit/ss-all-inkind-produce-rails-full-chain-refused last-ev true))
                 :last-run-ss-liquidity-gated-receive-admissible
                 (boolean (:audit/ss-liquidity-gated-receive-admissible last-ev))
                 :last-run-ss-liquidity-receive-full-chain-refused
                 (boolean (:audit/ss-liquidity-receive-full-chain-refused last-ev true))
                 :last-run-ss-all-seven-rails-receive-membrane-refused
                 (boolean (:audit/ss-all-seven-rails-receive-membrane-refused last-ev true))
                 :last-run-displacement-membrane-subjects
                 (or (:audit/displacement-membrane-subjects last-ev) 0)
                 :last-run-displacement-held-stress-subjects
                 (or (:audit/displacement-held-stress-subjects last-ev) 0)
                 :last-run-displacement-held-stress-ladder-refused
                 (or (:audit/displacement-held-stress-ladder-refused last-ev) 0)
                 :last-run-tenure-held-stress-subjects
                 (or (:audit/tenure-held-stress-subjects last-ev) 0)
                 :last-run-tenure-held-stress-ladder-refused
                 (or (:audit/tenure-held-stress-ladder-refused last-ev) 0)
                 :last-run-tenure-held-stress-carried
                 (or (:audit/tenure-held-stress-carried last-ev) 0)
                 :last-run-gov-held-stress-subjects
                 (or (:audit/gov-held-stress-subjects last-ev) 0)
                 :last-run-gov-held-stress-ladder-refused
                 (or (:audit/gov-held-stress-ladder-refused last-ev) 0)
                 :last-run-tenure-gov-held-stress-subjects
                 (or (:audit/tenure-gov-held-stress-subjects last-ev) 0)
                 :last-run-tenure-gov-held-stress-ladder-refused
                 (or (:audit/tenure-gov-held-stress-ladder-refused last-ev) 0)
                 :last-run-displacement-care-housing-full-chain-refused
                 (or (:audit/displacement-care-housing-full-chain-refused last-ev) 0)
                 :last-run-displacement-all-inkind-full-chain-refused
                 (or (:audit/displacement-all-inkind-full-chain-refused last-ev) 0)
                 :last-run-displacement-all-seven-receive-membrane-refused
                 (or (:audit/displacement-all-seven-receive-membrane-refused last-ev) 0)
                 :last-run-displacement-liquidity-recv-refused
                 (or (:audit/displacement-liquidity-recv-refused last-ev) 0)
                 :last-run-l0-all-seven-membrane-refused
                 (boolean (:audit/l0-all-seven-membrane-refused last-ev true))
                 :last-run-l0-all-seven-all-inkind-refused
                 (boolean (:audit/l0-all-seven-all-inkind-refused last-ev true))
                 :last-run-l0-all-seven-liquidity-receive-refused
                 (boolean (:audit/l0-all-seven-liquidity-receive-refused last-ev true))
                 :last-run-l0-all-seven-disclosure-state
                 (or (:audit/l0-all-seven-disclosure-state last-ev) "n/a")
                 :last-run-l0-all-seven-liquidity-member-principal
                 (boolean (:audit/l0-all-seven-liquidity-member-principal last-ev true))
                 :last-run-l0-all-seven-liquidity-loan-executed
                 (boolean (:audit/l0-all-seven-liquidity-loan-executed last-ev))
                 :last-run-l0-all-seven-liquidity-cash-usd-micros
                 (or (:audit/l0-all-seven-liquidity-cash-usd-micros last-ev) 0)
                 :last-run-l0-all-seven-land-grant-executed
                 (boolean (:audit/l0-all-seven-land-grant-executed last-ev))
                 :last-run-l0-all-seven-continuity-final-state
                 (or (:audit/l0-all-seven-continuity-final-state last-ev) "n/a")
                 :last-run-l0-all-seven-continuity-held-steps
                 (or (:audit/l0-all-seven-continuity-held-steps last-ev) 0)
                 :last-run-l0-all-seven-ladder-advance-phase
                 (or (:audit/l0-all-seven-ladder-advance-phase last-ev) "n/a")
                 :last-run-l0-all-seven-ladder-advance-refused
                 (boolean (:audit/l0-all-seven-ladder-advance-refused last-ev))
                 :last-run-l0-held-all-seven-membrane-refused
                 (boolean (:audit/l0-held-all-seven-membrane-refused last-ev true))
                 :last-run-l0-held-all-seven-disclosure-state
                 (or (:audit/l0-held-all-seven-disclosure-state last-ev) "n/a")
                 :last-run-l0-held-all-seven-disclosure-held
                 (boolean (:audit/l0-held-all-seven-disclosure-held last-ev true))
                 :last-run-l0-held-all-seven-entitlements-may-flow
                 (boolean (:audit/l0-held-all-seven-entitlements-may-flow last-ev))
                 :last-run-l0-held-all-seven-ladder-advance-phase
                 (or (:audit/l0-held-all-seven-ladder-advance-phase last-ev) "n/a")
                 :last-run-l0-held-all-seven-ladder-advance-refused
                 (boolean (:audit/l0-held-all-seven-ladder-advance-refused last-ev true))
                 :last-run-l0-held-all-seven-liquidity-loan-executed
                 (boolean (:audit/l0-held-all-seven-liquidity-loan-executed last-ev))
                 :last-run-l0-held-all-seven-land-grant-executed
                 (boolean (:audit/l0-held-all-seven-land-grant-executed last-ev))
                 :last-run-l0-exit-state
                 (or (:audit/l0-exit-state last-ev) "n/a")
                 :last-run-l0-exit-suspended
                 (boolean (:audit/l0-exit-suspended last-ev))
                 :last-run-l0-exit-entitlements-may-flow
                 (boolean (:audit/l0-exit-entitlements-may-flow last-ev))
                 :last-run-l0-exit-ladder-phase
                 (or (:audit/l0-exit-ladder-phase last-ev) "n/a")
                 :last-run-l0-exit-ladder-refused
                 (boolean (:audit/l0-exit-ladder-refused last-ev true))
                 :last-run-l0-reaffirm-state
                 (or (:audit/l0-reaffirm-state last-ev) "n/a")
                 :last-run-l0-reaffirm-exit-suspended
                 (boolean (:audit/l0-reaffirm-exit-suspended last-ev))
                 :last-run-l0-reaffirm-entitlements-may-flow
                 (boolean (:audit/l0-reaffirm-entitlements-may-flow last-ev true))
                 :last-run-l0-reaffirm-ladder-phase
                 (or (:audit/l0-reaffirm-ladder-phase last-ev) "n/a")
                 :last-run-l0-reaffirm-ladder-refused
                 (boolean (:audit/l0-reaffirm-ladder-refused last-ev))
                 :last-run-l0-falsehood-held
                 (boolean (:audit/l0-falsehood-held last-ev true))
                 :last-run-l0-falsehood-ladder-refused
                 (boolean (:audit/l0-falsehood-ladder-refused last-ev true))
                 :last-run-l0-lift-state
                 (or (:audit/l0-lift-state last-ev) "n/a")
                 :last-run-l0-lift-ladder-phase
                 (or (:audit/l0-lift-ladder-phase last-ev) "n/a")
                 :last-run-l0-lift-ladder-refused
                 (boolean (:audit/l0-lift-ladder-refused last-ev))
                 :last-run-l0-care-first-both-refused
                 (boolean (:audit/l0-care-first-both-refused last-ev true))
                 :last-run-l0-care-first-ladder-phase
                 (or (:audit/l0-care-first-ladder-phase last-ev) "n/a")
                 :last-run-l0-care-first-ladder-refused
                 (boolean (:audit/l0-care-first-ladder-refused last-ev))
                 :last-run-l0-care-first-mitsuho-live-produce
                 (boolean (:audit/l0-care-first-mitsuho-live-produce last-ev))
                 :last-run-l0-care-first-mitsuho-produce-executed
                 (boolean (:audit/l0-care-first-mitsuho-produce-executed last-ev))
                 :last-run-l0-care-first-care-delivery-executed
                 (boolean (:audit/l0-care-first-care-delivery-executed last-ev))
                 :last-run-l0-care-first-api-path
                 (or (:audit/l0-care-first-api-path last-ev) "care-first-mitsuho-path")
                 :last-run-l0-care-first-held-stress-ladder-refused
                 (boolean (:audit/l0-care-first-held-stress-ladder-refused last-ev true))
                 :last-run-l0-care-first-hikari-both-refused
                 (boolean (:audit/l0-care-first-hikari-both-refused last-ev true))
                 :last-run-l0-care-first-hikari-ladder-phase
                 (or (:audit/l0-care-first-hikari-ladder-phase last-ev) "n/a")
                 :last-run-l0-care-first-hikari-ladder-refused
                 (boolean (:audit/l0-care-first-hikari-ladder-refused last-ev))
                 :last-run-l0-care-first-hikari-held-stress-ladder-refused
                 (boolean (:audit/l0-care-first-hikari-held-stress-ladder-refused last-ev true))
                 :last-run-l0-care-first-hikari-live-produce
                 (boolean (:audit/l0-care-first-hikari-live-produce last-ev))
                 :last-run-l0-care-first-hikari-generate-executed
                 (boolean (:audit/l0-care-first-hikari-generate-executed last-ev))
                 :last-run-l0-care-first-hikari-api-path
                 (or (:audit/l0-care-first-hikari-api-path last-ev) "care-first-hikari-path")
                 :last-run-l0-care-first-mitsuho-hikari-all-refused
                 (boolean (:audit/l0-care-first-mitsuho-hikari-all-refused last-ev true))
                 :last-run-l0-care-first-mitsuho-hikari-mitsuho-hikari-both-refused
                 (boolean (:audit/l0-care-first-mitsuho-hikari-mitsuho-hikari-both-refused last-ev true))
                 :last-run-l0-care-first-mitsuho-hikari-ladder-phase
                 (or (:audit/l0-care-first-mitsuho-hikari-ladder-phase last-ev) "n/a")
                 :last-run-l0-care-first-mitsuho-hikari-ladder-refused
                 (boolean (:audit/l0-care-first-mitsuho-hikari-ladder-refused last-ev))
                 :last-run-l0-care-first-mitsuho-hikari-held-stress-ladder-refused
                 (boolean (:audit/l0-care-first-mitsuho-hikari-held-stress-ladder-refused last-ev true))
                 :last-run-l0-care-first-mitsuho-hikari-mitsuho-live-produce
                 (boolean (:audit/l0-care-first-mitsuho-hikari-mitsuho-live-produce last-ev))
                 :last-run-l0-care-first-mitsuho-hikari-hikari-live-produce
                 (boolean (:audit/l0-care-first-mitsuho-hikari-hikari-live-produce last-ev))
                 :last-run-l0-care-housing-both-refused
                 (boolean (:audit/l0-care-housing-both-refused last-ev true))
                 :last-run-l0-care-housing-land-grant-executed
                 (boolean (:audit/l0-care-housing-land-grant-executed last-ev))
                 :last-run-l0-care-housing-ladder-phase
                 (or (:audit/l0-care-housing-ladder-phase last-ev) "n/a")
                 :last-run-l0-care-housing-ladder-refused
                 (boolean (:audit/l0-care-housing-ladder-refused last-ev))
                 :last-run-l0-care-housing-held-stress-ladder-refused
                 (boolean (:audit/l0-care-housing-held-stress-ladder-refused last-ev true))
                 :last-run-l0-multi-gen-substrate-all-refused
                 (boolean (:audit/l0-multi-gen-substrate-all-refused last-ev true))
                 :last-run-l0-multi-gen-substrate-care-housing-both-refused
                 (boolean (:audit/l0-multi-gen-substrate-care-housing-both-refused last-ev true))
                 :last-run-l0-multi-gen-substrate-mitsuho-hikari-both-refused
                 (boolean (:audit/l0-multi-gen-substrate-mitsuho-hikari-both-refused last-ev true))
                 :last-run-l0-multi-gen-substrate-land-grant-executed
                 (boolean (:audit/l0-multi-gen-substrate-land-grant-executed last-ev))
                 :last-run-l0-multi-gen-substrate-ladder-phase
                 (or (:audit/l0-multi-gen-substrate-ladder-phase last-ev) "n/a")
                 :last-run-l0-multi-gen-substrate-ladder-refused
                 (boolean (:audit/l0-multi-gen-substrate-ladder-refused last-ev))
                 :last-run-l0-multi-gen-substrate-held-stress-ladder-refused
                 (boolean (:audit/l0-multi-gen-substrate-held-stress-ladder-refused last-ev true))
                 :last-run-l0-full-inkind-all-refused
                 (boolean (:audit/l0-full-inkind-all-refused last-ev true))
                 :last-run-l0-full-inkind-tooling-compute-both-refused
                 (boolean (:audit/l0-full-inkind-tooling-compute-both-refused last-ev true))
                 :last-run-l0-full-inkind-land-grant-executed
                 (boolean (:audit/l0-full-inkind-land-grant-executed last-ev))
                 :last-run-l0-full-inkind-fulfillment-executed
                 (boolean (:audit/l0-full-inkind-fulfillment-executed last-ev))
                 :last-run-l0-full-inkind-quota-executed
                 (boolean (:audit/l0-full-inkind-quota-executed last-ev))
                 :last-run-l0-full-inkind-ladder-phase
                 (or (:audit/l0-full-inkind-ladder-phase last-ev) "n/a")
                 :last-run-l0-full-inkind-ladder-refused
                 (boolean (:audit/l0-full-inkind-ladder-refused last-ev))
                 :last-run-l0-full-inkind-held-stress-ladder-refused
                 (boolean (:audit/l0-full-inkind-held-stress-ladder-refused last-ev true))
                 :last-run-l0-vocation-recovery-both-refused
                 (boolean (:audit/l0-vocation-recovery-both-refused last-ev true))
                 :last-run-l0-vocation-recovery-fulfillment-executed
                 (boolean (:audit/l0-vocation-recovery-fulfillment-executed last-ev))
                 :last-run-l0-vocation-recovery-quota-executed
                 (boolean (:audit/l0-vocation-recovery-quota-executed last-ev))
                 :last-run-l0-vocation-recovery-ladder-phase
                 (or (:audit/l0-vocation-recovery-ladder-phase last-ev) "n/a")
                 :last-run-l0-vocation-recovery-ladder-refused
                 (boolean (:audit/l0-vocation-recovery-ladder-refused last-ev))
                 :last-run-l0-vocation-recovery-held-stress-ladder-refused
                 (boolean (:audit/l0-vocation-recovery-held-stress-ladder-refused last-ev true))
                 :last-run-l0-liquidity-residual-receive-refused
                 (boolean (:audit/l0-liquidity-residual-receive-refused last-ev true))
                 :last-run-l0-liquidity-residual-member-principal
                 (boolean (:audit/l0-liquidity-residual-member-principal last-ev true))
                 :last-run-l0-liquidity-residual-loan-executed
                 (boolean (:audit/l0-liquidity-residual-loan-executed last-ev))
                 :last-run-l0-liquidity-residual-cash-usd-micros
                 (or (:audit/l0-liquidity-residual-cash-usd-micros last-ev) 0)
                 :last-run-l0-liquidity-residual-ladder-phase
                 (or (:audit/l0-liquidity-residual-ladder-phase last-ev) "n/a")
                 :last-run-l0-liquidity-residual-ladder-refused
                 (boolean (:audit/l0-liquidity-residual-ladder-refused last-ev))
                 :last-run-l0-liquidity-residual-held-stress-ladder-refused
                 (boolean (:audit/l0-liquidity-residual-held-stress-ladder-refused last-ev true))
                 :last-run-l0-all-seven-substrate-all-inkind-refused
                 (boolean (:audit/l0-all-seven-substrate-all-inkind-refused last-ev true))
                 :last-run-l0-all-seven-substrate-membrane-refused
                 (boolean (:audit/l0-all-seven-substrate-membrane-refused last-ev true))
                 :last-run-l0-all-seven-substrate-loan-executed
                 (boolean (:audit/l0-all-seven-substrate-loan-executed last-ev))
                 :last-run-l0-all-seven-substrate-land-grant-executed
                 (boolean (:audit/l0-all-seven-substrate-land-grant-executed last-ev))
                 :last-run-l0-all-seven-substrate-ladder-phase
                 (or (:audit/l0-all-seven-substrate-ladder-phase last-ev) "n/a")
                 :last-run-l0-all-seven-substrate-ladder-refused
                 (boolean (:audit/l0-all-seven-substrate-ladder-refused last-ev))
                 :last-run-l0-all-seven-substrate-held-stress-ladder-refused
                 (boolean (:audit/l0-all-seven-substrate-held-stress-ladder-refused last-ev true))
                 :last-run-l0-priority-path-count
                 (or (:audit/l0-priority-path-count last-ev) 0)
                 :last-run-l0-priority-held-stress-embed-count
                 (or (:audit/l0-priority-held-stress-embed-count last-ev) 0)
                 :last-run-l0-priority-held-stress-embed-all
                 (boolean (:audit/l0-priority-held-stress-embed-all last-ev true))
                 :last-run-priority-stack-ok
                 (boolean (:audit/priority-stack-ok last-ev))
                 :last-run-priority-stack-l0-stage
                 (or (:audit/priority-stack-l0-stage last-ev) "n/a")
                 :last-run-priority-stack-l0-published
                 (boolean (:audit/priority-stack-l0-published last-ev))
                 :last-run-priority-stack-disclosure-open-may-flow
                 (boolean (:audit/priority-stack-disclosure-open-may-flow last-ev))
                 :last-run-priority-stack-disclosure-stale-held
                 (boolean (:audit/priority-stack-disclosure-stale-held last-ev))
                 :last-run-priority-stack-disclosure-tick-final
                 (or (:audit/priority-stack-disclosure-tick-final last-ev) "n/a")
                 :last-run-priority-stack-mitsuho-r1-phase
                 (or (:audit/priority-stack-mitsuho-r1-phase last-ev) "n/a")
                 :last-run-priority-stack-mitsuho-gated-phase
                 (or (:audit/priority-stack-mitsuho-gated-phase last-ev) "n/a")
                 :last-run-priority-stack-mitsuho-produce-executed
                 (boolean (:audit/priority-stack-mitsuho-produce-executed last-ev))
                 :last-run-priority-stack-mitsuho-care-first-api
                 (or (:audit/priority-stack-mitsuho-care-first-api last-ev) "n/a")
                 :last-run-priority-stack-mitsuho-held-stress-ladder-refused
                 (boolean (:audit/priority-stack-mitsuho-held-stress-ladder-refused last-ev))
                 :last-run-rail-design-rail-count
                 (or (:audit/rail-design-rail-count last-ev) 0)
                 :last-run-rail-design-ok-count
                 (or (:audit/rail-design-ok-count last-ev) 0)
                 :last-run-rail-design-live-produce-never
                 (boolean (:audit/rail-design-live-produce-never last-ev true))
                 :last-run-rail-design-all-cash-zero
                 (boolean (:audit/rail-design-all-cash-zero last-ev true))
                 :last-run-rail-design-all-live-false
                 (boolean (:audit/rail-design-all-live-false last-ev true))
                 :last-run-rail-design-all-seven
                 (boolean (:audit/rail-design-all-seven last-ev true))
                 :last-run-ss-care-live-produce
                 (boolean (:audit/ss-care-live-produce last-ev))
                 :last-run-ss-housing-live-produce
                 (boolean (:audit/ss-housing-live-produce last-ev))
                 :last-run-ss-care-care-first-api-path
                 (or (:audit/ss-care-care-first-api-path last-ev)
                     "care-housing-first-path")
                 :last-run-ss-housing-care-first-api-path
                 (or (:audit/ss-housing-care-first-api-path last-ev)
                     "care-housing-first-path")
                 :last-run-ss-care-design-rail-kind
                 (or (:audit/ss-care-design-rail-kind last-ev) "care-iyashi")
                 :last-run-ss-housing-design-rail-kind
                 (or (:audit/ss-housing-design-rail-kind last-ev) "housing-commons")
                 :last-run-ss-mitsuho-live-produce
                 (boolean (:audit/ss-mitsuho-live-produce last-ev))
                 :last-run-ss-hikari-live-produce
                 (boolean (:audit/ss-hikari-live-produce last-ev))
                 :last-run-ss-mitsuho-care-first-api-path
                 (or (:audit/ss-mitsuho-care-first-api-path last-ev)
                     "care-first-mitsuho-path")
                 :last-run-ss-hikari-care-first-api-path
                 (or (:audit/ss-hikari-care-first-api-path last-ev)
                     "care-first-hikari-path")
                 :last-run-ss-tooling-live-produce
                 (boolean (:audit/ss-tooling-live-produce last-ev))
                 :last-run-ss-compute-live-produce
                 (boolean (:audit/ss-compute-live-produce last-ev))
                 :last-run-ss-liquidity-live-produce
                 (boolean (:audit/ss-liquidity-live-produce last-ev))
                 :last-run-ss-all-seven-design-embed-count
                 (or (:audit/ss-all-seven-design-embed-count last-ev) 7)
                 :last-run-ss-all-seven-design-live-produce-never
                 (boolean (:audit/ss-all-seven-design-live-produce-never last-ev true))
                 :cash-usd-micros 0
                 :cash-to-workers-usd-micros 0
                 :live false
                 :score-surface []
                 :priority-stack PRIORITY-STACK}]
        (pp/assert-no-public-scores! (dissoc out :last-run))
        (when-let [lr (:last-run out)] (pp/assert-no-public-scores! lr))
        out)))
