(ns fuchi.methods.displacement-pipeline
  "displacement_pipeline.cljc — single offline entry for robotics/itonami SS path.

  L0 enroll → disclosure → L4 multi-gen floors → book → G2 headroom
  → optional L6 tenure → G7 package → scorecard facts.
  write-all! optionally refreshes public/ package (Pages plan-only, never deploys).
  live=false throughout. cash≡0. Portable .cljc (bb + nbb; ADR-2607173000)."
  (:require [fuchi.methods.displacement-l0-path :as dl0]
            [fuchi.methods.displacement-tenure :as ten]
            [fuchi.methods.displacement-scorecard :as sc]
            [fuchi.methods.pipeline-audit-ledger :as audit]
            [fuchi.methods.displacement-gov :as dgov]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.pages-deploy :as pages-dep]
            [fuchi.methods.itonami-surplus-ledger :as surplus]
            [fuchi.methods.edn :as edn]
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

#?(:cljs
   (do
     (def ^:private fs (js/require "node:fs"))
     (def ^:private path (js/require "node:path"))))

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

(defn run!
  "Run full offline pipeline. Options: max-slots, climb-steps (L4=4), tenure-target (L6).
   Portable under bb and nbb."
  [& {:keys [max-slots climb-steps tenure-target include-tenure]
      :or {max-slots 2 climb-steps 4 tenure-target "L6" include-tenure true}}]
  (let [seed (itonami/load-itonami-seed-file)
        events (itonami/load-itonami-batch seed)
        batch (dl0/run-from-itonami-seed seed :max-slots max-slots :climb-steps climb-steps)
        batch2 (if include-tenure
                 (ten/run-batch-with-tenure batch events :target-stage tenure-target)
                 batch)
        batch3 (dgov/package-batch batch2)
        scorecard (sc/build batch3)
        out {:pipeline "displacement-ss-offline"
                :live false
                :cash-usd-micros 0
                :score-surface []
                :priority-stack PRIORITY-STACK
                :batch batch3
                :scorecard scorecard
                :gov-route-counts (:gov-route-counts batch3)
                :admissible-cohorts (:scorecard/admissible-cohorts scorecard)
                :tenure-subjects (:scorecard/tenure-subjects scorecard)
                :displacement-held-stress-subjects
                (or (:scorecard/displacement-held-stress-subjects scorecard) 0)
                :displacement-held-stress-ladder-refused
                (or (:scorecard/displacement-held-stress-ladder-refused scorecard) 0)
                :tenure-held-stress-subjects
                (or (:scorecard/tenure-held-stress-subjects scorecard) 0)
                :tenure-held-stress-ladder-refused
                (or (:scorecard/tenure-held-stress-ladder-refused scorecard) 0)
                :tenure-held-stress-carried
                (or (:scorecard/tenure-held-stress-carried scorecard) 0)
                :gov-held-stress-subjects
                (or (:scorecard/gov-held-stress-subjects scorecard) 0)
                :gov-held-stress-ladder-refused
                (or (:scorecard/gov-held-stress-ladder-refused scorecard) 0)
                :tenure-gov-held-stress-subjects
                (or (:scorecard/tenure-gov-held-stress-subjects scorecard) 0)
                :tenure-gov-held-stress-ladder-refused
                (or (:scorecard/tenure-gov-held-stress-ladder-refused scorecard) 0)
                :all-live-refused (:scorecard/all-live-refused scorecard)
                ;; surface G7 package facts at top-level (facts only)
                :gov-flowable-committed-usd-micros
                (or (:scorecard/gov-flowable-committed-usd-micros scorecard) 0)
                :gov-post-ratify-committed-usd-micros
                (or (:scorecard/gov-post-ratify-committed-usd-micros scorecard) 0)
                :tenure-gov-flowable-committed-usd-micros
                (or (:scorecard/tenure-gov-flowable-committed-usd-micros scorecard) 0)
                :tenure-gov-post-ratify-committed-usd-micros
                (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros scorecard) 0)
                :housing-land-grant-executed
                (or (:scorecard/housing-land-grant-executed scorecard) 0)
                :housing-council-held
                (or (:scorecard/housing-council-held scorecard) 0)
                :r2-status-count (or (:scorecard/r2-status-count scorecard) 0)
                :r2-refused (or (:scorecard/r2-refused scorecard) 0)
                :r2-executed (or (:scorecard/r2-executed scorecard) 0)
                :all-r2-not-executed
                (boolean (or (:scorecard/all-r2-not-executed scorecard)
                             (zero? (or (:scorecard/r2-executed scorecard) 0))))
                :ss-priority-path (or (:scorecard/ss-priority-path scorecard) {})
                :ss-all-rails-gated-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :all-rails-gated-refused]
                                 true))
                :ss-all-r2-not-executed
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :all-r2-not-executed]
                                 true))
                :ss-ladder-to
                (or (get-in scorecard [:scorecard/ss-priority-path :ladder-to]) "n/a")
                :ss-l0-disclosure-state
                (or (get-in scorecard [:scorecard/ss-priority-path :l0-disclosure-state]) "n/a")
                :ss-l0-disclosure-held
                (boolean (get-in scorecard [:scorecard/ss-priority-path :l0-disclosure-held]))
                :ss-l0-entitlements-may-flow
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :l0-entitlements-may-flow]
                                 true))
                :ss-l0-path
                (or (get-in scorecard [:scorecard/ss-priority-path :l0-path])
                    "l0-enroll-offline")
                :l0-all-seven-membrane-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-all-seven-enroll
                                  :all-seven-rails-receive-membrane-refused]
                                 true))
                :l0-all-seven-all-inkind-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-all-seven-enroll
                                  :all-inkind-produce-rails-full-chain-refused]
                                 true))
                :l0-all-seven-liquidity-receive-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-all-seven-enroll
                                  :liquidity-receive-full-chain-refused]
                                 true))
                :l0-all-seven-liquidity-loan-executed
                (boolean (get-in scorecard
                                 [:scorecard/l0-all-seven-enroll
                                  :liquidity-loan-executed]))
                :l0-all-seven-liquidity-cash-usd-micros
                (or (get-in scorecard
                            [:scorecard/l0-all-seven-enroll
                             :liquidity-cash-usd-micros])
                    0)
                :l0-all-seven-continuity-final-state
                (or (get-in scorecard
                            [:scorecard/l0-all-seven-enroll :continuity-final-state])
                    "n/a")
                :l0-all-seven-continuity-held-steps
                (or (get-in scorecard
                            [:scorecard/l0-all-seven-enroll :continuity-held-steps])
                    0)
                :l0-all-seven-ladder-advance-phase
                (or (get-in scorecard
                            [:scorecard/l0-all-seven-enroll :ladder-advance-phase])
                    "n/a")
                :l0-all-seven-ladder-advance-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-all-seven-enroll
                                  :ladder-advance-refused]))
                :l0-held-all-seven-membrane-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-held-all-seven-enroll
                                  :all-seven-rails-receive-membrane-refused]
                                 true))
                :l0-held-all-seven-disclosure-held
                (boolean (get-in scorecard
                                 [:scorecard/l0-held-all-seven-enroll
                                  :disclosure-held]
                                 true))
                :l0-held-all-seven-entitlements-may-flow
                (boolean (get-in scorecard
                                 [:scorecard/l0-held-all-seven-enroll
                                  :entitlements-may-flow]))
                :l0-held-all-seven-ladder-advance-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-held-all-seven-enroll
                                  :ladder-advance-refused]
                                 true))
                :l0-held-all-seven-ladder-advance-phase
                (or (get-in scorecard
                            [:scorecard/l0-held-all-seven-enroll
                             :ladder-advance-phase])
                    "n/a")
                :l0-exit-state
                (or (get-in scorecard [:scorecard/l0-exit-reaffirm :exit-state]) "n/a")
                :l0-exit-suspended
                (boolean (get-in scorecard
                                 [:scorecard/l0-exit-reaffirm :exit-suspended?]))
                :l0-exit-entitlements-may-flow
                (boolean (get-in scorecard
                                 [:scorecard/l0-exit-reaffirm
                                  :exit-entitlements-may-flow]))
                :l0-exit-ladder-phase
                (or (get-in scorecard
                            [:scorecard/l0-exit-reaffirm :exit-ladder-phase])
                    "n/a")
                :l0-exit-ladder-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-exit-reaffirm :exit-ladder-refused]
                                 true))
                :l0-reaffirm-state
                (or (get-in scorecard
                            [:scorecard/l0-exit-reaffirm :reaffirm-state])
                    "n/a")
                :l0-reaffirm-exit-suspended
                (boolean (get-in scorecard
                                 [:scorecard/l0-exit-reaffirm
                                  :reaffirm-exit-suspended?]))
                :l0-reaffirm-entitlements-may-flow
                (boolean (get-in scorecard
                                 [:scorecard/l0-exit-reaffirm
                                  :reaffirm-entitlements-may-flow]
                                 true))
                :l0-reaffirm-ladder-phase
                (or (get-in scorecard
                            [:scorecard/l0-exit-reaffirm :reaffirm-ladder-phase])
                    "n/a")
                :l0-reaffirm-ladder-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-exit-reaffirm
                                  :reaffirm-ladder-refused]))
                :l0-falsehood-held
                (boolean (get-in scorecard
                                 [:scorecard/l0-falsehood-lift :falsehood-held?]
                                 true))
                :l0-falsehood-ladder-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-falsehood-lift
                                  :falsehood-ladder-refused]
                                 true))
                :l0-lift-state
                (or (get-in scorecard [:scorecard/l0-falsehood-lift :lift-state])
                    "n/a")
                :l0-lift-ladder-phase
                (or (get-in scorecard
                            [:scorecard/l0-falsehood-lift :lift-ladder-phase])
                    "n/a")
                :l0-lift-ladder-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-falsehood-lift
                                  :lift-ladder-refused]))
                :l0-care-first-both-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-care-first-mitsuho
                                  :care-mitsuho-both-refused]
                                 true))
                :l0-care-first-ladder-phase
                (or (get-in scorecard
                            [:scorecard/l0-care-first-mitsuho
                             :ladder-advance-phase])
                    "n/a")
                :l0-care-first-ladder-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-care-first-mitsuho
                                  :ladder-advance-refused]))
                :l0-care-first-hikari-both-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-care-first-hikari
                                  :care-hikari-both-refused]
                                 true))
                :l0-care-first-hikari-ladder-phase
                (or (get-in scorecard
                            [:scorecard/l0-care-first-hikari
                             :ladder-advance-phase])
                    "n/a")
                :l0-care-first-hikari-ladder-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-care-first-hikari
                                  :ladder-advance-refused]))
                :l0-care-housing-both-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-care-housing-first
                                  :care-housing-both-refused]
                                 true))
                :l0-care-housing-land-grant-executed
                (boolean (get-in scorecard
                                 [:scorecard/l0-care-housing-first
                                  :land-grant-executed]))
                :l0-care-housing-ladder-phase
                (or (get-in scorecard
                            [:scorecard/l0-care-housing-first
                             :ladder-advance-phase])
                    "n/a")
                :l0-care-housing-ladder-refused
                (boolean (get-in scorecard
                                 [:scorecard/l0-care-housing-first
                                  :ladder-advance-refused]))
                :ss-stage-rails-first
                (or (get-in scorecard [:scorecard/ss-priority-path :stage-rails-first]) "n/a")
                :ss-stage-all-gated-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-all-gated-refused]
                                 true))
                :ss-stage-r2-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-r2-all-refused]
                                 true))
                :ss-stage-care-gated-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-care-gated-admissible]))
                :ss-stage-mitsuho-gated-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-mitsuho-gated-admissible]))
                :ss-stage-hikari-gated-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-hikari-gated-admissible]))
                :ss-stage-land-grant-executed
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path :stage-land-grant-executed]))
                :ss-mitsuho-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-gated-receive-admissible]))
                :ss-hikari-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :hikari-gated-receive-admissible]))
                :ss-care-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-gated-receive-admissible]))
                :ss-mitsuho-hikari-receive-both-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-hikari-receive-both-refused]
                                 true))
                :ss-care-mitsuho-hikari-receive-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-mitsuho-hikari-receive-all-refused]
                                 true))
                :ss-mitsuho-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-gated-produce-admissible]))
                :ss-hikari-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :hikari-gated-produce-admissible]))
                :ss-mitsuho-hikari-produce-both-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-hikari-produce-both-refused]
                                 true))
                :ss-mitsuho-hikari-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :mitsuho-hikari-full-chain-refused]
                                 true))
                :ss-care-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-gated-produce-admissible]))
                :ss-care-mitsuho-hikari-produce-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-mitsuho-hikari-produce-all-refused]
                                 true))
                :ss-care-mitsuho-hikari-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-mitsuho-hikari-full-chain-refused]
                                 true))
                :ss-housing-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :housing-gated-receive-admissible]))
                :ss-housing-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :housing-gated-produce-admissible]))
                :ss-housing-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :housing-full-chain-refused]
                                 true))
                :ss-care-housing-mitsuho-hikari-receive-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-housing-mitsuho-hikari-receive-all-refused]
                                 true))
                :ss-care-housing-mitsuho-hikari-produce-all-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-housing-mitsuho-hikari-produce-all-refused]
                                 true))
                :ss-care-housing-mitsuho-hikari-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :care-housing-mitsuho-hikari-full-chain-refused]
                                 true))
                :ss-tooling-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :tooling-gated-receive-admissible]))
                :ss-tooling-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :tooling-gated-produce-admissible]))
                :ss-tooling-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :tooling-full-chain-refused]
                                 true))
                :ss-compute-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :compute-gated-receive-admissible]))
                :ss-compute-gated-produce-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :compute-gated-produce-admissible]))
                :ss-compute-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :compute-full-chain-refused]
                                 true))
                :ss-tooling-compute-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :tooling-compute-full-chain-refused]
                                 true))
                :ss-all-inkind-produce-rails-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :all-inkind-produce-rails-full-chain-refused]
                                 true))
                :ss-liquidity-gated-receive-admissible
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :liquidity-gated-receive-admissible]))
                :ss-liquidity-receive-full-chain-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :liquidity-receive-full-chain-refused]
                                 true))
                :ss-all-seven-rails-receive-membrane-refused
                (boolean (get-in scorecard
                                 [:scorecard/ss-priority-path
                                  :all-seven-rails-receive-membrane-refused]
                                 true))
                ;; Displacement→L0 subject membranes (parity with scorecard aggregates)
                :displacement-membrane-subjects
                (or (:scorecard/displacement-membrane-subjects scorecard) 0)
                :displacement-care-housing-full-chain-refused
                (or (:scorecard/displacement-care-housing-full-chain-refused scorecard) 0)
                :displacement-all-inkind-full-chain-refused
                (or (:scorecard/displacement-all-inkind-full-chain-refused scorecard) 0)
                :displacement-all-seven-receive-membrane-refused
                (or (:scorecard/displacement-all-seven-receive-membrane-refused scorecard) 0)
                :displacement-liquidity-recv-refused
                (or (:scorecard/displacement-liquidity-recv-refused scorecard) 0)}]
       (pp/assert-no-public-scores!
        (select-keys out [:live :cash-usd-micros :score-surface :priority-stack
                          :admissible-cohorts :tenure-subjects :all-live-refused
                          :gov-flowable-committed-usd-micros
                          :gov-post-ratify-committed-usd-micros
                          :housing-land-grant-executed
                          :r2-executed :r2-refused :all-r2-not-executed
                          :ss-all-rails-gated-refused :ss-all-r2-not-executed
                          :ss-stage-all-gated-refused :ss-stage-r2-all-refused
                          :ss-stage-land-grant-executed
                          :ss-stage-care-gated-admissible
                          :ss-stage-mitsuho-gated-admissible
                          :ss-stage-hikari-gated-admissible
                          :ss-mitsuho-gated-receive-admissible
                          :ss-hikari-gated-receive-admissible
                          :ss-care-gated-receive-admissible
                          :ss-mitsuho-hikari-receive-both-refused
                          :ss-care-mitsuho-hikari-receive-all-refused
                          :ss-mitsuho-gated-produce-admissible
                          :ss-hikari-gated-produce-admissible
                          :ss-mitsuho-hikari-produce-both-refused
                          :ss-mitsuho-hikari-full-chain-refused
                          :ss-care-gated-produce-admissible
                          :ss-care-mitsuho-hikari-produce-all-refused
                          :ss-care-mitsuho-hikari-full-chain-refused
                          :ss-housing-gated-receive-admissible
                          :ss-housing-gated-produce-admissible
                          :ss-housing-full-chain-refused
                          :ss-care-housing-mitsuho-hikari-receive-all-refused
                          :ss-care-housing-mitsuho-hikari-produce-all-refused
                          :ss-care-housing-mitsuho-hikari-full-chain-refused
                          :ss-tooling-gated-receive-admissible
                          :ss-tooling-gated-produce-admissible
                          :ss-tooling-full-chain-refused
                          :ss-compute-gated-receive-admissible
                          :ss-compute-gated-produce-admissible
                          :ss-compute-full-chain-refused
                          :ss-tooling-compute-full-chain-refused
                          :ss-all-inkind-produce-rails-full-chain-refused
                          :ss-liquidity-gated-receive-admissible
                          :ss-liquidity-receive-full-chain-refused
                          :ss-all-seven-rails-receive-membrane-refused
                          :ss-l0-disclosure-held
                          :ss-l0-entitlements-may-flow
                          :l0-all-seven-membrane-refused
                          :l0-all-seven-all-inkind-refused
                          :l0-all-seven-liquidity-receive-refused
                          :l0-all-seven-liquidity-loan-executed
                          :l0-all-seven-liquidity-cash-usd-micros
                          :l0-all-seven-ladder-advance-refused
                          :l0-held-all-seven-membrane-refused
                          :l0-held-all-seven-disclosure-held
                          :l0-held-all-seven-entitlements-may-flow
                          :l0-held-all-seven-ladder-advance-refused
                          :l0-exit-suspended
                          :l0-exit-entitlements-may-flow
                          :l0-exit-ladder-refused
                          :l0-reaffirm-exit-suspended
                          :l0-reaffirm-entitlements-may-flow
                          :l0-reaffirm-ladder-refused
                          :l0-falsehood-held
                          :l0-falsehood-ladder-refused
                          :l0-lift-ladder-refused
                          :l0-care-first-both-refused
                          :l0-care-first-ladder-refused
                          :l0-care-first-hikari-both-refused
                          :l0-care-first-hikari-ladder-refused
                          :l0-care-housing-both-refused
                          :l0-care-housing-land-grant-executed
                          :l0-care-housing-ladder-refused
                          :displacement-membrane-subjects
                          :displacement-all-inkind-full-chain-refused
                          :displacement-all-seven-receive-membrane-refused
                          :displacement-liquidity-recv-refused]))
    out))

(defn write-all!
  "Run pipeline + write scorecard + append audit + optionally refresh public/ package.

  Options (in addition to run!):
    :include-public — default true; calls pages-deploy/write-deploy-package!
      (static package only; deployed=false; never wrangler/API).
    :deploy-env / :operator-did / :project-name — forwarded to deploy package writer.

  Never deploys. live=false. cash≡0. Portable under bb and nbb."
  [& {:keys [include-public deploy-env operator-did project-name]
      :or {include-public true}
      :as opts}]
  (let [opts (or opts {})
        run-opts (dissoc opts :include-public :deploy-env :operator-did :project-name)
        result (apply run! (mapcat identity run-opts))
        scard (:scorecard result)
        actor (actor-dir)
        outd (join-path actor "out")
        _ (ensure-dir! outd)
        md-path (join-path outd "displacement-scorecard.md")
        edn-path (join-path outd "displacement-scorecard.edn")
        _ (write-text! md-path (sc/scorecard-md scard))
        _ (write-text! edn-path (pr-str scard))
        paths {:md md-path :edn edn-path}
        audit-out (audit/append-from-pipeline! result)
        surplus-out (try (surplus/write-ledger!)
                         (catch #?(:clj Exception :cljs :default) e
                           {:error (or (ex-message e) (str e))
                            :cash-to-workers-usd-micros 0
                            :live false
                            :cash-usd-micros 0}))
        public-pkg (when include-public
                     (try
                       (pages-dep/write-deploy-package!
                        (cond-> {}
                          deploy-env (assoc :env deploy-env)
                          operator-did (assoc :operator-did operator-did)
                          project-name (assoc :project-name project-name)))
                       (catch #?(:clj Exception :cljs :default) e
                         {:error (or (ex-message e) (str e))
                          :deployed false
                          :live false
                          :cash-usd-micros 0
                          :package-ready false})))
        out (cond-> (assoc result
                           :paths paths
                           :audit audit-out
                           :itonami-surplus-ledger surplus-out
                           :live false
                           :cash-usd-micros 0
                           :score-surface []
                           :deployed false
                           :wrangler-invoked false
                           :package-ready (boolean (and include-public
                                                        public-pkg
                                                        (not (:error public-pkg))
                                                        (true? (:package-ready public-pkg true))))
                           :priority-stack PRIORITY-STACK)
              public-pkg (assoc :public-package public-pkg
                                :deploy-status (:deploy-status public-pkg)
                                :deployed (boolean (:deployed public-pkg false))
                                :wrangler-invoked (boolean (:wrangler-invoked public-pkg false))))]
    (pp/assert-no-public-scores!
     (select-keys out [:live :cash-usd-micros :score-surface :deployed
                       :package-ready :wrangler-invoked
                       :housing-land-grant-executed
                       :gov-post-ratify-committed-usd-micros]))
    (when-let [s (:summary surplus-out)]
      (when-not (zero? (long (or (:cash-to-workers-usd-micros s) 0)))
        (throw (ex-info "surplus ledger cash-to-workers must be 0" s))))
    out))
