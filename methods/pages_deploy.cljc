(ns fuchi.methods.pages-deploy
  "pages_deploy.cljc — Cloudflare Pages deploy membrane for public/ SS surface.

  Default REFUSE (no deploy). Writes a deploy-ready package (wrangler.toml stub +
  status). Even when FUCHI_ALLOW_PAGES_DEPLOY=1 + operator attestation present,
  this scaffold only returns :gated-deploy-plan — it does NOT shell out to wrangler
  or call Cloudflare APIs (side-effect execute remains out of band).

  Public surface only (facts). cash≡0. no scores. Portable .cljc
  (bb + nbb write-deploy-package!; ADR-2607173000)."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.pages-publish :as pages]
            [fuchi.methods.pipeline-audit-ledger :as audit]
            [fuchi.methods.edn :as edn]
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)
(def FLAG "FUCHI_ALLOW_PAGES_DEPLOY")
(def DEFAULT-PROJECT "fuchi-public-surface")

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

(defn- write-text! [file-path content]
  #?(:clj (spit (io/file file-path) content)
     :cljs (.writeFileSync fs file-path (str content) "utf8")))

(defn operator-runbook-facts
  "Facts-only operator runbook for plan-only Pages packaging.
   Scaffold never deploys; wrangler/API remain out-of-band."
  ([]
   (operator-runbook-facts DEFAULT-PROJECT))
  ([project-name]
   (let [out {:flag FLAG
              :project-name project-name
              :deploy-target "cloudflare-pages"
              :required-for-gated-plan [FLAG "=1" "operator-did non-blank"]
              :scaffold-invokes-wrangler false
              :scaffold-invokes-cloudflare-api false
              :side-effect-execute "out-of-band only"
              :live-disbursement false
              :cash-usd-micros 0
              :deployed false
              :live false
              :score-surface []
              :priority-stack PRIORITY-STACK
              :steps ["write-deploy-package! → refresh public/ static facts"
                      "review index.html / facts.edn (no personal scores)"
                      (str "optional gated plan: " FLAG "=1 + operator-did"
                           " → phase=:gated-deploy-plan still deployed=false")
                      "out-of-band: wrangler pages deploy public/"
                      "never enable live sustenance disbursement from this package"]
              :note "plan-only membrane; actual deploy is operator out-of-band"}]
     (pp/assert-no-public-scores! out)
     out)))

(defn default-refuse-status
  "Bare env → not admissible."
  ([]
   (default-refuse-status {}))
  ([env]
   (let [admissible (= "1" (get env FLAG))
         reason (if admissible
                  "flag present — still requires operator plan; no auto side-effect"
                  (str "missing operator process flag '" FLAG "'"))]
     {"admissible" admissible
      "flag" FLAG
      "reason" reason
      "deployed" false
      "live" false})))

(defn refuse-deploy
  "Status map: deploy refused (default path)."
  ([]
   (refuse-deploy {}))
  ([env]
   (let [st (default-refuse-status env)
         out {:phase :refused
              :deploy-target "cloudflare-pages"
              :admissible (boolean (get st "admissible"))
              :refusal-reason (get st "reason")
              :authorized-to-deploy false
              :wrangler-invoked false
              :cloudflare-api-invoked false
              :package-ready true
              :operator-flag FLAG
              :operator-runbook (operator-runbook-facts)
              :deployed false
              :live false
              :cash-usd-micros 0
              :score-surface []
              :priority-stack PRIORITY-STACK
              :note "Pages deploy default refuse — static package only; plan-only membrane"}]
     (pp/assert-no-public-scores! (dissoc out :operator-runbook))
     (pp/assert-no-public-scores! (:operator-runbook out))
     out)))

(defn gated-deploy-plan
  "Authorize a deploy PLAN only. Does not deploy. Requires FUCHI_ALLOW_PAGES_DEPLOY=1
   and non-blank operator-did."
  [{:keys [operator-did project-name]
    :or {project-name DEFAULT-PROJECT}}
   & {:keys [env]}]
  (let [env (or env {})]
    (when-not (= "1" (get env FLAG))
      (throw (ex-info (str "missing " FLAG) {:flag FLAG})))
    (when (or (nil? operator-did) (zero? (count (str operator-did))))
      (throw (ex-info "missing operator-did attestation" {})))
    (let [out {:phase :gated-deploy-plan
               :deploy-target "cloudflare-pages"
               :project-name project-name
               :operator-did operator-did
               :operator-flag FLAG
               :authorized-to-deploy true
               :package-ready true
               :deployed false
               :wrangler-invoked false
               :cloudflare-api-invoked false
               :operator-runbook (operator-runbook-facts project-name)
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK
               :note "gated deploy plan only — wrangler/API not invoked by scaffold; OOB deploy"}]
      (pp/assert-no-public-scores! (dissoc out :operator-runbook))
      (pp/assert-no-public-scores! (:operator-runbook out))
      out)))

(defn gated-deploy-status
  "Non-raising R1-style status for Pages deploy DESIGN.
   Default env refuses. Never invokes wrangler/Cloudflare API."
  [opts & {:keys [env]}]
  (let [env (or env {})
        operator-did (:operator-did opts)
        project-name (or (:project-name opts) DEFAULT-PROJECT)]
    (try
      (let [plan (gated-deploy-plan
                  {:operator-did operator-did :project-name project-name}
                  :env env)
            out (assoc plan
                       :admissible true
                       :score-surface []
                       :priority-stack PRIORITY-STACK)]
        (pp/assert-no-public-scores! (dissoc out :operator-runbook))
        out)
      (catch #?(:clj Exception :cljs :default) ex
        (let [st (default-refuse-status env)
              out {:phase :refused
                   :deploy-target "cloudflare-pages"
                   :project-name project-name
                   :operator-flag FLAG
                   :admissible false
                   :authorized-to-deploy false
                   :package-ready true
                   :deployed false
                   :wrangler-invoked false
                   :cloudflare-api-invoked false
                   :refusal-reason (or (ex-message ex) (get st "reason"))
                   :gate-admissible (boolean (get st "admissible"))
                   :operator-runbook (operator-runbook-facts project-name)
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :note "Pages deploy gated status — no side-effect; static package only"}]
          (pp/assert-no-public-scores! (dissoc out :operator-runbook))
          (pp/assert-no-public-scores! (:operator-runbook out))
          out)))))

(defn deploy-or-refuse
  "If flag+operator present → gated-deploy-plan; else refuse map."
  [opts & {:keys [env]}]
  (try
    (gated-deploy-plan opts :env env)
    (catch #?(:clj Exception :cljs :default) e
      (assoc (refuse-deploy env)
             :refusal-reason (or (ex-message e) (str e))
             :admissible false))))

(defn- audit-package-snapshot
  "Optional last-run audit facts for deploy-status package (facts only)."
  []
  (try
       (let [au (audit/summary)
             out {:runs (or (:runs au) 0)
                  :all-runs-live-refused (boolean (:all-runs-live-refused au true))
                  :any-land-grant-executed? (boolean (:any-land-grant-executed? au))
                  :last-run-gov-flowable-committed-usd-micros
                  (or (:last-run-gov-flowable-committed-usd-micros au) 0)
                  :last-run-gov-post-ratify-committed-usd-micros
                  (or (:last-run-gov-post-ratify-committed-usd-micros au) 0)
                  :last-run-tenure-gov-post-ratify-committed-usd-micros
                  (or (:last-run-tenure-gov-post-ratify-committed-usd-micros au) 0)
                  :last-run-housing-land-grant-executed
                  (or (:last-run-housing-land-grant-executed au) 0)
                  :last-run-ss-rails-gated-count
                  (or (:last-run-ss-rails-gated-count au) 0)
                  :last-run-ss-all-rails-gated-refused
                  (boolean (:last-run-ss-all-rails-gated-refused au true))
                  :last-run-ss-all-r2-not-executed
                  (boolean (:last-run-ss-all-r2-not-executed au true))
                  :last-run-ss-l0-published
                  (boolean (:last-run-ss-l0-published au))
                  :last-run-ss-l0-disclosure-state
                  (or (:last-run-ss-l0-disclosure-state au) "n/a")
                  :last-run-ss-l0-disclosure-held
                  (boolean (:last-run-ss-l0-disclosure-held au))
                  :last-run-ss-l0-entitlements-may-flow
                  (boolean (:last-run-ss-l0-entitlements-may-flow au true))
                  :last-run-ss-l0-path
                  (or (:last-run-ss-l0-path au) "l0-enroll-offline")
                  :last-run-ss-ladder-to
                  (or (:last-run-ss-ladder-to au) "n/a")
                  :last-run-ss-stage-rails-first
                  (or (:last-run-ss-stage-rails-first au) "n/a")
                  :last-run-ss-stage-rails-second
                  (or (:last-run-ss-stage-rails-second au) "n/a")
                  :last-run-ss-stage-gated-count
                  (or (:last-run-ss-stage-gated-count au) 0)
                  :last-run-ss-stage-all-gated-refused
                  (boolean (:last-run-ss-stage-all-gated-refused au true))
                  :last-run-ss-stage-r2-all-refused
                  (boolean (:last-run-ss-stage-r2-all-refused au true))
                  :last-run-ss-stage-care-gated-admissible
                  (boolean (:last-run-ss-stage-care-gated-admissible au))
                  :last-run-ss-stage-mitsuho-gated-admissible
                  (boolean (:last-run-ss-stage-mitsuho-gated-admissible au))
                  :last-run-ss-stage-hikari-gated-admissible
                  (boolean (:last-run-ss-stage-hikari-gated-admissible au))
                  :last-run-ss-stage-land-grant-executed
                  (boolean (:last-run-ss-stage-land-grant-executed au))
                  :last-run-ss-mitsuho-gated-receive-admissible
                  (boolean (:last-run-ss-mitsuho-gated-receive-admissible au))
                  :last-run-ss-hikari-gated-receive-admissible
                  (boolean (:last-run-ss-hikari-gated-receive-admissible au))
                  :last-run-ss-care-gated-receive-admissible
                  (boolean (:last-run-ss-care-gated-receive-admissible au))
                  :last-run-ss-mitsuho-hikari-receive-both-refused
                  (boolean (:last-run-ss-mitsuho-hikari-receive-both-refused au true))
                  :last-run-ss-care-mitsuho-hikari-receive-all-refused
                  (boolean (:last-run-ss-care-mitsuho-hikari-receive-all-refused au true))
                  :last-run-ss-mitsuho-gated-produce-admissible
                  (boolean (:last-run-ss-mitsuho-gated-produce-admissible au))
                  :last-run-ss-hikari-gated-produce-admissible
                  (boolean (:last-run-ss-hikari-gated-produce-admissible au))
                  :last-run-ss-care-gated-produce-admissible
                  (boolean (:last-run-ss-care-gated-produce-admissible au))
                  :last-run-ss-mitsuho-hikari-produce-both-refused
                  (boolean (:last-run-ss-mitsuho-hikari-produce-both-refused au true))
                  :last-run-ss-mitsuho-hikari-full-chain-refused
                  (boolean (:last-run-ss-mitsuho-hikari-full-chain-refused au true))
                  :last-run-ss-care-mitsuho-hikari-produce-all-refused
                  (boolean (:last-run-ss-care-mitsuho-hikari-produce-all-refused au true))
                  :last-run-ss-care-mitsuho-hikari-full-chain-refused
                  (boolean (:last-run-ss-care-mitsuho-hikari-full-chain-refused au true))
                  ;; multi-gen housing + vocation rails + seven-rail membrane (offline refuse)
                  :last-run-ss-housing-gated-receive-admissible
                  (boolean (:last-run-ss-housing-gated-receive-admissible au))
                  :last-run-ss-housing-gated-produce-admissible
                  (boolean (:last-run-ss-housing-gated-produce-admissible au))
                  :last-run-ss-housing-full-chain-refused
                  (boolean (:last-run-ss-housing-full-chain-refused au true))
                  :last-run-ss-care-housing-mitsuho-hikari-full-chain-refused
                  (boolean (:last-run-ss-care-housing-mitsuho-hikari-full-chain-refused au true))
                  :last-run-ss-tooling-gated-receive-admissible
                  (boolean (:last-run-ss-tooling-gated-receive-admissible au))
                  :last-run-ss-tooling-gated-produce-admissible
                  (boolean (:last-run-ss-tooling-gated-produce-admissible au))
                  :last-run-ss-tooling-full-chain-refused
                  (boolean (:last-run-ss-tooling-full-chain-refused au true))
                  :last-run-ss-compute-gated-receive-admissible
                  (boolean (:last-run-ss-compute-gated-receive-admissible au))
                  :last-run-ss-compute-gated-produce-admissible
                  (boolean (:last-run-ss-compute-gated-produce-admissible au))
                  :last-run-ss-compute-full-chain-refused
                  (boolean (:last-run-ss-compute-full-chain-refused au true))
                  :last-run-ss-tooling-compute-full-chain-refused
                  (boolean (:last-run-ss-tooling-compute-full-chain-refused au true))
                  :last-run-ss-all-inkind-produce-rails-full-chain-refused
                  (boolean (:last-run-ss-all-inkind-produce-rails-full-chain-refused au true))
                  :last-run-ss-liquidity-gated-receive-admissible
                  (boolean (:last-run-ss-liquidity-gated-receive-admissible au))
                  :last-run-ss-liquidity-receive-full-chain-refused
                  (boolean (:last-run-ss-liquidity-receive-full-chain-refused au true))
                  :last-run-ss-all-seven-rails-receive-membrane-refused
                  (boolean (:last-run-ss-all-seven-rails-receive-membrane-refused au true))
                  :last-run-displacement-membrane-subjects
                  (or (:last-run-displacement-membrane-subjects au) 0)
                  :last-run-displacement-held-stress-subjects
                  (or (:last-run-displacement-held-stress-subjects au) 0)
                  :last-run-displacement-held-stress-ladder-refused
                  (or (:last-run-displacement-held-stress-ladder-refused au) 0)
                  :last-run-tenure-held-stress-subjects
                  (or (:last-run-tenure-held-stress-subjects au) 0)
                  :last-run-tenure-held-stress-ladder-refused
                  (or (:last-run-tenure-held-stress-ladder-refused au) 0)
                  :last-run-tenure-held-stress-carried
                  (or (:last-run-tenure-held-stress-carried au) 0)
                  :last-run-gov-held-stress-subjects
                  (or (:last-run-gov-held-stress-subjects au) 0)
                  :last-run-gov-held-stress-ladder-refused
                  (or (:last-run-gov-held-stress-ladder-refused au) 0)
                  :last-run-tenure-gov-held-stress-subjects
                  (or (:last-run-tenure-gov-held-stress-subjects au) 0)
                  :last-run-tenure-gov-held-stress-ladder-refused
                  (or (:last-run-tenure-gov-held-stress-ladder-refused au) 0)
                  :last-run-displacement-all-inkind-full-chain-refused
                  (or (:last-run-displacement-all-inkind-full-chain-refused au) 0)
                  :last-run-displacement-all-seven-receive-membrane-refused
                  (or (:last-run-displacement-all-seven-receive-membrane-refused au) 0)
                  :last-run-displacement-liquidity-recv-refused
                  (or (:last-run-displacement-liquidity-recv-refused au) 0)
                  :last-run-l0-all-seven-membrane-refused
                  (boolean (:last-run-l0-all-seven-membrane-refused au true))
                  :last-run-l0-all-seven-all-inkind-refused
                  (boolean (:last-run-l0-all-seven-all-inkind-refused au true))
                  :last-run-l0-all-seven-liquidity-receive-refused
                  (boolean (:last-run-l0-all-seven-liquidity-receive-refused au true))
                  :last-run-l0-all-seven-disclosure-state
                  (or (:last-run-l0-all-seven-disclosure-state au) "n/a")
                  :last-run-l0-all-seven-liquidity-member-principal
                  (boolean (:last-run-l0-all-seven-liquidity-member-principal au true))
                  :last-run-l0-all-seven-liquidity-loan-executed
                  (boolean (:last-run-l0-all-seven-liquidity-loan-executed au))
                  :last-run-l0-all-seven-liquidity-cash-usd-micros
                  (or (:last-run-l0-all-seven-liquidity-cash-usd-micros au) 0)
                  :last-run-l0-all-seven-land-grant-executed
                  (boolean (:last-run-l0-all-seven-land-grant-executed au))
                  :last-run-l0-all-seven-continuity-final-state
                  (or (:last-run-l0-all-seven-continuity-final-state au) "n/a")
                  :last-run-l0-all-seven-continuity-held-steps
                  (or (:last-run-l0-all-seven-continuity-held-steps au) 0)
                  :last-run-l0-all-seven-ladder-advance-phase
                  (or (:last-run-l0-all-seven-ladder-advance-phase au) "n/a")
                  :last-run-l0-all-seven-ladder-advance-refused
                  (boolean (:last-run-l0-all-seven-ladder-advance-refused au))
                  :last-run-l0-held-all-seven-membrane-refused
                  (boolean (:last-run-l0-held-all-seven-membrane-refused au true))
                  :last-run-l0-held-all-seven-disclosure-held
                  (boolean (:last-run-l0-held-all-seven-disclosure-held au true))
                  :last-run-l0-held-all-seven-entitlements-may-flow
                  (boolean (:last-run-l0-held-all-seven-entitlements-may-flow au))
                  :last-run-l0-held-all-seven-ladder-advance-refused
                  (boolean (:last-run-l0-held-all-seven-ladder-advance-refused au true))
                  :last-run-l0-held-all-seven-ladder-advance-phase
                  (or (:last-run-l0-held-all-seven-ladder-advance-phase au) "n/a")
                  :last-run-l0-held-all-seven-liquidity-loan-executed
                  (boolean (:last-run-l0-held-all-seven-liquidity-loan-executed au))
                  :last-run-l0-held-all-seven-land-grant-executed
                  (boolean (:last-run-l0-held-all-seven-land-grant-executed au))
                  :last-run-l0-exit-state
                  (or (:last-run-l0-exit-state au) "n/a")
                  :last-run-l0-exit-suspended
                  (boolean (:last-run-l0-exit-suspended au))
                  :last-run-l0-exit-ladder-refused
                  (boolean (:last-run-l0-exit-ladder-refused au true))
                  :last-run-l0-reaffirm-state
                  (or (:last-run-l0-reaffirm-state au) "n/a")
                  :last-run-l0-reaffirm-exit-suspended
                  (boolean (:last-run-l0-reaffirm-exit-suspended au))
                  :last-run-l0-reaffirm-entitlements-may-flow
                  (boolean (:last-run-l0-reaffirm-entitlements-may-flow au true))
                  :last-run-l0-reaffirm-ladder-phase
                  (or (:last-run-l0-reaffirm-ladder-phase au) "n/a")
                  :last-run-l0-reaffirm-ladder-refused
                  (boolean (:last-run-l0-reaffirm-ladder-refused au))
                  :last-run-l0-falsehood-held
                  (boolean (:last-run-l0-falsehood-held au true))
                  :last-run-l0-falsehood-ladder-refused
                  (boolean (:last-run-l0-falsehood-ladder-refused au true))
                  :last-run-l0-lift-state
                  (or (:last-run-l0-lift-state au) "n/a")
                  :last-run-l0-lift-ladder-phase
                  (or (:last-run-l0-lift-ladder-phase au) "n/a")
                  :last-run-l0-lift-ladder-refused
                  (boolean (:last-run-l0-lift-ladder-refused au))
                  :last-run-l0-care-first-both-refused
                  (boolean (:last-run-l0-care-first-both-refused au true))
                  :last-run-l0-care-first-ladder-phase
                  (or (:last-run-l0-care-first-ladder-phase au) "n/a")
                  :last-run-l0-care-first-ladder-refused
                  (boolean (:last-run-l0-care-first-ladder-refused au))
                  :last-run-l0-care-first-held-stress-ladder-refused
                  (boolean (:last-run-l0-care-first-held-stress-ladder-refused au true))
                  :last-run-l0-care-first-hikari-both-refused
                  (boolean (:last-run-l0-care-first-hikari-both-refused au true))
                  :last-run-l0-care-first-hikari-ladder-phase
                  (or (:last-run-l0-care-first-hikari-ladder-phase au) "n/a")
                  :last-run-l0-care-first-hikari-ladder-refused
                  (boolean (:last-run-l0-care-first-hikari-ladder-refused au))
                  :last-run-l0-care-first-hikari-held-stress-ladder-refused
                  (boolean (:last-run-l0-care-first-hikari-held-stress-ladder-refused au true))
                  :last-run-l0-care-first-mitsuho-hikari-all-refused
                  (boolean (:last-run-l0-care-first-mitsuho-hikari-all-refused au true))
                  :last-run-l0-care-first-mitsuho-hikari-mitsuho-hikari-both-refused
                  (boolean (:last-run-l0-care-first-mitsuho-hikari-mitsuho-hikari-both-refused au true))
                  :last-run-l0-care-first-mitsuho-hikari-ladder-phase
                  (or (:last-run-l0-care-first-mitsuho-hikari-ladder-phase au) "n/a")
                  :last-run-l0-care-first-mitsuho-hikari-ladder-refused
                  (boolean (:last-run-l0-care-first-mitsuho-hikari-ladder-refused au))
                  :last-run-l0-care-first-mitsuho-hikari-held-stress-ladder-refused
                  (boolean (:last-run-l0-care-first-mitsuho-hikari-held-stress-ladder-refused au true))
                  :last-run-l0-care-housing-both-refused
                  (boolean (:last-run-l0-care-housing-both-refused au true))
                  :last-run-l0-care-housing-land-grant-executed
                  (boolean (:last-run-l0-care-housing-land-grant-executed au))
                  :last-run-l0-care-housing-ladder-phase
                  (or (:last-run-l0-care-housing-ladder-phase au) "n/a")
                  :last-run-l0-care-housing-ladder-refused
                  (boolean (:last-run-l0-care-housing-ladder-refused au))
                  :last-run-l0-care-housing-held-stress-ladder-refused
                  (boolean (:last-run-l0-care-housing-held-stress-ladder-refused au true))
                  :last-run-l0-multi-gen-substrate-all-refused
                  (boolean (:last-run-l0-multi-gen-substrate-all-refused au true))
                  :last-run-l0-multi-gen-substrate-care-housing-both-refused
                  (boolean (:last-run-l0-multi-gen-substrate-care-housing-both-refused au true))
                  :last-run-l0-multi-gen-substrate-mitsuho-hikari-both-refused
                  (boolean (:last-run-l0-multi-gen-substrate-mitsuho-hikari-both-refused au true))
                  :last-run-l0-multi-gen-substrate-land-grant-executed
                  (boolean (:last-run-l0-multi-gen-substrate-land-grant-executed au))
                  :last-run-l0-multi-gen-substrate-ladder-phase
                  (or (:last-run-l0-multi-gen-substrate-ladder-phase au) "n/a")
                  :last-run-l0-multi-gen-substrate-ladder-refused
                  (boolean (:last-run-l0-multi-gen-substrate-ladder-refused au))
                  :last-run-l0-multi-gen-substrate-held-stress-ladder-refused
                  (boolean (:last-run-l0-multi-gen-substrate-held-stress-ladder-refused au true))
                  :last-run-l0-full-inkind-all-refused
                  (boolean (:last-run-l0-full-inkind-all-refused au true))
                  :last-run-l0-full-inkind-tooling-compute-both-refused
                  (boolean (:last-run-l0-full-inkind-tooling-compute-both-refused au true))
                  :last-run-l0-full-inkind-land-grant-executed
                  (boolean (:last-run-l0-full-inkind-land-grant-executed au))
                  :last-run-l0-full-inkind-fulfillment-executed
                  (boolean (:last-run-l0-full-inkind-fulfillment-executed au))
                  :last-run-l0-full-inkind-quota-executed
                  (boolean (:last-run-l0-full-inkind-quota-executed au))
                  :last-run-l0-full-inkind-ladder-phase
                  (or (:last-run-l0-full-inkind-ladder-phase au) "n/a")
                  :last-run-l0-full-inkind-ladder-refused
                  (boolean (:last-run-l0-full-inkind-ladder-refused au))
                  :last-run-l0-full-inkind-held-stress-ladder-refused
                  (boolean (:last-run-l0-full-inkind-held-stress-ladder-refused au true))
                  :last-run-l0-vocation-recovery-both-refused
                  (boolean (:last-run-l0-vocation-recovery-both-refused au true))
                  :last-run-l0-vocation-recovery-fulfillment-executed
                  (boolean (:last-run-l0-vocation-recovery-fulfillment-executed au))
                  :last-run-l0-vocation-recovery-quota-executed
                  (boolean (:last-run-l0-vocation-recovery-quota-executed au))
                  :last-run-l0-vocation-recovery-ladder-phase
                  (or (:last-run-l0-vocation-recovery-ladder-phase au) "n/a")
                  :last-run-l0-vocation-recovery-ladder-refused
                  (boolean (:last-run-l0-vocation-recovery-ladder-refused au))
                  :last-run-l0-vocation-recovery-held-stress-ladder-refused
                  (boolean (:last-run-l0-vocation-recovery-held-stress-ladder-refused au true))
                  :last-run-l0-liquidity-residual-receive-refused
                  (boolean (:last-run-l0-liquidity-residual-receive-refused au true))
                  :last-run-l0-liquidity-residual-member-principal
                  (boolean (:last-run-l0-liquidity-residual-member-principal au true))
                  :last-run-l0-liquidity-residual-loan-executed
                  (boolean (:last-run-l0-liquidity-residual-loan-executed au))
                  :last-run-l0-liquidity-residual-cash-usd-micros
                  (or (:last-run-l0-liquidity-residual-cash-usd-micros au) 0)
                  :last-run-l0-liquidity-residual-ladder-phase
                  (or (:last-run-l0-liquidity-residual-ladder-phase au) "n/a")
                  :last-run-l0-liquidity-residual-ladder-refused
                  (boolean (:last-run-l0-liquidity-residual-ladder-refused au))
                  :last-run-l0-liquidity-residual-held-stress-ladder-refused
                  (boolean (:last-run-l0-liquidity-residual-held-stress-ladder-refused au true))
                  :last-run-l0-all-seven-substrate-all-inkind-refused
                  (boolean (:last-run-l0-all-seven-substrate-all-inkind-refused au true))
                  :last-run-l0-all-seven-substrate-membrane-refused
                  (boolean (:last-run-l0-all-seven-substrate-membrane-refused au true))
                  :last-run-l0-all-seven-substrate-loan-executed
                  (boolean (:last-run-l0-all-seven-substrate-loan-executed au))
                  :last-run-l0-all-seven-substrate-land-grant-executed
                  (boolean (:last-run-l0-all-seven-substrate-land-grant-executed au))
                  :last-run-l0-all-seven-substrate-ladder-phase
                  (or (:last-run-l0-all-seven-substrate-ladder-phase au) "n/a")
                  :last-run-l0-all-seven-substrate-ladder-refused
                  (boolean (:last-run-l0-all-seven-substrate-ladder-refused au))
                  :last-run-l0-all-seven-substrate-held-stress-ladder-refused
                  (boolean (:last-run-l0-all-seven-substrate-held-stress-ladder-refused au true))
                  :last-run-l0-priority-path-count
                  (or (:last-run-l0-priority-path-count au) 0)
                  :last-run-l0-priority-held-stress-embed-count
                  (or (:last-run-l0-priority-held-stress-embed-count au) 0)
                  :last-run-l0-priority-held-stress-embed-all
                  (boolean (:last-run-l0-priority-held-stress-embed-all au true))
                  :last-run-rail-design-rail-count
                  (or (:last-run-rail-design-rail-count au) 0)
                  :last-run-rail-design-ok-count
                  (or (:last-run-rail-design-ok-count au) 0)
                  :last-run-rail-design-live-produce-never
                  (boolean (:last-run-rail-design-live-produce-never au true))
                  :last-run-rail-design-all-cash-zero
                  (boolean (:last-run-rail-design-all-cash-zero au true))
                  :last-run-rail-design-all-live-false
                  (boolean (:last-run-rail-design-all-live-false au true))
                  :last-run-rail-design-all-seven
                  (boolean (:last-run-rail-design-all-seven au true))
                  :last-run-ss-care-live-produce
                  (boolean (:last-run-ss-care-live-produce au))
                  :last-run-ss-housing-live-produce
                  (boolean (:last-run-ss-housing-live-produce au))
                  :last-run-ss-care-care-first-api-path
                  (or (:last-run-ss-care-care-first-api-path au) "care-housing-first-path")
                  :last-run-ss-housing-care-first-api-path
                  (or (:last-run-ss-housing-care-first-api-path au) "care-housing-first-path")
                  :last-run-ss-mitsuho-live-produce
                  (boolean (:last-run-ss-mitsuho-live-produce au))
                  :last-run-ss-hikari-live-produce
                  (boolean (:last-run-ss-hikari-live-produce au))
                  :last-run-ss-tooling-live-produce
                  (boolean (:last-run-ss-tooling-live-produce au))
                  :last-run-ss-compute-live-produce
                  (boolean (:last-run-ss-compute-live-produce au))
                  :last-run-ss-liquidity-live-produce
                  (boolean (:last-run-ss-liquidity-live-produce au))
                  :last-run-ss-all-seven-design-embed-count
                  (or (:last-run-ss-all-seven-design-embed-count au) 7)
                  :last-run-ss-all-seven-design-live-produce-never
                  (boolean (:last-run-ss-all-seven-design-live-produce-never au true))
                  :last-run-l0-care-first-mitsuho-live-produce
                  (boolean (:last-run-l0-care-first-mitsuho-live-produce au))
                  :last-run-l0-care-first-hikari-live-produce
                  (boolean (:last-run-l0-care-first-hikari-live-produce au))
                  :last-run-l0-care-first-api-path
                  (or (:last-run-l0-care-first-api-path au) "care-first-mitsuho-path")
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK}]
         (pp/assert-no-public-scores! out)
         out)
       (catch #?(:clj Exception :cljs :default) _
         {:runs 0 :live false :cash-usd-micros 0 :score-surface []
          :any-land-grant-executed? false :all-runs-live-refused true
          :priority-stack PRIORITY-STACK})))

(defn write-deploy-package!
  "Refresh public/ via pages-publish + write wrangler.toml stub + deploy-status.edn
   + deploy-runbook.edn. Never deploys. Returns package map with deployed=false.
   Portable under bb and nbb."
  ([]
   (write-deploy-package! {}))
  ([{:keys [env operator-did project-name]
     :or {project-name DEFAULT-PROJECT}}]
   (let [actor (actor-dir)
         env (or env {})
         published (pages/write-pages!)
         status0 (deploy-or-refuse {:operator-did (or operator-did "")
                                    :project-name project-name}
                                   :env env)
         runbook (or (:operator-runbook status0) (operator-runbook-facts project-name))
         audit-snap (audit-package-snapshot)
         status (assoc status0
                       :operator-runbook runbook
                       :audit-snapshot audit-snap
                       :package-ready true
                       :package-dir "public"
                       :static-files ["index.html" "facts.edn" "scorecard.md"
                                      "scorecard.edn" "priority-stack-offline.edn"
                                      "audit-summary.edn"
                                      "deploy-status.edn" "deploy-runbook.edn"
                                      "wrangler.toml" "README.md"]
                       :wrangler-invoked false
                       :cloudflare-api-invoked false
                       :deployed false
                       :live false
                       :cash-usd-micros 0
                       :score-surface []
                       :priority-stack PRIORITY-STACK)
         pub (join-path actor "public")
         wrangler (str "name = \"" project-name "\"\n"
                       "compatibility_date = \"2026-07-17\"\n"
                       "pages_build_output_dir = \".\"\n"
                       "# Generated offline by fuchi.methods.pages-deploy\n"
                       "# Deploy is OUT OF BAND. Scaffold never invokes wrangler.\n"
                       "# cash≡0 live=false no personal scores\n")
         readme (str "# fuchi public surface (static)\n\n"
                        "Generated offline. cash≡0. live=false. No personal scores.\n"
                        "Priority: wellbecoming > mago > ko > present.\n"
                        "Includes displacement→L0 offline enroll + L6 tenure + audit.\n"
                        "SS rails: gated-receive/produce DESIGN default refuse"
                        " (care/housing/food/energy/tooling/compute + liquidity receive).\n\n"
                        "## Deploy membrane (plan-only)\n\n"
                        "- Default: refused (`" FLAG "` unset).\n"
                        "- Gated plan: flag=1 + operator-did → phase=:gated-deploy-plan,"
                        " still `deployed=false` (no wrangler/API here).\n"
                        "- Actual `wrangler pages deploy public/` is **operator out-of-band**.\n"
                        "- Do not enable live sustenance disbursement from this package.\n"
                        "- land-grant-executed stays 0 until Council-gated live path.\n\n"
                        "### Operator runbook steps\n\n"
                        (apply str (map #(str "1. " % "\n") (:steps runbook)))
                        "\n## Deploy status\n\n"
                        "- phase: " (:phase status) "\n"
                        "- authorized-to-deploy: " (boolean (:authorized-to-deploy status)) "\n"
                        "- package-ready: true\n"
                        "- wrangler-invoked: false\n"
                        "- cloudflare-api-invoked: false\n"
                        "- deployed: false\n"
                        "- live disbursement: never from this package\n"
                        "- last-run land-grant-executed: "
                        (or (:last-run-housing-land-grant-executed audit-snap) 0) "\n"
                        "- last-run all-inkind-produce-rails full-chain-refused: "
                        (boolean (:last-run-ss-all-inkind-produce-rails-full-chain-refused
                                  audit-snap true)) "\n"
                        "- last-run all-seven-rails receive-membrane-refused: "
                        (boolean (:last-run-ss-all-seven-rails-receive-membrane-refused
                                  audit-snap true)) "\n"
                        "- last-run L0 enroll disclosure open/held/may-flow: "
                        (or (:last-run-ss-l0-disclosure-state audit-snap) "n/a") "/"
                        (boolean (:last-run-ss-l0-disclosure-held audit-snap)) "/"
                        (boolean (:last-run-ss-l0-entitlements-may-flow audit-snap true))
                        " path=" (or (:last-run-ss-l0-path audit-snap) "l0-enroll-offline") "\n"
                        "- last-run L0 all-seven membrane/all-inkind/liq-recv: "
                        (boolean (:last-run-l0-all-seven-membrane-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-all-seven-all-inkind-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-all-seven-liquidity-receive-refused audit-snap true))
                        " loan=" (boolean (:last-run-l0-all-seven-liquidity-loan-executed audit-snap))
                        " cash=" (or (:last-run-l0-all-seven-liquidity-cash-usd-micros audit-snap) 0) "\n"
                        "- last-run L0 all-seven continuity/ladder: "
                        (or (:last-run-l0-all-seven-continuity-final-state audit-snap) "n/a") "/"
                        (or (:last-run-l0-all-seven-continuity-held-steps audit-snap) 0)
                        " ladder=" (or (:last-run-l0-all-seven-ladder-advance-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-all-seven-ladder-advance-refused audit-snap)) "\n"
                        "- last-run L0 held all-seven membrane/held/ladder-refused: "
                        (boolean (:last-run-l0-held-all-seven-membrane-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-held-all-seven-disclosure-held audit-snap true)) "/"
                        (boolean (:last-run-l0-held-all-seven-ladder-advance-refused audit-snap true))
                        " loan=" (boolean (:last-run-l0-held-all-seven-liquidity-loan-executed audit-snap)) "\n"
                        "- last-run L0 exit→re-affirm: exit="
                        (or (:last-run-l0-exit-state audit-snap) "n/a")
                        "/ladder-refused=" (boolean (:last-run-l0-exit-ladder-refused audit-snap true))
                        " reaffirm=" (or (:last-run-l0-reaffirm-state audit-snap) "n/a")
                        "/ladder=" (or (:last-run-l0-reaffirm-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-reaffirm-ladder-refused audit-snap)) "\n"
                        "- last-run L0 falsehood→lift: held/ladder-refused="
                        (boolean (:last-run-l0-falsehood-held audit-snap true)) "/"
                        (boolean (:last-run-l0-falsehood-ladder-refused audit-snap true))
                        " lift=" (or (:last-run-l0-lift-state audit-snap) "n/a")
                        "/ladder=" (or (:last-run-l0-lift-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-lift-ladder-refused audit-snap)) "\n"
                        "- last-run L0 care-first+mitsuho both-refused/ladder: "
                        (boolean (:last-run-l0-care-first-both-refused audit-snap true)) "/"
                        (or (:last-run-l0-care-first-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-care-first-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-care-first-held-stress-ladder-refused audit-snap true)) "\n"
                        "- last-run L0 care-first+hikari both-refused/ladder: "
                        (boolean (:last-run-l0-care-first-hikari-both-refused audit-snap true)) "/"
                        (or (:last-run-l0-care-first-hikari-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-care-first-hikari-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-care-first-hikari-held-stress-ladder-refused audit-snap true)) "\n"
                        "- last-run L0 care-first+mitsuho+hikari all-refused/ladder: "
                        (boolean (:last-run-l0-care-first-mitsuho-hikari-all-refused audit-snap true)) "/"
                        (or (:last-run-l0-care-first-mitsuho-hikari-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-care-first-mitsuho-hikari-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-care-first-mitsuho-hikari-held-stress-ladder-refused audit-snap true)) "\n"
                        "- last-run L0 care+housing both-refused/land-grant/ladder: "
                        (boolean (:last-run-l0-care-housing-both-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-care-housing-land-grant-executed audit-snap)) "/"
                        (or (:last-run-l0-care-housing-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-care-housing-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-care-housing-held-stress-ladder-refused audit-snap true)) "\n"
                        "- last-run L0 multi-gen substrate all-refused/land-grant/ladder: "
                        (boolean (:last-run-l0-multi-gen-substrate-all-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-multi-gen-substrate-land-grant-executed audit-snap)) "/"
                        (or (:last-run-l0-multi-gen-substrate-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-multi-gen-substrate-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-multi-gen-substrate-held-stress-ladder-refused audit-snap true)) "\n"
                        "- last-run L0 full-inkind six-rails all-refused/tooling+compute/ladder: "
                        (boolean (:last-run-l0-full-inkind-all-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-full-inkind-tooling-compute-both-refused audit-snap true)) "/"
                        (or (:last-run-l0-full-inkind-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-full-inkind-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-full-inkind-held-stress-ladder-refused audit-snap true))
                        " land-grant/fulfillment/quota="
                        (boolean (:last-run-l0-full-inkind-land-grant-executed audit-snap)) "/"
                        (boolean (:last-run-l0-full-inkind-fulfillment-executed audit-snap)) "/"
                        (boolean (:last-run-l0-full-inkind-quota-executed audit-snap)) "\n"
                        "- last-run L0 vocation recovery tooling+compute both-refused/ladder: "
                        (boolean (:last-run-l0-vocation-recovery-both-refused audit-snap true)) "/"
                        (or (:last-run-l0-vocation-recovery-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-vocation-recovery-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-vocation-recovery-held-stress-ladder-refused audit-snap true))
                        " fulfillment/quota="
                        (boolean (:last-run-l0-vocation-recovery-fulfillment-executed audit-snap)) "/"
                        (boolean (:last-run-l0-vocation-recovery-quota-executed audit-snap)) "\n"
                        "- last-run L0 liquidity residual receive-refused/member-principal/loan/ladder: "
                        (boolean (:last-run-l0-liquidity-residual-receive-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-liquidity-residual-member-principal audit-snap true)) "/"
                        (boolean (:last-run-l0-liquidity-residual-loan-executed audit-snap))
                        " cash=" (or (:last-run-l0-liquidity-residual-cash-usd-micros audit-snap) 0)
                        " ladder=" (or (:last-run-l0-liquidity-residual-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-liquidity-residual-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-liquidity-residual-held-stress-ladder-refused audit-snap true)) "\n"
                        "- last-run L0 all-seven substrate all-inkind/membrane/loan/land-grant/ladder: "
                        (boolean (:last-run-l0-all-seven-substrate-all-inkind-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-all-seven-substrate-membrane-refused audit-snap true)) "/"
                        (boolean (:last-run-l0-all-seven-substrate-loan-executed audit-snap)) "/"
                        (boolean (:last-run-l0-all-seven-substrate-land-grant-executed audit-snap))
                        " ladder=" (or (:last-run-l0-all-seven-substrate-ladder-phase audit-snap) "n/a")
                        "/refused=" (boolean (:last-run-l0-all-seven-substrate-ladder-refused audit-snap))
                        " held-stress-ladder-refused="
                        (boolean (:last-run-l0-all-seven-substrate-held-stress-ladder-refused audit-snap true)) "\n"
                        "- last-run L0 priority catalog path-count/held-stress-embed-count/embed-all: "
                        (or (:last-run-l0-priority-path-count audit-snap) 0) "/"
                        (or (:last-run-l0-priority-held-stress-embed-count audit-snap) 0) "/"
                        (boolean (:last-run-l0-priority-held-stress-embed-all audit-snap true)) "\n"
                        "- last-run rail DESIGN catalog rail-count/ok/live-produce-never/all-seven: "
                        (or (:last-run-rail-design-rail-count audit-snap) 0) "/"
                        (or (:last-run-rail-design-ok-count audit-snap) 0) "/"
                        (boolean (:last-run-rail-design-live-produce-never audit-snap true)) "/"
                        (boolean (:last-run-rail-design-all-seven audit-snap true))
                        " cash-zero=" (boolean (:last-run-rail-design-all-cash-zero audit-snap true))
                        " live-false=" (boolean (:last-run-rail-design-all-live-false audit-snap true)) "\n"
                        "- last-run care-first DESIGN mitsuho/hikari live-produce: "
                        (boolean (:last-run-l0-care-first-mitsuho-live-produce audit-snap)) "/"
                        (boolean (:last-run-l0-care-first-hikari-live-produce audit-snap))
                        " ss-care/housing="
                        (boolean (:last-run-ss-care-live-produce audit-snap)) "/"
                        (boolean (:last-run-ss-housing-live-produce audit-snap))
                        " ss-mitsuho/hikari="
                        (boolean (:last-run-ss-mitsuho-live-produce audit-snap)) "/"
                        (boolean (:last-run-ss-hikari-live-produce audit-snap))
                        " ss-tool/comp/liq="
                        (boolean (:last-run-ss-tooling-live-produce audit-snap)) "/"
                        (boolean (:last-run-ss-compute-live-produce audit-snap)) "/"
                        (boolean (:last-run-ss-liquidity-live-produce audit-snap))
                        " all-seven-embed="
                        (or (:last-run-ss-all-seven-design-embed-count audit-snap) 7)
                        "/live-produce-never="
                        (boolean (:last-run-ss-all-seven-design-live-produce-never audit-snap true))
                        "\n"
                        "- last-run displacement L0 membranes subjects/all-inkind/all-seven/liq-recv: "
                        (or (:last-run-displacement-membrane-subjects audit-snap) 0) "/"
                        (or (:last-run-displacement-all-inkind-full-chain-refused audit-snap) 0) "/"
                        (or (:last-run-displacement-all-seven-receive-membrane-refused audit-snap) 0) "/"
                        (or (:last-run-displacement-liquidity-recv-refused audit-snap) 0) "\n"
                        "- last-run displacement L0 held-stress subjects/ladder-refused: "
                        (or (:last-run-displacement-held-stress-subjects audit-snap) 0) "/"
                        (or (:last-run-displacement-held-stress-ladder-refused audit-snap) 0) "\n"
                        "- last-run tenure held-stress subjects/ladder-refused/carried-from-L0: "
                        (or (:last-run-tenure-held-stress-subjects audit-snap) 0) "/"
                        (or (:last-run-tenure-held-stress-ladder-refused audit-snap) 0) "/"
                        (or (:last-run-tenure-held-stress-carried audit-snap) 0) "\n"
                        "- last-run gov held-stress subjects/ladder-refused (L4 rows): "
                        (or (:last-run-gov-held-stress-subjects audit-snap) 0) "/"
                        (or (:last-run-gov-held-stress-ladder-refused audit-snap) 0) "\n"
                        "- last-run tenure-gov held-stress subjects/ladder-refused: "
                        (or (:last-run-tenure-gov-held-stress-subjects audit-snap) 0) "/"
                        (or (:last-run-tenure-gov-held-stress-ladder-refused audit-snap) 0) "\n")]
     (write-text! (join-path pub "wrangler.toml") wrangler)
     (write-text! (join-path pub "deploy-status.edn") (pr-str status))
     (write-text! (join-path pub "deploy-runbook.edn") (pr-str runbook))
     (write-text! (join-path pub "README.md") readme)
     (merge published
            {:wrangler (join-path pub "wrangler.toml")
             :deploy-status status
             :deploy-runbook (join-path pub "deploy-runbook.edn")
             :deployed false
             :wrangler-invoked false
             :cloudflare-api-invoked false
             :package-ready true
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :ss-all-seven-design-live-produce-never
             (boolean (:last-run-ss-all-seven-design-live-produce-never audit-snap true))}))))
