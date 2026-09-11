(ns fuchi.methods.tooling-okaimono-produce-plan
  "tooling_okaimono_produce_plan.cljc — dry TOOLING PLAN for vocation recovery (R2 design).

  After dry-receive, plan tool-access units floor from imputed USD micros.
  Does NOT fulfill tools, live=false, cash≡0, no scores. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.tooling-okaimono-receive :as recv]
            [fuchi.methods.live-gate :as live-gate]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Accounting-only: ~$100/tool-access unit proxy → 0.01 units per USD.
(def UNITS-PER-USD-METHOD "v1-tool-access-proxy-illustrative")
(def UNITS-PER-USD 0.01)

(def MULTI-GEN-FACTS
  ["tool-units-floor-supports-household-vocation"
   "illustrative-conversion-not-a-score"
   "fulfillment-not-executed"
   "wellbecoming-substrate"])

(defn- micros->tool-units-yr [imputed-usd-micros-yr]
  (let [usd (/ (double imputed-usd-micros-yr) 1000000.0)]
    (long (Math/floor (* usd UNITS-PER-USD)))))

(defn dry-produce-plan
  [receive-ack]
  (when-not (#{:dry-ack :gated-ack-plan} (:phase receive-ack))
    (throw (ex-info "produce-plan requires dry-ack or gated-ack-plan" {:phase (:phase receive-ack)})))
  (when (:fulfillment-invoked receive-ack)
    (throw (ex-info "receive already claimed fulfillment — scaffold forbids" {})))
  (when-not (= 0 (:cash-usd-micros receive-ack))
    (throw (ex-info "cash≡0" {})))
  (let [imp (long (:imputed-usd-micros-yr receive-ack))
        units (micros->tool-units-yr imp)
        plan {:phase :dry-produce-plan
              :provider-did (:provider-did receive-ack)
              :rail-kind "tooling-okaimono"
              :alloc-id (:alloc-id receive-ack)
              :subject-did (:subject-did receive-ack)
              :imputed-usd-micros-yr imp
              :tool-units-floor-yr units
              :units-method UNITS-PER-USD-METHOD
              :produce-executed false
              :fulfillment-invoked false
              :fulfillment-executed false
              :published false
              :live false
              :cash-usd-micros 0
              :server-held-key false
              :priority-stack PRIORITY-STACK
              :multi-gen-facts MULTI-GEN-FACTS
              :score-surface []
              :note "dry tooling plan only — no okaimono fulfillment"}]
    (pp/assert-no-public-scores! plan)
    plan))

(defn plan-from-r1 [r1-pkg]
  (let [ack (recv/receive-from-r1-package r1-pkg)]
    (dry-produce-plan ack)))

(defn gated-produce-plan
  [r1-pkg gate & {:keys [env]}]
  (let [ack (recv/gated-receive-plan r1-pkg gate :env env)
        plan (dry-produce-plan ack)]
    (assoc plan
           :phase :gated-produce-plan
           :authorized-to-publish (boolean (:authorized-to-publish ack))
           :produce-executed false
           :fulfillment-executed false
           :live false
           :published false
           :note "gated plan authorized; okaimono fulfillment still not executed")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))

(defn gated-produce-status
  "Non-raising R1→gated-produce DESIGN for tooling-okaimono (vocation recovery).
   Default gate/env refuses. Never executes fulfillment; cash≡0; live=false."
  [r1-pkg & {:keys [gate env]}]
  (cond
    (nil? r1-pkg)
    nil

    (= :refused (:phase r1-pkg))
    (let [out {:rail-kind "tooling-okaimono"
               :phase :refused
               :r1-phase :refused
               :admissible false
               :authorized-to-publish false
               :fulfillment-executed false
               :produce-executed false
               :refusal-reason (or (:refusal-reason r1-pkg) "r1 refused")
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK
               :multi-gen-facts MULTI-GEN-FACTS
               :note "R1 refused — gated-produce not attempted"}]
      (pp/assert-no-public-scores! out)
      out)

    :else
    (let [g (or gate (live-gate/make-live-gate {:leg "provision"}))
          e (or env {})]
      (try
        (let [plan (gated-produce-plan r1-pkg g :env e)
              out {:rail-kind "tooling-okaimono"
                   :phase :gated-produce-plan
                   :r1-phase (:phase r1-pkg)
                   :admissible true
                   :authorized-to-publish (boolean (:authorized-to-publish plan))
                   :fulfillment-executed false
                   :produce-executed false
                   :tool-units-floor-yr (:tool-units-floor-yr plan)
                   :published false
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :multi-gen-facts MULTI-GEN-FACTS
                   :note "gated-produce plan authorized — okaimono fulfillment not executed"}]
          (pp/assert-no-public-scores! out)
          out)
        (catch #?(:clj Exception :cljs :default) ex
          (let [st (live-gate/gate-status g e)
                out {:rail-kind "tooling-okaimono"
                     :phase :refused
                     :r1-phase (:phase r1-pkg)
                     :admissible false
                     :authorized-to-publish false
                     :fulfillment-executed false
                     :produce-executed false
                     :refusal-reason (or (ex-message ex)
                                         (get st "reason")
                                         "live gate default refuse")
                     :gate-admissible (boolean (get st "admissible"))
                     :live false
                     :cash-usd-micros 0
                     :score-surface []
                     :priority-stack PRIORITY-STACK
                     :multi-gen-facts MULTI-GEN-FACTS
                     :note "R1 dry ok; gated-produce refused by default membrane"}]
            (pp/assert-no-public-scores! out)
            out))))))
