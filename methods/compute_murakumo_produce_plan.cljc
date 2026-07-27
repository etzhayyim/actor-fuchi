(ns fuchi.methods.compute-murakumo-produce-plan
  "compute_murakumo_produce_plan.cljc — dry COMPUTE PLAN for learning/vocation (R2 design).

  After dry-receive, plan GPU-hour floor from imputed USD micros.
  Does NOT allocate mesh quota, live=false, cash≡0, no scores. Portable .cljc."
  (:require [fuchi.methods.public-person :as pp]
            [fuchi.methods.compute-murakumo-receive :as recv]
            [fuchi.methods.live-gate :as live-gate]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

;; Accounting-only: ~$2/GPU-hr proxy → 0.5 hr per USD.
(def GPU-HR-PER-USD-METHOD "v1-mesh-gpu-proxy-illustrative")
(def GPU-HR-PER-USD 0.5)

(def MULTI-GEN-FACTS
  ["gpu-hours-floor-supports-learning-and-vocation"
   "illustrative-conversion-not-a-score"
   "mesh-quota-not-executed"
   "wellbecoming-substrate"])

(defn- micros->gpu-hours-yr [imputed-usd-micros-yr]
  (let [usd (/ (double imputed-usd-micros-yr) 1000000.0)]
    (long (Math/floor (* usd GPU-HR-PER-USD)))))

(defn dry-produce-plan
  [receive-ack]
  (when-not (#{:dry-ack :gated-ack-plan} (:phase receive-ack))
    (throw (ex-info "produce-plan requires dry-ack or gated-ack-plan" {:phase (:phase receive-ack)})))
  (when (:quota-invoked receive-ack)
    (throw (ex-info "receive already claimed quota — scaffold forbids" {})))
  (when-not (= 0 (:cash-usd-micros receive-ack))
    (throw (ex-info "cash≡0" {})))
  (let [imp (long (:imputed-usd-micros-yr receive-ack))
        hrs (micros->gpu-hours-yr imp)
        plan {:phase :dry-produce-plan
              :provider-did (:provider-did receive-ack)
              :rail-kind "compute-murakumo"
              :alloc-id (:alloc-id receive-ack)
              :subject-did (:subject-did receive-ack)
              :imputed-usd-micros-yr imp
              :gpu-hours-floor-yr hrs
              :gpu-hours-method GPU-HR-PER-USD-METHOD
              :produce-executed false
              :quota-invoked false
              :quota-executed false
              :published false
              :live false
              :cash-usd-micros 0
              :server-held-key false
              :priority-stack PRIORITY-STACK
              :multi-gen-facts MULTI-GEN-FACTS
              :score-surface []
              :note "dry compute plan only — no murakumo quota allocated"}]
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
           :quota-executed false
           :live false
           :published false
           :note "gated plan authorized; murakumo quota still not executed")))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))

(defn gated-produce-status
  "Non-raising R1→gated-produce DESIGN for compute-murakumo (learning/vocation).
   Default gate/env refuses. Never executes mesh quota; cash≡0; live=false."
  [r1-pkg & {:keys [gate env]}]
  (cond
    (nil? r1-pkg)
    nil

    (= :refused (:phase r1-pkg))
    (let [out {:rail-kind "compute-murakumo"
               :phase :refused
               :r1-phase :refused
               :admissible false
               :authorized-to-publish false
               :quota-executed false
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
              out {:rail-kind "compute-murakumo"
                   :phase :gated-produce-plan
                   :r1-phase (:phase r1-pkg)
                   :admissible true
                   :authorized-to-publish (boolean (:authorized-to-publish plan))
                   :quota-executed false
                   :produce-executed false
                   :gpu-hours-floor-yr (:gpu-hours-floor-yr plan)
                   :published false
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :multi-gen-facts MULTI-GEN-FACTS
                   :note "gated-produce plan authorized — murakumo quota not executed"}]
          (pp/assert-no-public-scores! out)
          out)
        (catch #?(:clj Exception :cljs :default) ex
          (let [st (live-gate/gate-status g e)
                out {:rail-kind "compute-murakumo"
                     :phase :refused
                     :r1-phase (:phase r1-pkg)
                     :admissible false
                     :authorized-to-publish false
                     :quota-executed false
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
