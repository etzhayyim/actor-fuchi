(ns fuchi.methods.l0-enroll
  "l0_enroll.cljc — offline 信者 Level 0 enrollment scaffold (§1.16.3a + ADR-2607177000).

  Path (dry-run only) — priority stack for covenantal SS offline:
    (1) draft-vow → triple-permanent → L0 entitlement → public-person facts
    (2) disclosure hold machine + continuity tick (stale → hold entitlements)
    (3) optional rail R1→gated DESIGN (care/housing first for 孫/子, then food/energy,
        tooling/compute vocation, liquidity residual member-principal)

  NEVER live: no SBT mint, no IPFS pin, no openmail, no cash (G2/G9/G10).
  Priority stack fact embedded: wellbecoming > mago > ko > present.
  Portable .cljc; pure fns."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.disclosure-continuity :as disc]
            [fuchi.methods.liberation-ladder :as ladder]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.care-iyashi-receive :as crecv]
            [fuchi.methods.care-iyashi-produce-plan :as cprod]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.housing-commons-receive :as housrecv]
            [fuchi.methods.housing-commons-produce-plan :as housprod]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.mitsuho-receive :as frecv]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.hikari-receive :as hrecv]
            [fuchi.methods.hikari-produce-plan :as hprod]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.tooling-okaimono-receive :as trecv]
            [fuchi.methods.tooling-okaimono-produce-plan :as tprod]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.compute-murakumo-receive :as mrecv]
            [fuchi.methods.compute-murakumo-produce-plan :as cmpprod]
            [fuchi.methods.rail-liquidity-warifu :as liquidity]
            [fuchi.methods.liquidity-warifu-receive :as wrecv]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(def L0-FLOOR-USD-MICROS-YR 300000000) ; $300/yr advisory+community (ADR-2605261000 L1 band as L0 floor proxy)
;; Default floors when attach scaffolds are used (illustrative offline).
(def DEFAULT-CARE-MICROS-YR 1000000000)
(def DEFAULT-HOUSING-MICROS-YR 6000000000)
(def DEFAULT-MITSUHO-MICROS-YR 2000000000)
(def DEFAULT-HIKARI-MICROS-YR 1500000000)
(def DEFAULT-TOOLING-MICROS-YR 500000000)
(def DEFAULT-COMPUTE-MICROS-YR 400000000)
(def DEFAULT-LIQUIDITY-MICROS-YR 1500000000)

(def MULTI-GEN-FACTS
  ["wellbecoming-over-mago-over-ko"
   "l0-entry-for-descendant-wellbecoming"
   "not-a-happiness-score"
   "cash-equiv-zero"])

(defn- ->str [v] (str (or v "")))

(defn- fnv1a64
  "Stable non-crypto content digest for offline CIDs (not security; reproducibility only)."
  [s]
  (let [s (str s)
        prime 1099511628211
        offset -3750763034362895579]
    (loop [i 0 h offset]
      (if (>= i (count s))
        #?(:clj (Long/toUnsignedString (long h) 16)
           :cljs (.toString (js/BigInt h) 16))
        (let [c #?(:clj (int (.charAt ^String s i))
                   :cljs (.charCodeAt s i))
              h' (bit-xor h c)
              h2 (unchecked-multiply h' prime)]
          (recur (inc i) h2))))))

(defn content-cid
  "Offline content-address stub: bafy-offline-<hex>."
  [s]
  (str "bafy-offline-" (fnv1a64 (str s))))

(defn assert-no-cash! [m]
  (when (and (contains? m :cash-usd-micros) (not= 0 (:cash-usd-micros m)))
    (throw (ex-info "cash≡0 INVARIANT (G2/N1): L0 enroll never disburses cash" m)))
  (when (and (contains? m :cashUsdMicros) (not= 0 (:cashUsdMicros m)))
    (throw (ex-info "cash≡0 INVARIANT (G2/N1)" m)))
  true)

(defn assert-no-server-key! [m]
  (when (or (true? (:server-held-key m)) (true? (:serverHeldKey m)))
    (throw (ex-info "no-server-key INVARIANT (G9)" m)))
  true)

(defn draft-vow
  "Draft a permanent commitment vow (pre-triple-commit). Coercion check: subject must self-sign intent."
  [{:keys [subject-did vow-text covenant member-signature multi-gen-note]
    :or {covenant "vowed" multi-gen-note "enroll-for-descendant-wellbecoming"}}]
  (when (str/blank? (str subject-did))
    (throw (ex-info "subject-did required" {})))
  (when (str/blank? (str vow-text))
    (throw (ex-info "vow-text required (metanoia/baptism/tokudo content)" {})))
  (when (str/blank? (str member-signature))
    (throw (ex-info "member-signature required (anti-coercion / no-server-key)" {})))
  (let [c (-> covenant str (str/replace #"^:" "") str/lower-case)]
    (when-not (contains? #{"outreach" "vowed"} c)
      (throw (ex-info (str "G4: covenant " c " unrepresentable") {:covenant c})))
    {:subject-did subject-did
     :vow-text vow-text
     :covenant c
     :member-signature member-signature
     :multi-gen-note multi-gen-note
     :priority-stack PRIORITY-STACK
     :phase :drafted
     :cash-usd-micros 0
     :server-held-key false
     :published false}))

(defn triple-permanent
  "Apply kotoba + IPFS + token content-address stubs. published stays false (offline)."
  [draft]
  (assert-no-cash! draft)
  (assert-no-server-key! draft)
  (when-not (= :drafted (:phase draft))
    (throw (ex-info "triple-permanent requires :drafted phase" {:phase (:phase draft)})))
  (let [body (str (:subject-did draft) "|" (:vow-text draft) "|" (:member-signature draft))
        kotoba (content-cid (str "kotoba|" body))
        ipfs (content-cid (str "ipfs|" body))
        token (str "sbt-offline-" (content-cid (str "sbt|" (:subject-did draft))))]
    (assoc draft
           :phase :committed-offline
           :stage "L0"
           :kotoba-cid kotoba
           :ipfs-cid ipfs
           :token-id token
           :covenant "vowed" ; L0 enroll seals vowed
           :published false
           :cash-usd-micros 0
           :server-held-key false)))

(defn l0-entitlement
  "Project L0 in-kind entitlement facts (advisory + community floor). No score, cash≡0.
  Rails are dry-run projections — not live provision.
  Default care rail is multi-gen advisory (wellbecoming > 孫 > 子 entry)."
  [committed]
  (when-not (= :committed-offline (:phase committed))
    (throw (ex-info "l0-entitlement requires :committed-offline" {:phase (:phase committed)})))
  (assert-no-cash! committed)
  {:subject-did (:subject-did committed)
   :stage "L0"
   :token-id (:token-id committed)
   :covenant "vowed"
   :floor-usd-micros-yr L0-FLOOR-USD-MICROS-YR
   :rails [{:kind "care" :active? true :note "multi-gen-advisory-L0-mago-ko"}]
   :cash-usd-micros 0
   :published false
   :server-held-key false
   :priority-stack PRIORITY-STACK
   :multi-gen-facts MULTI-GEN-FACTS
   :score-surface []
   :phase :l0-entitled})

(defn- default-disclosure []
  {:wage-labor-band "0-10h"
   :state-benefits? false
   :wellbecoming-attest-fact :submitted
   :related-party-edges []
   :rider-s2-self-report :none})

(defn enroll
  "Full offline L0 path (priorities 1+2):
   draft → triple → L0 entitlement → public-person facts
   → disclosure hold machine + continuity tick (stale → hold entitlements).
   Never live mint/dispatch. cash≡0. no scores."
  [opts]
  (let [draft (draft-vow opts)
        committed (triple-permanent draft)
        ent (l0-entitlement committed)
        disc-in (or (:disclosure opts) (default-disclosure))
        person0 {:did (:subject-did ent)
                 :covenant "vowed"
                 :rails (:rails ent)
                 :floor-usd-micros-yr (:floor-usd-micros-yr ent)
                 :stage "L0"
                 :exit-suspended? false
                 :disclosure disc-in
                 :multi-gen-care-facts (vec (distinct
                                             (concat [(:multi-gen-note committed)]
                                                     MULTI-GEN-FACTS)))
                 :cash-usd-micros 0}
        hold0 (dh/initial person0)
        cont (disc/tick hold0 person0
                        :disclosure disc-in
                        :reason "l0-enroll-continuity")
        hold (:machine cont)
        person (:person cont)
        gate (pp/disclosure-gate person)
        surface (pp/public-surface person
                                   :stage "L0"
                                   :disclosure-status (:action gate)
                                   :hold-reason (when (= :hold (:action gate)) (:reason gate)))
        out {:path "l0-enroll-offline"
             :vow committed
             :entitlement ent
             :public-person surface
             :disclosure-gate gate
             :disclosure-hold hold
             :disclosure-continuity cont
             :disclosure-state (:state hold)
             :disclosure-held? (boolean (:entitlements-held? hold))
             :entitlements-may-flow? (disc/entitlements-may-flow? hold)
             :person person
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS}]
    (pp/assert-no-public-scores! surface)
    (pp/assert-no-public-scores!
     (dissoc out :vow :entitlement :public-person :disclosure-gate
             :disclosure-hold :disclosure-continuity :person))
    out))

(def FRESH-DISC
  (default-disclosure))

(def STALE-DISC
  {:wage-labor-band :stale
   :state-benefits? false
   :wellbecoming-attest-fact :stale
   :related-party-edges []
   :rider-s2-self-report :none})

(defn apply-disclosure-tick
  "Priority (2): re-apply disclosure package on an enroll result (continuity tick).
   Updates hold/person/public-person/gate. Does not re-attach rail scaffolds.
   cash≡0. live=false. no scores."
  [enrolled disclosure & {:keys [reason]
                          :or {reason "l0-disclosure-continuity-tick"}}]
  (let [d (or disclosure FRESH-DISC)
        person0 (or (:person enrolled)
                    {:did (get-in enrolled [:vow :subject-did])
                     :covenant "vowed"
                     :rails (get-in enrolled [:entitlement :rails]
                                    [{:kind "care" :active? true}])
                     :floor-usd-micros-yr (or (get-in enrolled [:entitlement
                                                                :floor-usd-micros-yr])
                                              L0-FLOOR-USD-MICROS-YR)
                     :stage "L0"
                     :exit-suspended? false
                     :disclosure d
                     :cash-usd-micros 0})
        person0 (assoc person0 :disclosure d)
        hold0 (or (:disclosure-hold enrolled) (dh/initial person0))
        cont (disc/tick hold0 person0 :disclosure d :reason reason)
        hold (:machine cont)
        person (:person cont)
        gate (pp/disclosure-gate person)
        surface (pp/public-surface person
                                   :stage (or (:stage person) "L0")
                                   :disclosure-status (:action gate)
                                   :hold-reason (when (= :hold (:action gate))
                                                  (:reason gate)))
        out (assoc enrolled
                   :person person
                   :disclosure-hold hold
                   :disclosure-continuity cont
                   :disclosure-state (:state hold)
                   :disclosure-held? (boolean (:entitlements-held? hold))
                   :entitlements-may-flow? (disc/entitlements-may-flow? hold)
                   :disclosure-gate gate
                   :public-person surface
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! surface)
    out))

(defn continuity-stress
  "Priority (2) stress series: fresh → stale → fresh on enroll person.
   Returns series history + final open/held facts. Never live."
  [enrolled]
  (let [person (or (:person enrolled)
                   {:did (get-in enrolled [:vow :subject-did])
                    :covenant "vowed"
                    :rails [{:kind "care" :active? true}]
                    :floor-usd-micros-yr L0-FLOOR-USD-MICROS-YR
                    :disclosure FRESH-DISC
                    :stage "L0"
                    :exit-suspended? false
                    :cash-usd-micros 0})
        series (disc/tick-series person [FRESH-DISC STALE-DISC FRESH-DISC])
        hist (or (:history series) [])
        held-steps (count (filter :held? hist))
        out {:path "l0-disclosure-continuity-stress"
             :final-state (when-let [s (:final-state series)] (name s))
             :held-steps held-steps
             :history-count (count hist)
             :final-entitlements-may-flow
             (disc/entitlements-may-flow? (:machine series))
             :series series
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "continuity stress offline — open→held→open; no rail execute"}]
    (pp/assert-no-public-scores! (dissoc out :series :note))
    out))

(defn try-ladder-advance
  "One liberation-ladder offline step from enroll hold state.
   Held disclosure → phase :refused. Never mint. cash≡0."
  [enrolled & {:keys [member-signature]}]
  (let [person (or (:person enrolled)
                   {:did (get-in enrolled [:vow :subject-did])
                    :covenant "vowed"
                    :rails (get-in enrolled [:entitlement :rails]
                                   [{:kind "care" :active? true}])
                    :stage "L0"
                    :disclosure (or (get-in enrolled [:person :disclosure]) FRESH-DISC)
                    :exit-suspended? (boolean (:disclosure-held? enrolled))
                    :cash-usd-micros 0})
        hold (or (:disclosure-hold enrolled) (dh/initial person))
        sig (or member-signature
                (get-in enrolled [:vow :member-signature])
                (str "sig-" (:did person)))
        adv (ladder/advance-offline person hold :member-signature sig)
        out (assoc adv
                   :disclosure-state (when hold (name (:state hold)))
                   :entitlements-may-flow? (disc/entitlements-may-flow? hold)
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! (dissoc out :person :history))
    out))

(defn- refresh-enroll-surface
  "Rebuild public-person + gate from person + hold (shared by exit/re-affirm)."
  [enrolled person hold cont]
  (let [gate (pp/disclosure-gate person)
        surface (pp/public-surface person
                                   :stage (or (:stage person) "L0")
                                   :disclosure-status (:action gate)
                                   :hold-reason (when (= :hold (:action gate))
                                                  (:reason gate)))
        out (assoc enrolled
                   :person person
                   :disclosure-hold hold
                   :disclosure-continuity (or cont
                                              {:machine hold
                                               :person person
                                               :action :transitioned
                                               :held? (boolean (:entitlements-held? hold))
                                               :live false
                                               :cash-usd-micros 0
                                               :score-surface []
                                               :priority-stack PRIORITY-STACK})
                   :disclosure-state (:state hold)
                   :disclosure-held? (boolean (:entitlements-held? hold))
                   :entitlements-may-flow? (disc/entitlements-may-flow? hold)
                   :disclosure-gate gate
                   :public-person surface
                   :exit-suspended? (boolean (or (:exit-suspended? person)
                                                 (= :exit-suspended (:state hold))))
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! surface)
    out))

(defn exit-suspend
  "Priority (2): transition enroll hold to :exit-suspended (entitlements held;
   public-person? false on machine). Never live. cash≡0."
  [enrolled & {:keys [reason]
               :or {reason "l0-exit-suspend"}}]
  (let [person0 (or (:person enrolled)
                    {:did (get-in enrolled [:vow :subject-did])
                     :covenant "vowed"
                     :rails (get-in enrolled [:entitlement :rails]
                                    [{:kind "care" :active? true}])
                     :stage "L0"
                     :disclosure (or (get-in enrolled [:person :disclosure]) FRESH-DISC)
                     :exit-suspended? false
                     :cash-usd-micros 0})
        hold0 (or (:disclosure-hold enrolled) (dh/initial person0))
        hold (dh/transition hold0 :exit :person person0 :reason reason)
        person (assoc person0 :exit-suspended? true)]
    (refresh-enroll-surface enrolled person hold
                            {:machine hold :person person :action :exit
                             :to-state :exit-suspended :held? true
                             :reason reason :live false :cash-usd-micros 0
                             :score-surface [] :priority-stack PRIORITY-STACK})))

(defn re-affirm
  "Priority (2): re-affirm after exit-suspend with fresh disclosure.
   Opens hold when disclosure ok; public-person may return. Never live. cash≡0."
  [enrolled & {:keys [disclosure reason]
               :or {reason "l0-re-affirm"}}]
  (let [d (or disclosure FRESH-DISC)
        person0 (assoc (or (:person enrolled)
                           {:did (get-in enrolled [:vow :subject-did])
                            :covenant "vowed"
                            :rails (get-in enrolled [:entitlement :rails]
                                           [{:kind "care" :active? true}])
                            :stage "L0"
                            :cash-usd-micros 0})
                       :exit-suspended? false
                       :disclosure d)
        hold0 (or (:disclosure-hold enrolled) (dh/initial person0))
        hold (dh/transition hold0 :re-affirm :person person0 :disclosure d :reason reason)
        person (assoc person0
                      :exit-suspended? (= :exit-suspended (:state hold))
                      :disclosure d)]
    (refresh-enroll-surface enrolled person hold
                            {:machine hold :person person :action :re-affirm
                             :to-state (:state hold)
                             :held? (boolean (:entitlements-held? hold))
                             :reason reason :live false :cash-usd-micros 0
                             :score-surface [] :priority-stack PRIORITY-STACK})))

(defn exit-reaffirm-stress
  "Open enroll → exit-suspend → re-affirm(fresh). Facts-only summary.
   Ladder refused while exit-suspended; may advance after re-affirm."
  [enrolled & {:keys [member-signature]}]
  (let [sig (or member-signature
                (get-in enrolled [:vow :member-signature])
                "sig-exit-reaffirm")
        exited (exit-suspend enrolled :reason "exit-reaffirm-stress")
        lad-exit (try-ladder-advance exited :member-signature sig)
        restored (re-affirm exited :disclosure FRESH-DISC :reason "exit-reaffirm-stress")
        lad-restored (try-ladder-advance restored :member-signature sig)
        out {:path "l0-exit-reaffirm-stress"
             :exit-state (when-let [s (:disclosure-state exited)] (name s))
             :exit-suspended? (boolean (:exit-suspended? exited))
             :exit-entitlements-may-flow (boolean (:entitlements-may-flow? exited))
             :exit-ladder-phase (when-let [p (:phase lad-exit)] (name p))
             :exit-ladder-refused (boolean (= :refused (:phase lad-exit)))
             :reaffirm-state (when-let [s (:disclosure-state restored)] (name s))
             :reaffirm-exit-suspended? (boolean (:exit-suspended? restored))
             :reaffirm-entitlements-may-flow (boolean (:entitlements-may-flow? restored))
             :reaffirm-ladder-phase (when-let [p (:phase lad-restored)] (name p))
             :reaffirm-ladder-refused (boolean (= :refused (:phase lad-restored)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "exit→re-affirm offline stress — no mint/rail execute"}]
    (pp/assert-no-public-scores! (dissoc out :note))
    out))

(defn report-falsehood
  "Priority (2): open → held via :falsehood (Charter-Rider self-report path).
   Never live. cash≡0."
  [enrolled & {:keys [reason]
               :or {reason "l0-falsehood-hold"}}]
  (let [person0 (or (:person enrolled)
                    {:did (get-in enrolled [:vow :subject-did])
                     :covenant "vowed"
                     :rails (get-in enrolled [:entitlement :rails]
                                    [{:kind "care" :active? true}])
                     :stage "L0"
                     :disclosure (or (get-in enrolled [:person :disclosure]) FRESH-DISC)
                     :exit-suspended? false
                     :cash-usd-micros 0})
        hold0 (or (:disclosure-hold enrolled) (dh/initial person0))
        hold (dh/transition hold0 :falsehood :person person0 :reason reason)
        person (assoc person0 :disclosure (or (:disclosure person0) FRESH-DISC))]
    (refresh-enroll-surface enrolled person hold
                            {:machine hold :person person :action :falsehood
                             :to-state :held :held? true :reason reason
                             :live false :cash-usd-micros 0
                             :score-surface [] :priority-stack PRIORITY-STACK})))

(defn lift-hold
  "Priority (2): Council/operator lift-hold when disclosure fresh.
   Throws if disclosure not fresh (SM invariant). Never live. cash≡0."
  [enrolled & {:keys [disclosure reason]
               :or {reason "l0-lift-hold"}}]
  (let [d (or disclosure FRESH-DISC)
        person0 (assoc (or (:person enrolled)
                           {:did (get-in enrolled [:vow :subject-did])
                            :covenant "vowed"
                            :rails (get-in enrolled [:entitlement :rails]
                                           [{:kind "care" :active? true}])
                            :stage "L0"
                            :cash-usd-micros 0})
                       :disclosure d
                       :exit-suspended? false)
        hold0 (or (:disclosure-hold enrolled) (dh/initial person0))
        hold (dh/transition hold0 :lift-hold :person person0 :disclosure d :reason reason)
        person (assoc person0 :disclosure d)]
    (refresh-enroll-surface enrolled person hold
                            {:machine hold :person person :action :lift-hold
                             :to-state (:state hold)
                             :held? (boolean (:entitlements-held? hold))
                             :reason reason :live false :cash-usd-micros 0
                             :score-surface [] :priority-stack PRIORITY-STACK})))

(defn falsehood-lift-stress
  "Open enroll → falsehood hold → lift-hold(fresh). Ladder refused while held;
   may advance after lift. Facts-only. cash≡0."
  [enrolled & {:keys [member-signature]}]
  (let [sig (or member-signature
                (get-in enrolled [:vow :member-signature])
                "sig-falsehood-lift")
        held (report-falsehood enrolled :reason "falsehood-lift-stress")
        lad-held (try-ladder-advance held :member-signature sig)
        lifted (lift-hold held :disclosure FRESH-DISC :reason "falsehood-lift-stress")
        lad-lifted (try-ladder-advance lifted :member-signature sig)
        out {:path "l0-falsehood-lift-stress"
             :falsehood-state (when-let [s (:disclosure-state held)] (name s))
             :falsehood-held? (boolean (:disclosure-held? held))
             :falsehood-entitlements-may-flow (boolean (:entitlements-may-flow? held))
             :falsehood-ladder-phase (when-let [p (:phase lad-held)] (name p))
             :falsehood-ladder-refused (boolean (= :refused (:phase lad-held)))
             :lift-state (when-let [s (:disclosure-state lifted)] (name s))
             :lift-held? (boolean (:disclosure-held? lifted))
             :lift-entitlements-may-flow (boolean (:entitlements-may-flow? lifted))
             :lift-ladder-phase (when-let [p (:phase lad-lifted)] (name p))
             :lift-ladder-refused (boolean (= :refused (:phase lad-lifted)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "falsehood→lift-hold offline stress — no mint/rail execute"}]
    (pp/assert-no-public-scores! (dissoc out :note))
    out))

(defn- care-first-mitsuho-core
  "Internal: single disclosure path (open or held via opts :disclosure)."
  [opts]
  (let [e0 (enroll opts)
        e (-> e0
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-mitsuho-r1-scaffold
               :food-imputed-usd-micros-yr
               (or (:food-imputed-usd-micros-yr opts) DEFAULT-MITSUHO-MICROS-YR)))
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        mitsuho-design (mitsuho/design-public-facts)
        care-design (care/design-public-facts)
        out {:path "l0-care-first-mitsuho"
             :api "enroll+care+mitsuho+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :care-full-chain-refused (boolean (:care-full-chain-refused e))
             :mitsuho-full-chain-refused (boolean (:mitsuho-full-chain-refused e))
             :care-mitsuho-both-refused
             (boolean (and (:care-full-chain-refused e)
                           (:mitsuho-full-chain-refused e)))
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             ;; priority (3) DESIGN facts (care-first order; live-produce never)
             :care-design care-design
             :mitsuho-design mitsuho-design
             :care-first-api-path (or (:care-first-api-path mitsuho-design)
                                     "care-first-mitsuho-path")
             :care-first-before-rails (or (:care-first-before-rails mitsuho-design)
                                         ["care" "housing"])
             :mitsuho-live-produce (boolean (:live-produce mitsuho-design))
             :mitsuho-produce-executed (boolean (:produce-executed mitsuho-design))
             :care-delivery-executed (boolean (:care-delivery-executed care-design))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "care-first (孫/子) + mitsuho food — R1 gated DESIGN refuse; ladder offline"}]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :care-design :mitsuho-design
             :care-first-before-rails :care-first-api-path))
    out))

(defn care-first-mitsuho-path
  "Priority stack slice: (1) enroll (2) disclosure open (3) care then mitsuho membranes
   + ladder. Care-first for 孫/子; both rails default refuse offline. cash≡0.
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (care-first-mitsuho-core opts)
        held (when include-held?
               (care-first-mitsuho-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :care-mitsuho-both-refused (boolean (:care-mitsuho-both-refused held))
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — membranes refuse; ladder refused"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-both-refused
                                 (boolean (:care-mitsuho-both-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(defn- care-first-hikari-core
  "Internal: single disclosure path (open or held via opts :disclosure)."
  [opts]
  (let [e0 (enroll opts)
        e (-> e0
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-hikari-r1-scaffold
               :energy-imputed-usd-micros-yr
               (or (:energy-imputed-usd-micros-yr opts) DEFAULT-HIKARI-MICROS-YR)))
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        hikari-design (hikari/design-public-facts)
        care-design (care/design-public-facts)
        out {:path "l0-care-first-hikari"
             :api "enroll+care+hikari+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :care-full-chain-refused (boolean (:care-full-chain-refused e))
             :hikari-full-chain-refused (boolean (:hikari-full-chain-refused e))
             :care-hikari-both-refused
             (boolean (and (:care-full-chain-refused e)
                           (:hikari-full-chain-refused e)))
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             :care-design care-design
             :hikari-design hikari-design
             :care-first-api-path (or (:care-first-api-path hikari-design)
                                     "care-first-hikari-path")
             :care-first-before-rails (or (:care-first-before-rails hikari-design)
                                         ["care" "housing"])
             :hikari-live-produce (boolean (:live-produce hikari-design))
             :hikari-generate-executed (boolean (:generate-executed hikari-design))
             :care-delivery-executed (boolean (:care-delivery-executed care-design))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "care-first (孫/子) + hikari energy — R1 gated DESIGN refuse; ladder offline"}]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :care-design :hikari-design
             :care-first-before-rails :care-first-api-path))
    out))

(defn care-first-hikari-path
  "Priority stack slice: (1) enroll (2) disclosure open (3) care then hikari energy
   + ladder. Care-first 孫/子; both full-chains refuse offline. cash≡0.
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (care-first-hikari-core opts)
        held (when include-held?
               (care-first-hikari-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :care-hikari-both-refused (boolean (:care-hikari-both-refused held))
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — membranes refuse; ladder refused"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-both-refused
                                 (boolean (:care-hikari-both-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(defn- care-first-mitsuho-hikari-core
  "Internal: single disclosure path for care then mitsuho+hikari dual rail."
  [opts]
  (let [e0 (enroll opts)
        e (-> e0
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-mitsuho-r1-scaffold
               :food-imputed-usd-micros-yr
               (or (:food-imputed-usd-micros-yr opts) DEFAULT-MITSUHO-MICROS-YR))
              (attach-hikari-r1-scaffold
               :energy-imputed-usd-micros-yr
               (or (:energy-imputed-usd-micros-yr opts) DEFAULT-HIKARI-MICROS-YR)))
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        all-refused?
        (boolean (and (:care-full-chain-refused e)
                      (:mitsuho-full-chain-refused e)
                      (:hikari-full-chain-refused e)))
        mitsuho-design (mitsuho/design-public-facts)
        hikari-design (hikari/design-public-facts)
        care-design (care/design-public-facts)
        out {:path "l0-care-first-mitsuho-hikari"
             :api "enroll+care+mitsuho+hikari+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :care-full-chain-refused (boolean (:care-full-chain-refused e))
             :mitsuho-full-chain-refused (boolean (:mitsuho-full-chain-refused e))
             :hikari-full-chain-refused (boolean (:hikari-full-chain-refused e))
             :care-mitsuho-hikari-all-refused all-refused?
             :mitsuho-hikari-both-refused
             (boolean (and (:mitsuho-full-chain-refused e)
                           (:hikari-full-chain-refused e)))
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             :care-design care-design
             :mitsuho-design mitsuho-design
             :hikari-design hikari-design
             :care-first-before-rails ["care" "housing"]
             :mitsuho-live-produce (boolean (:live-produce mitsuho-design))
             :mitsuho-produce-executed (boolean (:produce-executed mitsuho-design))
             :hikari-live-produce (boolean (:live-produce hikari-design))
             :hikari-generate-executed (boolean (:generate-executed hikari-design))
             :care-delivery-executed (boolean (:care-delivery-executed care-design))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "care-first (孫/子) + mitsuho food + hikari energy — R1 gated DESIGN refuse; ladder offline"}]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :care-design :mitsuho-design :hikari-design
             :care-first-before-rails))
    out))

(defn care-first-mitsuho-hikari-path
  "Priority stack slice: (1) enroll (2) disclosure open (3) care then mitsuho+hikari
   (food+energy R1→gated DESIGN) + ladder. Care-first for 孫/子; all three full-chains
   default refuse offline. cash≡0.
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (care-first-mitsuho-hikari-core opts)
        held (when include-held?
               (care-first-mitsuho-hikari-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :care-mitsuho-hikari-all-refused
                    (boolean (:care-mitsuho-hikari-all-refused held))
                    :mitsuho-hikari-both-refused
                    (boolean (:mitsuho-hikari-both-refused held))
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — dual rail refuse; ladder refused"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-all-refused
                                 (boolean (:care-mitsuho-hikari-all-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(defn- care-housing-first-core
  "Internal: single disclosure path for care+housing multi-gen substrate."
  [opts]
  (let [e0 (enroll opts)
        e (-> e0
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-housing-r1-scaffold
               :housing-imputed-usd-micros-yr
               (or (:housing-imputed-usd-micros-yr opts) DEFAULT-HOUSING-MICROS-YR)))
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        out {:path "l0-care-housing-first"
             :api "enroll+care+housing+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :care-full-chain-refused (boolean (:care-full-chain-refused e))
             :housing-full-chain-refused (boolean (:housing-full-chain-refused e))
             :care-housing-both-refused
             (boolean (and (:care-full-chain-refused e)
                           (:housing-full-chain-refused e)))
             :land-grant-executed false
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "care+housing multi-gen substrate (孫/子) — R1 refuse; land-grant never"}]
    (pp/assert-no-public-scores! (dissoc out :note :api :multi-gen-facts))
    out))

(defn care-housing-first-path
  "Priority stack: (1) enroll (2) open disclosure (3) care+housing multi-gen substrate
   (孫/子) + ladder. Full-chains refuse; land-grant never. cash≡0.
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (care-housing-first-core opts)
        held (when include-held?
               (care-housing-first-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :care-housing-both-refused (boolean (:care-housing-both-refused held))
                    :land-grant-executed false
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — care+housing refuse; ladder refused; land-grant never"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-both-refused
                                 (boolean (:care-housing-both-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(defn- multi-gen-substrate-core
  "Internal: single disclosure path for L4 multi-gen + mitsuho+hikari four-rail."
  [opts]
  (let [e0 (enroll opts)
        e (-> e0
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-housing-r1-scaffold
               :housing-imputed-usd-micros-yr
               (or (:housing-imputed-usd-micros-yr opts) DEFAULT-HOUSING-MICROS-YR))
              (attach-mitsuho-r1-scaffold
               :food-imputed-usd-micros-yr
               (or (:food-imputed-usd-micros-yr opts) DEFAULT-MITSUHO-MICROS-YR))
              (attach-hikari-r1-scaffold
               :energy-imputed-usd-micros-yr
               (or (:energy-imputed-usd-micros-yr opts) DEFAULT-HIKARI-MICROS-YR)))
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        four-refused?
        (boolean (and (:care-full-chain-refused e)
                      (:housing-full-chain-refused e)
                      (:mitsuho-full-chain-refused e)
                      (:hikari-full-chain-refused e)))
        out {:path "l0-multi-gen-substrate"
             :api "enroll+care+housing+mitsuho+hikari+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :care-full-chain-refused (boolean (:care-full-chain-refused e))
             :housing-full-chain-refused (boolean (:housing-full-chain-refused e))
             :mitsuho-full-chain-refused (boolean (:mitsuho-full-chain-refused e))
             :hikari-full-chain-refused (boolean (:hikari-full-chain-refused e))
             :care-housing-both-refused
             (boolean (and (:care-full-chain-refused e)
                           (:housing-full-chain-refused e)))
             :mitsuho-hikari-both-refused
             (boolean (and (:mitsuho-full-chain-refused e)
                           (:hikari-full-chain-refused e)))
             :care-housing-mitsuho-hikari-all-refused four-refused?
             :land-grant-executed false
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "L4 multi-gen substrate (care+housing 孫/子) + mitsuho+hikari — R1 gated refuse; land-grant never"}]
    (pp/assert-no-public-scores! (dissoc out :note :api :multi-gen-facts))
    out))

(defn multi-gen-substrate-path
  "Priority stack L4 slice: (1) enroll (2) disclosure open (3) care+housing first
   (孫/子 multi-gen substrate) then mitsuho+hikari (food+energy R1→gated DESIGN) + ladder.
   All four full-chains default refuse offline; land-grant never. cash≡0.
   Combines care-housing-first + care-first-mitsuho-hikari into one offline smoke path.
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (multi-gen-substrate-core opts)
        held (when include-held?
               (multi-gen-substrate-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :care-housing-mitsuho-hikari-all-refused
                    (boolean (:care-housing-mitsuho-hikari-all-refused held))
                    :care-housing-both-refused
                    (boolean (:care-housing-both-refused held))
                    :mitsuho-hikari-both-refused
                    (boolean (:mitsuho-hikari-both-refused held))
                    :land-grant-executed false
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — four-rail refuse; ladder refused; land-grant never"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-all-refused
                                 (boolean (:care-housing-mitsuho-hikari-all-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(defn- full-inkind-substrate-core
  "Internal: single disclosure path for six in-kind (multi-gen + vocation)."
  [opts]
  (let [e0 (enroll opts)
        e (-> e0
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-housing-r1-scaffold
               :housing-imputed-usd-micros-yr
               (or (:housing-imputed-usd-micros-yr opts) DEFAULT-HOUSING-MICROS-YR))
              (attach-mitsuho-r1-scaffold
               :food-imputed-usd-micros-yr
               (or (:food-imputed-usd-micros-yr opts) DEFAULT-MITSUHO-MICROS-YR))
              (attach-hikari-r1-scaffold
               :energy-imputed-usd-micros-yr
               (or (:energy-imputed-usd-micros-yr opts) DEFAULT-HIKARI-MICROS-YR))
              (attach-tooling-r1-scaffold
               :tooling-imputed-usd-micros-yr
               (or (:tooling-imputed-usd-micros-yr opts) DEFAULT-TOOLING-MICROS-YR))
              (attach-compute-r1-scaffold
               :compute-imputed-usd-micros-yr
               (or (:compute-imputed-usd-micros-yr opts) DEFAULT-COMPUTE-MICROS-YR)))
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        six-refused?
        (boolean (and (:care-full-chain-refused e)
                      (:housing-full-chain-refused e)
                      (:mitsuho-full-chain-refused e)
                      (:hikari-full-chain-refused e)
                      (:tooling-full-chain-refused e)
                      (:compute-full-chain-refused e)))
        out {:path "l0-full-inkind-substrate"
             :api "enroll+six-inkind+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :care-full-chain-refused (boolean (:care-full-chain-refused e))
             :housing-full-chain-refused (boolean (:housing-full-chain-refused e))
             :mitsuho-full-chain-refused (boolean (:mitsuho-full-chain-refused e))
             :hikari-full-chain-refused (boolean (:hikari-full-chain-refused e))
             :tooling-full-chain-refused (boolean (:tooling-full-chain-refused e))
             :compute-full-chain-refused (boolean (:compute-full-chain-refused e))
             :care-housing-both-refused
             (boolean (and (:care-full-chain-refused e)
                           (:housing-full-chain-refused e)))
             :mitsuho-hikari-both-refused
             (boolean (and (:mitsuho-full-chain-refused e)
                           (:hikari-full-chain-refused e)))
             :tooling-compute-both-refused
             (boolean (and (:tooling-full-chain-refused e)
                           (:compute-full-chain-refused e)))
             :all-inkind-produce-rails-full-chain-refused six-refused?
             :land-grant-executed false
             :fulfillment-executed false
             :quota-executed false
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "L4 multi-gen + vocation six in-kind (itonami recovery) — R1 gated refuse; no land-grant/fulfillment/quota"}]
    (pp/assert-no-public-scores! (dissoc out :note :api :multi-gen-facts))
    out))

(defn full-inkind-substrate-path
  "Priority stack for itonami/robotics displacement recovery offline:
   (1) enroll (2) disclosure open (3) care+housing (孫/子) then food+energy then
   tooling+compute vocation rails + ladder. All six in-kind full-chains default refuse;
   land-grant / fulfillment / quota never executed. cash≡0.
   Does not attach liquidity residual (see enroll-with-all-seven-rails).
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (full-inkind-substrate-core opts)
        held (when include-held?
               (full-inkind-substrate-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :all-inkind-produce-rails-full-chain-refused
                    (boolean (:all-inkind-produce-rails-full-chain-refused held))
                    :tooling-compute-both-refused
                    (boolean (:tooling-compute-both-refused held))
                    :land-grant-executed false
                    :fulfillment-executed false
                    :quota-executed false
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — six in-kind refuse; ladder refused; no land-grant/fulfillment/quota"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-all-refused
                                 (boolean (:all-inkind-produce-rails-full-chain-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(defn- vocation-recovery-core
  "Internal: single disclosure path for tooling+compute vocation rails."
  [opts]
  (let [e0 (enroll opts)
        e (-> e0
              (attach-tooling-r1-scaffold
               :tooling-imputed-usd-micros-yr
               (or (:tooling-imputed-usd-micros-yr opts) DEFAULT-TOOLING-MICROS-YR))
              (attach-compute-r1-scaffold
               :compute-imputed-usd-micros-yr
               (or (:compute-imputed-usd-micros-yr opts) DEFAULT-COMPUTE-MICROS-YR)))
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        both-refused?
        (boolean (and (:tooling-full-chain-refused e)
                      (:compute-full-chain-refused e)))
        out {:path "l0-vocation-recovery"
             :api "enroll+tooling+compute+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :tooling-full-chain-refused (boolean (:tooling-full-chain-refused e))
             :compute-full-chain-refused (boolean (:compute-full-chain-refused e))
             :tooling-compute-both-refused both-refused?
             :fulfillment-executed false
             :quota-executed false
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "itonami vocation recovery (tooling+compute) — R1 gated refuse; no fulfillment/quota"}]
    (pp/assert-no-public-scores! (dissoc out :note :api :multi-gen-facts))
    out))

(defn vocation-recovery-path
  "Priority stack for robotics/itonami job-loss recovery offline (vocation rails only):
   (1) enroll (2) disclosure open (3) tooling-okaimono + compute-murakumo R1→gated DESIGN
   + ladder. Both vocation full-chains default refuse; fulfillment/quota never.
   Multi-gen floors are a separate path (multi-gen-substrate / full-inkind). cash≡0.
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (vocation-recovery-core opts)
        held (when include-held?
               (vocation-recovery-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :tooling-compute-both-refused
                    (boolean (:tooling-compute-both-refused held))
                    :fulfillment-executed false
                    :quota-executed false
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — vocation rails refuse; ladder refused; no fulfillment/quota"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-both-refused
                                 (boolean (:tooling-compute-both-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(defn- liquidity-residual-core
  "Internal: single disclosure path for warifu liquidity residual."
  [opts]
  (let [e0 (enroll opts)
        e (attach-liquidity-r1-scaffold
           e0
           :liquidity-imputed-usd-micros-yr
           (or (:liquidity-imputed-usd-micros-yr opts) DEFAULT-LIQUIDITY-MICROS-YR))
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        out {:path "l0-liquidity-residual"
             :api "enroll+liquidity+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :liquidity-receive-full-chain-refused
             (boolean (:liquidity-receive-full-chain-refused e))
             :liquidity-member-principal true
             :liquidity-loan-executed false
             :liquidity-cash-usd-micros 0
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "liquidity residual (warifu member-principal) — R1 gated refuse; loan never; cash≡0"}]
    (pp/assert-no-public-scores! (dissoc out :note :api :multi-gen-facts))
    out))

(defn liquidity-residual-path
  "Priority stack residual after multi-gen/vocation in-kind floors (N4 / warifu):
   (1) enroll (2) disclosure open (3) liquidity-warifu R1→gated-live + gated-receive
   DESIGN + ladder. Member-principal only; loan never; cash≡0 always.
   Completes the offline path family before all-seven (six in-kind + this residual).
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (liquidity-residual-core opts)
        held (when include-held?
               (liquidity-residual-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :liquidity-receive-full-chain-refused
                    (boolean (:liquidity-receive-full-chain-refused held))
                    :liquidity-member-principal true
                    :liquidity-loan-executed false
                    :liquidity-cash-usd-micros 0
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — liquidity residual refuse; ladder refused; loan never; cash≡0"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-receive-refused
                                 (boolean (:liquidity-receive-full-chain-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(defn- all-seven-substrate-core
  "Internal: single disclosure path for six in-kind + liquidity residual capstone."
  [opts]
  (let [e (enroll-with-all-seven-rails opts)
        lad (try-ladder-advance e
                                :member-signature
                                (or (:member-signature opts)
                                    (get-in e [:vow :member-signature])))
        out {:path "l0-all-seven-substrate"
             :api "enroll-with-all-seven-rails+ladder"
             :disclosure-state (when-let [s (:disclosure-state e)] (name s))
             :disclosure-held (boolean (:disclosure-held? e))
             :entitlements-may-flow (boolean (:entitlements-may-flow? e))
             :care-full-chain-refused (boolean (:care-full-chain-refused e))
             :housing-full-chain-refused (boolean (:housing-full-chain-refused e))
             :mitsuho-full-chain-refused (boolean (:mitsuho-full-chain-refused e))
             :hikari-full-chain-refused (boolean (:hikari-full-chain-refused e))
             :tooling-full-chain-refused (boolean (:tooling-full-chain-refused e))
             :compute-full-chain-refused (boolean (:compute-full-chain-refused e))
             :all-inkind-produce-rails-full-chain-refused
             (boolean (:all-inkind-produce-rails-full-chain-refused e))
             :liquidity-receive-full-chain-refused
             (boolean (:liquidity-receive-full-chain-refused e))
             :all-seven-rails-receive-membrane-refused
             (boolean (:all-seven-rails-receive-membrane-refused e))
             :liquidity-member-principal true
             :liquidity-loan-executed false
             :liquidity-cash-usd-micros 0
             :land-grant-executed false
             :fulfillment-executed false
             :quota-executed false
             :ladder-advance-phase (when-let [p (:phase lad)] (name p))
             :ladder-advance-refused (boolean (= :refused (:phase lad)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :multi-gen-facts MULTI-GEN-FACTS
             :note "all-seven substrate (six in-kind + warifu residual) — R1 gated refuse; loan/land-grant never"}]
    (pp/assert-no-public-scores! (dissoc out :note :api :multi-gen-facts))
    out))

(defn all-seven-substrate-path
  "Capstone offline priority path for covenantal SS (itonami displacement recovery):
   (1) enroll (2) disclosure open (3) six in-kind (care+housing+food+energy+tooling+compute)
   + liquidity residual (member-principal) R1→gated DESIGN + ladder.
   All membranes default refuse; land-grant/fulfillment/quota/loan never. cash≡0.
   Ladder-path twin of enroll-with-all-seven-rails for scorecard/public smoke.
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [opts]
  (let [include-held? (not (false? (:include-held-stress opts)))
        open (all-seven-substrate-core opts)
        held (when include-held?
               (all-seven-substrate-core (assoc opts :disclosure STALE-DISC)))
        held-stress
        (when held
          (let [hs {:disclosure-state (:disclosure-state held)
                    :disclosure-held (boolean (:disclosure-held held))
                    :entitlements-may-flow (boolean (:entitlements-may-flow held))
                    :all-inkind-produce-rails-full-chain-refused
                    (boolean (:all-inkind-produce-rails-full-chain-refused held))
                    :all-seven-rails-receive-membrane-refused
                    (boolean (:all-seven-rails-receive-membrane-refused held))
                    :liquidity-receive-full-chain-refused
                    (boolean (:liquidity-receive-full-chain-refused held))
                    :liquidity-member-principal true
                    :liquidity-loan-executed false
                    :liquidity-cash-usd-micros 0
                    :land-grant-executed false
                    :fulfillment-executed false
                    :quota-executed false
                    :ladder-advance-refused (boolean (:ladder-advance-refused held))
                    :ladder-advance-phase (:ladder-advance-phase held)
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress — all-seven membrane refuse; ladder refused; loan/land-grant never"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> open
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-membrane-refused
                                 (boolean (:all-seven-rails-receive-membrane-refused held-stress))))]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :multi-gen-facts :held-stress))
    out))

(def PRIORITY-PATH-CATALOG
  "Machine-readable index of offline ladder-path smokes (priority 1+2+3).
   Facts only — no scores; cash≡0; live default refuse. Order = care-first stack.
   Every path embeds priority-(2) held-stress (stale disclosure → ladder refuse)."
  [{:id "care-first-mitsuho"
    :api "care-first-mitsuho-path"
    :rails ["care" "food"]
    :role "孫/子 care then food"
    :held-stress-embed true
    :live false :cash-usd-micros 0}
   {:id "care-first-hikari"
    :api "care-first-hikari-path"
    :rails ["care" "energy"]
    :role "孫/子 care then energy"
    :held-stress-embed true
    :live false :cash-usd-micros 0}
   {:id "care-first-mitsuho-hikari"
    :api "care-first-mitsuho-hikari-path"
    :rails ["care" "food" "energy"]
    :role "care then food+energy dual rail"
    :held-stress-embed true
    :live false :cash-usd-micros 0}
   {:id "care-housing-first"
    :api "care-housing-first-path"
    :rails ["care" "housing"]
    :role "L4 multi-gen substrate only"
    :held-stress-embed true
    :live false :cash-usd-micros 0 :land-grant-executed false}
   {:id "multi-gen-substrate"
    :api "multi-gen-substrate-path"
    :rails ["care" "housing" "food" "energy"]
    :role "L4 multi-gen + food/energy"
    :held-stress-embed true
    :live false :cash-usd-micros 0 :land-grant-executed false}
   {:id "full-inkind-substrate"
    :api "full-inkind-substrate-path"
    :rails ["care" "housing" "food" "energy" "tooling" "compute"]
    :role "six in-kind (itonami multi-gen + vocation)"
    :held-stress-embed true
    :live false :cash-usd-micros 0
    :land-grant-executed false :fulfillment-executed false :quota-executed false}
   {:id "vocation-recovery"
    :api "vocation-recovery-path"
    :rails ["tooling" "compute"]
    :role "itonami job-loss vocation rails only"
    :held-stress-embed true
    :live false :cash-usd-micros 0
    :fulfillment-executed false :quota-executed false}
   {:id "liquidity-residual"
    :api "liquidity-residual-path"
    :rails ["liquidity"]
    :role "warifu member-principal residual (N4)"
    :held-stress-embed true
    :live false :cash-usd-micros 0
    :liquidity-member-principal true :liquidity-loan-executed false}
   {:id "all-seven-substrate"
    :api "all-seven-substrate-path"
    :rails ["care" "housing" "food" "energy" "tooling" "compute" "liquidity"]
    :role "capstone six in-kind + residual"
    :held-stress-embed true
    :live false :cash-usd-micros 0
    :liquidity-member-principal true :liquidity-loan-executed false
    :land-grant-executed false}])

(defn priority-path-catalog
  "Facts-only catalog of offline priority ladder paths (no scores; cash≡0).
   Used by scorecard/public surfaces for discovery; does not execute paths.
   held-stress-embed-count == path-count when all paths embed priority-(2) stress."
  []
  (let [held-n (count (filter :held-stress-embed PRIORITY-PATH-CATALOG))
        out {:catalog-id "fuchi.l0-offline-priority-paths"
             :priority-stack PRIORITY-STACK
             :path-count (count PRIORITY-PATH-CATALOG)
             :held-stress-embed-count held-n
             :paths (mapv (fn [p]
                            (assoc p
                                   :score-surface []
                                   :priority-stack PRIORITY-STACK))
                          PRIORITY-PATH-CATALOG)
             :invariants {:cash-usd-micros 0
                          :live false
                          :score-surface []
                          :loan-never true
                          :land-grant-never true
                          :public-person-facts-only true
                          :held-stress-embed-all true}
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "offline path catalog — generators only; no live side-effects; all paths hold-stress embed"}]
    (pp/assert-no-public-scores! (dissoc out :note :paths :invariants))
    (doseq [p (:paths out)]
      (pp/assert-no-public-scores!
       (dissoc p :rails :role :api :id :held-stress-embed)))
    out))

(defn attach-mitsuho-r1-scaffold
  "Priority (3) slice on an enroll result: mitsuho food R1 dry → gated-live /
   gated-receive / gated-produce DESIGN status (default refuse). Never produce-execute.
   When disclosure held, r1 package itself refuses via hold-machine. cash≡0."
  [enrolled & {:keys [food-imputed-usd-micros-yr]
               :or {food-imputed-usd-micros-yr DEFAULT-MITSUHO-MICROS-YR}}]
  (let [subject-did (or (get-in enrolled [:vow :subject-did])
                        (get-in enrolled [:public-person :did]))
        hold (:disclosure-hold enrolled)
        person (or (:person enrolled)
                   {:did subject-did
                    :covenant "vowed"
                    :rails [{:kind "food" :active? true}
                            {:kind "care" :active? true}]
                    :floor-usd-micros-yr food-imputed-usd-micros-yr
                    :disclosure (or (get-in enrolled [:person :disclosure])
                                    (default-disclosure))
                    :exit-suspended? (boolean (:disclosure-held? enrolled))
                    :stage "L0"
                    :cash-usd-micros 0})
        person (update person :rails
                       (fn [rs]
                         (let [rs (or rs [])]
                           (if (some #(= "food" (or (:kind %) %)) rs)
                             rs
                             (conj (vec rs) {:kind "food" :active? true})))))
        pkg (mitsuho/r1-dry-package
             {:alloc-id (str "l0-mitsuho-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr food-imputed-usd-micros-yr
              :person person
              :hold-machine hold})
        gated (when (and pkg (not= :refused (:phase pkg)))
                (mitsuho/gated-live-status pkg :hold-machine hold))
        recv (when (and pkg (not= :refused (:phase pkg)))
               (frecv/gated-receive-status pkg))
        prod (when (and pkg (not= :refused (:phase pkg)))
               (mprod/gated-produce-status pkg))
        full-chain-refused?
        (boolean
         (or (= :refused (:phase pkg))
             (and (some? gated) (some? recv) (some? prod)
                  (not (true? (:admissible gated)))
                  (not (true? (:admissible recv)))
                  (not (true? (:admissible prod))))))
        membrane {:mitsuho-r1-phase (when pkg (name (:phase pkg)))
                  :mitsuho-gated-admissible (boolean (:admissible gated))
                  :mitsuho-gated-receive-admissible (boolean (:admissible recv))
                  :mitsuho-gated-produce-admissible (boolean (:admissible prod))
                  :mitsuho-full-chain-refused full-chain-refused?
                  :produce-executed false
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK
                  :note "L0 mitsuho R1→gated DESIGN — default refuse; no live produce"}
        out (assoc enrolled
                   :food-package pkg
                   :food-gated-live-status gated
                   :food-gated-receive-status recv
                   :food-gated-produce-status prod
                   :mitsuho-membrane membrane
                   :mitsuho-full-chain-refused full-chain-refused?
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! membrane)
    (when pkg (pp/assert-no-public-scores! pkg))
    (doseq [st [gated recv prod]]
      (when st (pp/assert-no-public-scores! st)))
    out))

(defn enroll-with-mitsuho
  "Offline path priorities (1)+(2)+(3): L0 enroll + disclosure continuity + mitsuho membrane."
  [opts]
  (attach-mitsuho-r1-scaffold (enroll opts)
                              :food-imputed-usd-micros-yr
                              (or (:food-imputed-usd-micros-yr opts)
                                  DEFAULT-MITSUHO-MICROS-YR)))

(defn attach-care-r1-scaffold
  "Priority (3) care-iyashi rail (孫/子 multi-gen) on enroll result:
   R1 dry → gated-live / receive / produce DESIGN (default refuse).
   Never care-delivery-execute. Held disclosure → R1 refuse. cash≡0."
  [enrolled & {:keys [care-imputed-usd-micros-yr]
               :or {care-imputed-usd-micros-yr DEFAULT-CARE-MICROS-YR}}]
  (let [subject-did (or (get-in enrolled [:vow :subject-did])
                        (get-in enrolled [:public-person :did]))
        hold (:disclosure-hold enrolled)
        person (or (:person enrolled)
                   {:did subject-did
                    :covenant "vowed"
                    :rails [{:kind "care" :active? true}]
                    :floor-usd-micros-yr care-imputed-usd-micros-yr
                    :disclosure (or (get-in enrolled [:person :disclosure])
                                    (default-disclosure))
                    :exit-suspended? (boolean (:disclosure-held? enrolled))
                    :stage "L0"
                    :cash-usd-micros 0})
        person (update person :rails
                       (fn [rs]
                         (let [rs (or rs [])]
                           (if (some #(= "care" (or (:kind %) %)) rs)
                             rs
                             (conj (vec rs) {:kind "care" :active? true})))))
        pkg (care/r1-dry-package
             {:alloc-id (str "l0-care-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr care-imputed-usd-micros-yr
              :person person
              :hold-machine hold})
        gated (when (and pkg (not= :refused (:phase pkg)))
                (care/gated-live-status pkg :hold-machine hold))
        recv (when (and pkg (not= :refused (:phase pkg)))
               (crecv/gated-receive-status pkg))
        prod (when (and pkg (not= :refused (:phase pkg)))
               (cprod/gated-produce-status pkg))
        full-chain-refused?
        (boolean
         (or (= :refused (:phase pkg))
             (and (some? gated) (some? recv) (some? prod)
                  (not (true? (:admissible gated)))
                  (not (true? (:admissible recv)))
                  (not (true? (:admissible prod))))))
        membrane {:care-r1-phase (when pkg (name (:phase pkg)))
                  :care-gated-admissible (boolean (:admissible gated))
                  :care-gated-receive-admissible (boolean (:admissible recv))
                  :care-gated-produce-admissible (boolean (:admissible prod))
                  :care-full-chain-refused full-chain-refused?
                  :care-delivery-executed false
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK
                  :multi-gen-facts MULTI-GEN-FACTS
                  :note "L0 care-iyashi R1→gated DESIGN (孫/子) — default refuse; no delivery"}
        out (assoc enrolled
                   :care-package pkg
                   :care-gated-live-status gated
                   :care-gated-receive-status recv
                   :care-gated-produce-status prod
                   :care-membrane membrane
                   :care-full-chain-refused full-chain-refused?
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! membrane)
    (when pkg (pp/assert-no-public-scores! pkg))
    (doseq [st [gated recv prod]]
      (when st (pp/assert-no-public-scores! st)))
    out))

(defn enroll-with-care
  "Offline path priorities (1)+(2)+(3 care-first 孫/子): L0 + disclosure + care membrane."
  [opts]
  (attach-care-r1-scaffold (enroll opts)
                           :care-imputed-usd-micros-yr
                           (or (:care-imputed-usd-micros-yr opts)
                               DEFAULT-CARE-MICROS-YR)))

(defn attach-housing-r1-scaffold
  "Priority (3) housing-commons rail (孫/子 multi-gen shelter) on enroll result:
   R1 dry → gated-live / receive / produce DESIGN (default refuse).
   Never land-grant-execute. Held disclosure → R1 refuse. cash≡0."
  [enrolled & {:keys [housing-imputed-usd-micros-yr]
               :or {housing-imputed-usd-micros-yr DEFAULT-HOUSING-MICROS-YR}}]
  (let [subject-did (or (get-in enrolled [:vow :subject-did])
                        (get-in enrolled [:public-person :did]))
        hold (:disclosure-hold enrolled)
        person (or (:person enrolled)
                   {:did subject-did
                    :covenant "vowed"
                    :rails [{:kind "housing" :active? true}
                            {:kind "care" :active? true}]
                    :floor-usd-micros-yr housing-imputed-usd-micros-yr
                    :disclosure (or (get-in enrolled [:person :disclosure])
                                    (default-disclosure))
                    :exit-suspended? (boolean (:disclosure-held? enrolled))
                    :stage "L0"
                    :cash-usd-micros 0})
        person (update person :rails
                       (fn [rs]
                         (let [rs (or rs [])]
                           (if (some #(= "housing" (or (:kind %) %)) rs)
                             rs
                             (conj (vec rs) {:kind "housing" :active? true})))))
        pkg (housing/r1-dry-package
             {:alloc-id (str "l0-housing-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr housing-imputed-usd-micros-yr
              :person person
              :hold-machine hold})
        gated (when (and pkg (not= :refused (:phase pkg)))
                (housing/gated-live-status pkg :hold-machine hold
                                           :council-housing-held? false))
        recv (when (and pkg (not= :refused (:phase pkg)))
               (housrecv/gated-receive-status pkg))
        prod (when (and pkg (not= :refused (:phase pkg)))
               (housprod/gated-produce-status pkg))
        full-chain-refused?
        (boolean
         (or (= :refused (:phase pkg))
             (and (some? gated) (some? recv) (some? prod)
                  (not (true? (:admissible gated)))
                  (not (true? (:admissible recv)))
                  (not (true? (:admissible prod))))))
        care-housing-refused?
        (boolean (and (true? (:care-full-chain-refused enrolled))
                      full-chain-refused?))
        membrane {:housing-r1-phase (when pkg (name (:phase pkg)))
                  :housing-gated-admissible (boolean (:admissible gated))
                  :housing-gated-receive-admissible (boolean (:admissible recv))
                  :housing-gated-produce-admissible (boolean (:admissible prod))
                  :housing-full-chain-refused full-chain-refused?
                  :care-housing-full-chain-refused care-housing-refused?
                  :land-grant-executed false
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK
                  :multi-gen-facts MULTI-GEN-FACTS
                  :note "L0 housing-commons R1→gated DESIGN (孫/子) — default refuse; no land grant"}
        out (assoc enrolled
                   :housing-package pkg
                   :housing-gated-live-status gated
                   :housing-gated-receive-status recv
                   :housing-gated-produce-status prod
                   :housing-membrane membrane
                   :housing-full-chain-refused full-chain-refused?
                   :care-housing-full-chain-refused care-housing-refused?
                   :land-grant-executed false
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! membrane)
    (when pkg (pp/assert-no-public-scores! pkg))
    (doseq [st [gated recv prod]]
      (when st (pp/assert-no-public-scores! st)))
    out))

(defn enroll-with-housing
  "Offline path priorities (1)+(2)+(3 housing 孫/子): L0 + disclosure + housing membrane."
  [opts]
  (attach-housing-r1-scaffold (enroll opts)
                              :housing-imputed-usd-micros-yr
                              (or (:housing-imputed-usd-micros-yr opts)
                                  DEFAULT-HOUSING-MICROS-YR)))

(defn enroll-with-care-housing
  "L4-style multi-gen substrate first (care then housing): L0 + disclosure + both membranes.
   Full-chains default refuse. cash≡0. land-grant never executed."
  [opts]
  (let [e (-> (enroll opts)
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-housing-r1-scaffold
               :housing-imputed-usd-micros-yr
               (or (:housing-imputed-usd-micros-yr opts) DEFAULT-HOUSING-MICROS-YR)))]
    (assoc e
           :care-housing-full-chain-refused
           (boolean (and (:care-full-chain-refused e)
                         (:housing-full-chain-refused e)))
           :land-grant-executed false)))

(defn attach-hikari-r1-scaffold
  "Priority (3) energy rail on an enroll result: hikari R1 dry → gated-live /
   gated-receive / gated-produce DESIGN (default refuse). Never generate-execute.
   Held disclosure → R1 refuse. cash≡0. no scores."
  [enrolled & {:keys [energy-imputed-usd-micros-yr]
               :or {energy-imputed-usd-micros-yr DEFAULT-HIKARI-MICROS-YR}}]
  (let [subject-did (or (get-in enrolled [:vow :subject-did])
                        (get-in enrolled [:public-person :did]))
        hold (:disclosure-hold enrolled)
        person (or (:person enrolled)
                   {:did subject-did
                    :covenant "vowed"
                    :rails [{:kind "energy" :active? true}
                            {:kind "care" :active? true}]
                    :floor-usd-micros-yr energy-imputed-usd-micros-yr
                    :disclosure (or (get-in enrolled [:person :disclosure])
                                    (default-disclosure))
                    :exit-suspended? (boolean (:disclosure-held? enrolled))
                    :stage "L0"
                    :cash-usd-micros 0})
        person (update person :rails
                       (fn [rs]
                         (let [rs (or rs [])]
                           (if (some #(= "energy" (or (:kind %) %)) rs)
                             rs
                             (conj (vec rs) {:kind "energy" :active? true})))))
        pkg (hikari/r1-dry-package
             {:alloc-id (str "l0-hikari-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr energy-imputed-usd-micros-yr
              :person person
              :hold-machine hold})
        gated (when (and pkg (not= :refused (:phase pkg)))
                (hikari/gated-live-status pkg :hold-machine hold))
        recv (when (and pkg (not= :refused (:phase pkg)))
               (hrecv/gated-receive-status pkg))
        prod (when (and pkg (not= :refused (:phase pkg)))
               (hprod/gated-produce-status pkg))
        full-chain-refused?
        (boolean
         (or (= :refused (:phase pkg))
             (and (some? gated) (some? recv) (some? prod)
                  (not (true? (:admissible gated)))
                  (not (true? (:admissible recv)))
                  (not (true? (:admissible prod))))))
        both-food-energy-refused?
        (boolean (and (true? (:mitsuho-full-chain-refused enrolled))
                      full-chain-refused?))
        membrane {:hikari-r1-phase (when pkg (name (:phase pkg)))
                  :hikari-gated-admissible (boolean (:admissible gated))
                  :hikari-gated-receive-admissible (boolean (:admissible recv))
                  :hikari-gated-produce-admissible (boolean (:admissible prod))
                  :hikari-full-chain-refused full-chain-refused?
                  :mitsuho-hikari-full-chain-refused both-food-energy-refused?
                  :generate-executed false
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK
                  :note "L0 hikari R1→gated DESIGN — default refuse; no live generate"}
        out (assoc enrolled
                   :energy-package pkg
                   :energy-gated-live-status gated
                   :energy-gated-receive-status recv
                   :energy-gated-produce-status prod
                   :hikari-membrane membrane
                   :hikari-full-chain-refused full-chain-refused?
                   :mitsuho-hikari-full-chain-refused both-food-energy-refused?
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! membrane)
    (when pkg (pp/assert-no-public-scores! pkg))
    (doseq [st [gated recv prod]]
      (when st (pp/assert-no-public-scores! st)))
    out))

(defn enroll-with-hikari
  "Offline path priorities (1)+(2)+(3 energy): L0 enroll + disclosure + hikari membrane."
  [opts]
  (attach-hikari-r1-scaffold (enroll opts)
                             :energy-imputed-usd-micros-yr
                             (or (:energy-imputed-usd-micros-yr opts)
                                 DEFAULT-HIKARI-MICROS-YR)))

(defn enroll-with-mitsuho-hikari
  "Offline path priorities (1)+(2)+(3 food+energy): L0 + disclosure + mitsuho + hikari
   membranes. Both full-chains default refuse offline. cash≡0."
  [opts]
  (-> (enroll opts)
      (attach-mitsuho-r1-scaffold
       :food-imputed-usd-micros-yr
       (or (:food-imputed-usd-micros-yr opts) DEFAULT-MITSUHO-MICROS-YR))
      (attach-hikari-r1-scaffold
       :energy-imputed-usd-micros-yr
       (or (:energy-imputed-usd-micros-yr opts) DEFAULT-HIKARI-MICROS-YR))))

(defn enroll-with-care-mitsuho-hikari
  "Care-first offline path (wellbecoming > 孫 > 子): L0 + disclosure + care + food + energy
   membranes. All full-chains default refuse. cash≡0."
  [opts]
  (let [e (-> (enroll opts)
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-mitsuho-r1-scaffold
               :food-imputed-usd-micros-yr
               (or (:food-imputed-usd-micros-yr opts) DEFAULT-MITSUHO-MICROS-YR))
              (attach-hikari-r1-scaffold
               :energy-imputed-usd-micros-yr
               (or (:energy-imputed-usd-micros-yr opts) DEFAULT-HIKARI-MICROS-YR)))]
    (assoc e
           :care-mitsuho-hikari-full-chain-refused
           (boolean (and (:care-full-chain-refused e)
                         (:mitsuho-full-chain-refused e)
                         (:hikari-full-chain-refused e))))))

(defn enroll-with-multi-gen-substrate
  "L4 multi-gen substrate offline (care+housing first, then food+energy):
   L0 + disclosure + four membranes. All full-chains default refuse. land-grant never.
   Priority: wellbecoming > 孫 > 子."
  [opts]
  (let [e (-> (enroll opts)
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-housing-r1-scaffold
               :housing-imputed-usd-micros-yr
               (or (:housing-imputed-usd-micros-yr opts) DEFAULT-HOUSING-MICROS-YR))
              (attach-mitsuho-r1-scaffold
               :food-imputed-usd-micros-yr
               (or (:food-imputed-usd-micros-yr opts) DEFAULT-MITSUHO-MICROS-YR))
              (attach-hikari-r1-scaffold
               :energy-imputed-usd-micros-yr
               (or (:energy-imputed-usd-micros-yr opts) DEFAULT-HIKARI-MICROS-YR)))]
    (assoc e
           :care-housing-full-chain-refused
           (boolean (and (:care-full-chain-refused e)
                         (:housing-full-chain-refused e)))
           :care-housing-mitsuho-hikari-full-chain-refused
           (boolean (and (:care-full-chain-refused e)
                         (:housing-full-chain-refused e)
                         (:mitsuho-full-chain-refused e)
                         (:hikari-full-chain-refused e)))
           :land-grant-executed false)))

(defn attach-tooling-r1-scaffold
  "Vocation rail (robotics/itonami recovery): tooling-okaimono R1→gated DESIGN.
   Default refuse; fulfillment never executed. cash≡0."
  [enrolled & {:keys [tooling-imputed-usd-micros-yr]
               :or {tooling-imputed-usd-micros-yr DEFAULT-TOOLING-MICROS-YR}}]
  (let [subject-did (or (get-in enrolled [:vow :subject-did])
                        (get-in enrolled [:public-person :did]))
        hold (:disclosure-hold enrolled)
        person (or (:person enrolled)
                   {:did subject-did
                    :covenant "vowed"
                    :rails [{:kind "tooling" :active? true}]
                    :floor-usd-micros-yr tooling-imputed-usd-micros-yr
                    :disclosure (or (get-in enrolled [:person :disclosure])
                                    (default-disclosure))
                    :exit-suspended? (boolean (:disclosure-held? enrolled))
                    :stage "L0"
                    :cash-usd-micros 0})
        person (update person :rails
                       (fn [rs]
                         (let [rs (or rs [])]
                           (if (some #(= "tooling" (or (:kind %) %)) rs)
                             rs
                             (conj (vec rs) {:kind "tooling" :active? true})))))
        pkg (tooling/r1-dry-package
             {:alloc-id (str "l0-tooling-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr tooling-imputed-usd-micros-yr
              :person person
              :hold-machine hold})
        gated (when (and pkg (not= :refused (:phase pkg)))
                (tooling/gated-live-status pkg :hold-machine hold))
        recv (when (and pkg (not= :refused (:phase pkg)))
               (trecv/gated-receive-status pkg))
        prod (when (and pkg (not= :refused (:phase pkg)))
               (tprod/gated-produce-status pkg))
        full-chain-refused?
        (boolean
         (or (= :refused (:phase pkg))
             (and (some? gated) (some? recv) (some? prod)
                  (not (true? (:admissible gated)))
                  (not (true? (:admissible recv)))
                  (not (true? (:admissible prod))))))
        membrane {:tooling-r1-phase (when pkg (name (:phase pkg)))
                  :tooling-gated-admissible (boolean (:admissible gated))
                  :tooling-gated-receive-admissible (boolean (:admissible recv))
                  :tooling-gated-produce-admissible (boolean (:admissible prod))
                  :tooling-full-chain-refused full-chain-refused?
                  :fulfillment-executed false
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK
                  :note "L0 tooling-okaimono R1→gated DESIGN (vocation) — default refuse"}
        out (assoc enrolled
                   :tooling-package pkg
                   :tooling-gated-live-status gated
                   :tooling-gated-receive-status recv
                   :tooling-gated-produce-status prod
                   :tooling-membrane membrane
                   :tooling-full-chain-refused full-chain-refused?
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! membrane)
    (when pkg (pp/assert-no-public-scores! pkg))
    (doseq [st [gated recv prod]]
      (when st (pp/assert-no-public-scores! st)))
    out))

(defn attach-compute-r1-scaffold
  "Vocation rail (robotics/itonami recovery): compute-murakumo R1→gated DESIGN.
   Default refuse; quota never executed. cash≡0."
  [enrolled & {:keys [compute-imputed-usd-micros-yr]
               :or {compute-imputed-usd-micros-yr DEFAULT-COMPUTE-MICROS-YR}}]
  (let [subject-did (or (get-in enrolled [:vow :subject-did])
                        (get-in enrolled [:public-person :did]))
        hold (:disclosure-hold enrolled)
        person (or (:person enrolled)
                   {:did subject-did
                    :covenant "vowed"
                    :rails [{:kind "compute" :active? true}]
                    :floor-usd-micros-yr compute-imputed-usd-micros-yr
                    :disclosure (or (get-in enrolled [:person :disclosure])
                                    (default-disclosure))
                    :exit-suspended? (boolean (:disclosure-held? enrolled))
                    :stage "L0"
                    :cash-usd-micros 0})
        person (update person :rails
                       (fn [rs]
                         (let [rs (or rs [])]
                           (if (some #(= "compute" (or (:kind %) %)) rs)
                             rs
                             (conj (vec rs) {:kind "compute" :active? true})))))
        pkg (compute/r1-dry-package
             {:alloc-id (str "l0-compute-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr compute-imputed-usd-micros-yr
              :person person
              :hold-machine hold})
        gated (when (and pkg (not= :refused (:phase pkg)))
                (compute/gated-live-status pkg :hold-machine hold))
        recv (when (and pkg (not= :refused (:phase pkg)))
               (mrecv/gated-receive-status pkg))
        prod (when (and pkg (not= :refused (:phase pkg)))
               (cmpprod/gated-produce-status pkg))
        full-chain-refused?
        (boolean
         (or (= :refused (:phase pkg))
             (and (some? gated) (some? recv) (some? prod)
                  (not (true? (:admissible gated)))
                  (not (true? (:admissible recv)))
                  (not (true? (:admissible prod))))))
        tooling-compute-refused?
        (boolean (and (true? (:tooling-full-chain-refused enrolled))
                      full-chain-refused?))
        membrane {:compute-r1-phase (when pkg (name (:phase pkg)))
                  :compute-gated-admissible (boolean (:admissible gated))
                  :compute-gated-receive-admissible (boolean (:admissible recv))
                  :compute-gated-produce-admissible (boolean (:admissible prod))
                  :compute-full-chain-refused full-chain-refused?
                  :tooling-compute-full-chain-refused tooling-compute-refused?
                  :quota-executed false
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK
                  :note "L0 compute-murakumo R1→gated DESIGN (vocation) — default refuse"}
        out (assoc enrolled
                   :compute-package pkg
                   :compute-gated-live-status gated
                   :compute-gated-receive-status recv
                   :compute-gated-produce-status prod
                   :compute-membrane membrane
                   :compute-full-chain-refused full-chain-refused?
                   :tooling-compute-full-chain-refused tooling-compute-refused?
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! membrane)
    (when pkg (pp/assert-no-public-scores! pkg))
    (doseq [st [gated recv prod]]
      (when st (pp/assert-no-public-scores! st)))
    out))

(defn enroll-with-tooling
  "Offline L0 + disclosure + tooling vocation membrane (default refuse)."
  [opts]
  (attach-tooling-r1-scaffold (enroll opts)
                              :tooling-imputed-usd-micros-yr
                              (or (:tooling-imputed-usd-micros-yr opts)
                                  DEFAULT-TOOLING-MICROS-YR)))

(defn enroll-with-compute
  "Offline L0 + disclosure + compute vocation membrane (default refuse)."
  [opts]
  (attach-compute-r1-scaffold (enroll opts)
                              :compute-imputed-usd-micros-yr
                              (or (:compute-imputed-usd-micros-yr opts)
                                  DEFAULT-COMPUTE-MICROS-YR)))

(defn enroll-with-full-inkind-rails
  "All six in-kind rails offline (L4 multi-gen + vocation for itonami displacement):
   care+housing first, then food+energy, then tooling+compute.
   All full-chains default refuse. land-grant/fulfillment/quota never executed."
  [opts]
  (let [e (-> (enroll opts)
              (attach-care-r1-scaffold
               :care-imputed-usd-micros-yr
               (or (:care-imputed-usd-micros-yr opts) DEFAULT-CARE-MICROS-YR))
              (attach-housing-r1-scaffold
               :housing-imputed-usd-micros-yr
               (or (:housing-imputed-usd-micros-yr opts) DEFAULT-HOUSING-MICROS-YR))
              (attach-mitsuho-r1-scaffold
               :food-imputed-usd-micros-yr
               (or (:food-imputed-usd-micros-yr opts) DEFAULT-MITSUHO-MICROS-YR))
              (attach-hikari-r1-scaffold
               :energy-imputed-usd-micros-yr
               (or (:energy-imputed-usd-micros-yr opts) DEFAULT-HIKARI-MICROS-YR))
              (attach-tooling-r1-scaffold
               :tooling-imputed-usd-micros-yr
               (or (:tooling-imputed-usd-micros-yr opts) DEFAULT-TOOLING-MICROS-YR))
              (attach-compute-r1-scaffold
               :compute-imputed-usd-micros-yr
               (or (:compute-imputed-usd-micros-yr opts) DEFAULT-COMPUTE-MICROS-YR)))]
    (assoc e
           :care-housing-full-chain-refused
           (boolean (and (:care-full-chain-refused e)
                         (:housing-full-chain-refused e)))
           :care-housing-mitsuho-hikari-full-chain-refused
           (boolean (and (:care-full-chain-refused e)
                         (:housing-full-chain-refused e)
                         (:mitsuho-full-chain-refused e)
                         (:hikari-full-chain-refused e)))
           :tooling-compute-full-chain-refused
           (boolean (and (:tooling-full-chain-refused e)
                         (:compute-full-chain-refused e)))
           :all-inkind-produce-rails-full-chain-refused
           (boolean (and (:care-full-chain-refused e)
                         (:housing-full-chain-refused e)
                         (:mitsuho-full-chain-refused e)
                         (:hikari-full-chain-refused e)
                         (:tooling-full-chain-refused e)
                         (:compute-full-chain-refused e)))
           :land-grant-executed false)))

(defn attach-liquidity-r1-scaffold
  "Member-principal residual (warifu): R1 dry → gated-live + gated-receive DESIGN.
   No produce plan; loan never invoked; fuchi not creditor; cash≡0 always."
  [enrolled & {:keys [liquidity-imputed-usd-micros-yr]
               :or {liquidity-imputed-usd-micros-yr DEFAULT-LIQUIDITY-MICROS-YR}}]
  (let [subject-did (or (get-in enrolled [:vow :subject-did])
                        (get-in enrolled [:public-person :did]))
        hold (:disclosure-hold enrolled)
        person (or (:person enrolled)
                   {:did subject-did
                    :covenant "vowed"
                    :rails [{:kind "liquidity" :active? true}]
                    :floor-usd-micros-yr liquidity-imputed-usd-micros-yr
                    :disclosure (or (get-in enrolled [:person :disclosure])
                                    (default-disclosure))
                    :exit-suspended? (boolean (:disclosure-held? enrolled))
                    :stage "L0"
                    :cash-usd-micros 0})
        person (update person :rails
                       (fn [rs]
                         (let [rs (or rs [])]
                           (if (some #(= "liquidity" (or (:kind %) %)) rs)
                             rs
                             (conj (vec rs) {:kind "liquidity" :active? true})))))
        pkg (liquidity/r1-dry-package
             {:alloc-id (str "l0-liq-" subject-did)
              :subject-did subject-did
              :imputed-usd-micros-yr liquidity-imputed-usd-micros-yr
              :person person
              :hold-machine hold})
        gated (when (and pkg (not= :refused (:phase pkg)))
                (liquidity/gated-live-status pkg :hold-machine hold))
        recv (when (and pkg (not= :refused (:phase pkg)))
               (wrecv/gated-receive-status pkg))
        full-chain-refused?
        (boolean
         (or (= :refused (:phase pkg))
             (and (some? gated) (some? recv)
                  (not (true? (:admissible gated)))
                  (not (true? (:admissible recv))))))
        all-seven?
        (boolean (and (true? (:all-inkind-produce-rails-full-chain-refused enrolled))
                      full-chain-refused?))
        membrane {:liquidity-r1-phase (when pkg (name (:phase pkg)))
                  :liquidity-gated-admissible (boolean (:admissible gated))
                  :liquidity-gated-receive-admissible (boolean (:admissible recv))
                  :liquidity-receive-full-chain-refused full-chain-refused?
                  :liquidity-loan-executed false
                  :liquidity-member-principal true
                  :liquidity-cash-usd-micros 0
                  :all-seven-rails-receive-membrane-refused all-seven?
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :priority-stack PRIORITY-STACK
                  :note "L0 liquidity-warifu residual — member-principal; loan never; cash≡0"}
        out (assoc enrolled
                   :liquidity-package pkg
                   :liquidity-gated-live-status gated
                   :liquidity-gated-receive-status recv
                   :liquidity-membrane membrane
                   :liquidity-receive-full-chain-refused full-chain-refused?
                   :liquidity-loan-executed false
                   :liquidity-member-principal true
                   :liquidity-cash-usd-micros 0
                   :all-seven-rails-receive-membrane-refused all-seven?
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK)]
    (pp/assert-no-public-scores! membrane)
    (when pkg (pp/assert-no-public-scores! pkg))
    (doseq [st [gated recv]]
      (when st (pp/assert-no-public-scores! st)))
    (when-not (true? (:member-principal pkg true))
      (throw (ex-info "liquidity residual must be member-principal" pkg)))
    out))

(defn enroll-with-liquidity
  "Offline L0 + disclosure + liquidity residual membrane (member-principal; no loan)."
  [opts]
  (attach-liquidity-r1-scaffold (enroll opts)
                                :liquidity-imputed-usd-micros-yr
                                (or (:liquidity-imputed-usd-micros-yr opts)
                                    DEFAULT-LIQUIDITY-MICROS-YR)))

(defn enroll-with-all-seven-rails
  "Full covenantal SS rail set offline: six in-kind produce rails + liquidity residual.
   all-inkind + all-seven receive-membrane refuse by default. cash≡0; loan never."
  [opts]
  (-> (enroll-with-full-inkind-rails opts)
      (attach-liquidity-r1-scaffold
       :liquidity-imputed-usd-micros-yr
       (or (:liquidity-imputed-usd-micros-yr opts) DEFAULT-LIQUIDITY-MICROS-YR))))

(defn enroll-batch
  "Offline L0 for a vector of enroll option maps (e.g. displacement cohort stubs).
   Pure map; never live. Returns {:enrollments :live false ...}.
   Flags include :with-full-inkind-rails? and :with-all-seven-rails?."
  [opts-list & {:keys [with-care? with-housing? with-care-housing?
                       with-mitsuho? with-hikari? with-mitsuho-hikari?
                       with-care-mitsuho-hikari? with-multi-gen-substrate?
                       with-tooling? with-compute? with-full-inkind-rails?
                       with-liquidity? with-all-seven-rails?]
                :or {with-care? false with-housing? false with-care-housing? false
                     with-mitsuho? false with-hikari? false
                     with-mitsuho-hikari? false with-care-mitsuho-hikari? false
                     with-multi-gen-substrate? false
                     with-tooling? false with-compute? false
                     with-full-inkind-rails? false
                     with-liquidity? false with-all-seven-rails? false}}]
  (let [enrollments
        (mapv (fn [opts]
                (cond
                  with-all-seven-rails? (enroll-with-all-seven-rails opts)
                  with-full-inkind-rails? (enroll-with-full-inkind-rails opts)
                  with-multi-gen-substrate? (enroll-with-multi-gen-substrate opts)
                  with-care-mitsuho-hikari? (enroll-with-care-mitsuho-hikari opts)
                  with-care-housing? (enroll-with-care-housing opts)
                  with-mitsuho-hikari? (enroll-with-mitsuho-hikari opts)
                  (and with-mitsuho? with-hikari?) (enroll-with-mitsuho-hikari opts)
                  with-housing? (enroll-with-housing opts)
                  with-care? (enroll-with-care opts)
                  with-tooling? (enroll-with-tooling opts)
                  with-compute? (enroll-with-compute opts)
                  with-liquidity? (enroll-with-liquidity opts)
                  with-mitsuho? (enroll-with-mitsuho opts)
                  with-hikari? (enroll-with-hikari opts)
                  :else (enroll opts)))
              (or opts-list []))]
    {:path "l0-enroll-batch-offline"
     :count (count enrollments)
     :enrollments enrollments
     :disclosure-open
     (count (filter #(= :open (:disclosure-state %)) enrollments))
     :disclosure-held
     (count (filter :disclosure-held? enrollments))
     :care-full-chain-refused-n
     (count (filter :care-full-chain-refused enrollments))
     :housing-full-chain-refused-n
     (count (filter :housing-full-chain-refused enrollments))
     :care-housing-full-chain-refused-n
     (count (filter :care-housing-full-chain-refused enrollments))
     :mitsuho-full-chain-refused-n
     (count (filter :mitsuho-full-chain-refused enrollments))
     :hikari-full-chain-refused-n
     (count (filter :hikari-full-chain-refused enrollments))
     :tooling-full-chain-refused-n
     (count (filter :tooling-full-chain-refused enrollments))
     :compute-full-chain-refused-n
     (count (filter :compute-full-chain-refused enrollments))
     :liquidity-receive-full-chain-refused-n
     (count (filter :liquidity-receive-full-chain-refused enrollments))
     :mitsuho-hikari-full-chain-refused-n
     (count (filter :mitsuho-hikari-full-chain-refused enrollments))
     :care-mitsuho-hikari-full-chain-refused-n
     (count (filter :care-mitsuho-hikari-full-chain-refused enrollments))
     :care-housing-mitsuho-hikari-full-chain-refused-n
     (count (filter :care-housing-mitsuho-hikari-full-chain-refused enrollments))
     :tooling-compute-full-chain-refused-n
     (count (filter :tooling-compute-full-chain-refused enrollments))
     :all-inkind-produce-rails-full-chain-refused-n
     (count (filter :all-inkind-produce-rails-full-chain-refused enrollments))
     :all-seven-rails-receive-membrane-refused-n
     (count (filter :all-seven-rails-receive-membrane-refused enrollments))
     :any-land-grant-executed?
     (boolean (some :land-grant-executed enrollments))
     :any-liquidity-loan-executed?
     (boolean (some :liquidity-loan-executed enrollments))
     :total-liquidity-cash-usd-micros
     (reduce + 0 (map #(or (:liquidity-cash-usd-micros %) 0) enrollments))
     :live false
     :cash-usd-micros 0
     :score-surface []
     :priority-stack PRIORITY-STACK
     :multi-gen-facts MULTI-GEN-FACTS}))

(defn enroll-record
  "Lexicon-shaped record for com.etzhayyim.fuchi.commitmentVow (offline)."
  [enrolled]
  (let [v (:vow enrolled)]
    {:subjectDid (:subject-did v)
     :vowText (:vow-text v)
     :covenant (:covenant v)
     :stage (:stage v)
     :kotobaCid (:kotoba-cid v)
     :ipfsCid (:ipfs-cid v)
     :tokenId (:token-id v)
     :priorityStack (mapv name PRIORITY-STACK)
     :published false
     :cashUsdMicros 0
     :serverHeldKey false
     :memberSignature (:member-signature v)
     :multiGenNote (:multi-gen-note v)
     :disclosureState (when-let [s (:disclosure-state enrolled)] (name s))
     :entitlementsMayFlow (boolean (:entitlements-may-flow? enrolled))
     :live false}))
