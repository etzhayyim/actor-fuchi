(ns fuchi.methods.rail-hikari
  "rail_hikari.cljc — energy-hikari single-rail R1 → gated-live DESIGN.

  Sibling of food-mitsuho: renewable energy via hikari 光 for multi-gen wellbecoming.
  - R1 dry-run provisioning intent (cash≡0, published=false)
  - gated-live-plan after live_gate + disclosure open — no hikari produce call
  - disclosure held / exit → refuse
  - no personal scores
  Portable .cljc (design EDN load under bb + nbb)."
  (:require [fuchi.methods.provision :as provision]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.edn :as edn]))

(def RAIL-KIND "energy-hikari")
(def PROVIDER-DID "did:web:etzhayyim.com:actor:hikari")
(def PRIORITY-STACK pp/PRIORITY-STACK)

(def MULTI-GEN-FACTS
  ["kwh-floor-supports-household-care-and-learning"
   "energy-after-care-housing-for-mago-ko-substrate"
   "no-fossil-no-nuclear-constitutional"
   "not-a-happiness-score"
   "imputed-usd-is-accounting-fact-only"])

;; Portable design facts (no I/O). Mirrors data/rail-hikari-design.edn care-first slice.
(def DESIGN-PUBLIC-FACTS
  {:rail-kind RAIL-KIND
   :provider-did PROVIDER-DID
   :priority-stack PRIORITY-STACK
   :care-first-before-rails ["care" "housing"]
   :care-first-api-path "care-first-hikari-path"
   :multi-gen-facts MULTI-GEN-FACTS
   :live-produce false
   :generate-executed false
   :live false
   :cash-usd-micros 0
   :score-surface []
   :note "R1→gated DESIGN only; energy after care/housing for 孫/子"})

(defn design-public-facts
  "Facts-only projection of hikari single-rail design (no scores; cash≡0)."
  []
  (let [out DESIGN-PUBLIC-FACTS]
    (pp/assert-no-public-scores! (dissoc out :note :multi-gen-facts :care-first-before-rails
                                         :care-first-api-path :priority-stack))
    out))

(defn load-design
  "Load data/rail-hikari-design.edn. Portable under bb and nbb."
  []
  (edn/load-data "rail-hikari-design.edn"))

(defn design-edn-invariants
  "Facts-only checks against hikari design EDN: cash≡0, gated live=false, no scores."
  []
  (let [d (load-design)
        cash (or (get d ":rail/cash-usd-micros") (get d :rail/cash-usd-micros) 0)
        scores (or (get d ":rail/score-surface") (get d :rail/score-surface) [])
        gated (or (get-in d [":rail/phases" ":gated-live-design"])
                  (get-in d [:rail/phases :gated-live-design])
                  (get-in d [":rail/phases" :gated-live-design])
                  {})
        live (or (get gated ":live") (get gated :live) false)
        cfo (or (get d ":rail/care-first-order") (get d :rail/care-first-order) {})
        before (or (get cfo ":before-rails") (get cfo :before-rails) ["care" "housing"])
        api (or (get cfo ":api-path") (get cfo :api-path) "care-first-hikari-path")
        before-v (mapv str before)
        out {:rail-kind (or (get d ":rail/kind") (get d :rail/kind) RAIL-KIND)
             :cash-usd-micros cash
             :score-surface scores
             :gated-live false
             :live-produce false
             :design-live live
             :care-first-before-rails before-v
             :care-first-api-path (str api)
             :priority-stack PRIORITY-STACK}]
    (when-not (zero? (long cash))
      (throw (ex-info "design EDN cash≡0" out)))
    (when (true? live)
      (throw (ex-info "design EDN gated-live must stay live=false" out)))
    (when-not (= ["care" "housing"] before-v)
      (throw (ex-info "design EDN care-first-order before-rails must be care then housing"
                      out)))
    (pp/assert-no-public-scores!
     (dissoc out :care-first-before-rails :care-first-api-path :priority-stack))
    out))

(defn- assert-invariants! [m]
  (when-not (= 0 (or (:cash-usd-micros m) (:cashUsdMicros m) 0))
    (throw (ex-info "cash≡0 (G2)" m)))
  (when (or (true? (:server-held-key m)) (true? (:serverHeldKey m)))
    (throw (ex-info "no-server-key (G9)" m)))
  (when (or (true? (:published m)) (true? (:live m)))
    (throw (ex-info "G10: rail-hikari scaffold never publishes or goes live" m)))
  true)

(defn energy-rail [imputed-usd-micros-yr]
  {:kind RAIL-KIND
   :imputed-usd-micros-yr (long imputed-usd-micros-yr)
   :member-principal false})

(defn r1-dry-intent [alloc-id imputed-usd-micros-yr]
  (let [intents (provision/provision [(energy-rail imputed-usd-micros-yr)] alloc-id)
        i (first intents)]
    (when-not (= RAIL-KIND (:rail-kind i))
      (throw (ex-info "expected energy-hikari intent" {:got i})))
    (assert-invariants! i)
    (when-not (= PROVIDER-DID (:provider-did i))
      (throw (ex-info "provider must be hikari actor DID" {:provider (:provider-did i)})))
    i))

(defn- disclosure-state [person hold-machine]
  (cond
    hold-machine (name (:state hold-machine))
    (pp/exit-suspended? person) "exit-suspended"
    (and (pp/public-person? person) (not (pp/disclosure-ok? person))) "held"
    :else "open"))

(defn refuse-package [alloc-id subject-did imputed reason disclosure-state public-person?]
  {:alloc-id alloc-id :subject-did subject-did :rail-kind RAIL-KIND
   :provider-did PROVIDER-DID :imputed-usd-micros-yr (long imputed)
   :phase :refused :refusal-reason reason :disclosure-state disclosure-state
   :public-person? public-person? :priority-stack PRIORITY-STACK
   :multi-gen-facts MULTI-GEN-FACTS :authorized-to-publish false
   :published false :cash-usd-micros 0 :server-held-key false :live false
   :score-surface []})

(defn r1-dry-package
  [{:keys [alloc-id subject-did imputed-usd-micros-yr person hold-machine]
    :or {alloc-id "alloc-energy" imputed-usd-micros-yr 0}}]
  (let [person (or person {:did subject-did :covenant "vowed"
                           :rails [{:kind "energy" :active? true}]
                           :floor-usd-micros-yr imputed-usd-micros-yr
                           :disclosure {:wage-labor-band "0-10h" :state-benefits? false
                                        :wellbecoming-attest-fact :submitted
                                        :related-party-edges [] :rider-s2-self-report :none}
                           :exit-suspended? false})
        ds (disclosure-state person hold-machine)
        pp? (pp/public-person? person)
        hold? (or (= ds "held") (= ds "exit-suspended")
                  (and hold-machine (:entitlements-held? hold-machine)))]
    (if hold?
      (refuse-package alloc-id (or subject-did (:did person)) imputed-usd-micros-yr
                      (str "disclosure/entitlements not open (" ds ")") ds pp?)
      (let [intent (r1-dry-intent alloc-id imputed-usd-micros-yr)
            pkg {:alloc-id alloc-id
                 :subject-did (or subject-did (:did person))
                 :rail-kind RAIL-KIND :provider-did PROVIDER-DID
                 :imputed-usd-micros-yr (long imputed-usd-micros-yr)
                 :phase :R1-dry :intent intent :disclosure-state ds
                 :public-person? pp? :priority-stack PRIORITY-STACK
                 :multi-gen-facts MULTI-GEN-FACTS :authorized-to-publish false
                 :published false :cash-usd-micros 0 :server-held-key false
                 :live false :score-surface []}]
        (assert-invariants! pkg)
        (pp/assert-no-public-scores! pkg)
        pkg))))

(defn gated-live-plan
  [r1-pkg gate & {:keys [env hold-machine]}]
  (when (= :refused (:phase r1-pkg))
    (throw (ex-info "cannot plan live on refused R1 package" r1-pkg)))
  (let [ds (:disclosure-state r1-pkg)
        hold? (or (= ds "held") (= ds "exit-suspended")
                  (and hold-machine (#{:held :exit-suspended} (:state hold-machine))))]
    (when hold?
      (throw (ex-info (str "refuse gated-live: disclosure " ds) {:kind ::disclosure-hold})))
    (live-gate/require-gate gate env)
    (let [authorized (first (provision/dispatch-live [(:intent r1-pkg)] gate :env env))
          pkg (assoc r1-pkg :phase :gated-live-plan :authorized-to-publish true
                     :authorization authorized :published false :live false
                     :cash-usd-micros 0 :server-held-key false
                     :note "authorized plan only — hikari produce not invoked")]
      (when (or (:published pkg) (:live pkg) (not= 0 (:cash-usd-micros pkg)))
        (throw (ex-info "G2/G10" {})))
      (pp/assert-no-public-scores! pkg)
      pkg)))

(defn default-refuse-status []
  (live-gate/gate-status (live-gate/make-live-gate {:leg "provision"}) {}))

(defn gated-live-status
  "Non-raising R1→gated-live DESIGN status for energy-hikari.
   Default gate/env refuses. Never generates power; cash≡0; live false."
  [r1-pkg & {:keys [gate env hold-machine]}]
  (cond
    (nil? r1-pkg)
    nil

    (= :refused (:phase r1-pkg))
    {:rail-kind RAIL-KIND
     :provider-did PROVIDER-DID
     :phase :refused
     :r1-phase :refused
     :admissible false
     :authorized-to-publish false
     :generate-executed false
     :refusal-reason (or (:refusal-reason r1-pkg) "r1 refused")
     :disclosure-state (or (:disclosure-state r1-pkg) "n/a")
     :care-first-api-path "care-first-hikari-path"
     :care-first-before-rails ["care" "housing"]
     :multi-gen-facts MULTI-GEN-FACTS
     :live false
     :cash-usd-micros 0
     :score-surface []
     :priority-stack PRIORITY-STACK
     :note "R1 refused — gated-live not attempted"}

    :else
    (let [g (or gate (live-gate/make-live-gate {:leg "provision"}))
          e (or env {})]
      (try
        (let [plan (gated-live-plan r1-pkg g :env e :hold-machine hold-machine)
              out {:rail-kind RAIL-KIND
                   :provider-did PROVIDER-DID
                   :phase :gated-live-plan
                   :r1-phase (:phase r1-pkg)
                   :admissible true
                   :authorized-to-publish (boolean (:authorized-to-publish plan))
                   :generate-executed false
                   :published false
                   :disclosure-state (or (:disclosure-state r1-pkg) "open")
                   :care-first-api-path "care-first-hikari-path"
                   :care-first-before-rails ["care" "housing"]
                   :multi-gen-facts MULTI-GEN-FACTS
                   :live false
                   :cash-usd-micros 0
                   :score-surface []
                   :priority-stack PRIORITY-STACK
                   :note "gated-live plan authorized — hikari generate not invoked"}]
          (pp/assert-no-public-scores! (dissoc out :multi-gen-facts :care-first-before-rails))
          out)
        (catch #?(:clj Exception :cljs :default) ex
          (let [st (live-gate/gate-status g e)
                out {:rail-kind RAIL-KIND
                     :provider-did PROVIDER-DID
                     :phase :refused
                     :r1-phase (:phase r1-pkg)
                     :admissible false
                     :authorized-to-publish false
                     :generate-executed false
                     :refusal-reason (or (ex-message ex)
                                         (get st "reason")
                                         "live gate default refuse")
                     :disclosure-state (or (:disclosure-state r1-pkg) "open")
                     :gate-admissible (boolean (get st "admissible"))
                     :care-first-api-path "care-first-hikari-path"
                     :care-first-before-rails ["care" "housing"]
                     :multi-gen-facts MULTI-GEN-FACTS
                     :live false
                     :cash-usd-micros 0
                     :score-surface []
                     :priority-stack PRIORITY-STACK
                     :note "R1 dry ok; gated-live refused by default membrane"}]
            (pp/assert-no-public-scores! (dissoc out :multi-gen-facts :care-first-before-rails))
            out))))))

(defn lexicon-record [pkg]
  {:allocId (:alloc-id pkg) :subjectDid (:subject-did pkg)
   :railKind RAIL-KIND :providerDid PROVIDER-DID
   :imputedUsdMicrosYr (:imputed-usd-micros-yr pkg)
   :phase (name (:phase pkg)) :refusalReason (or (:refusal-reason pkg) "")
   :disclosureState (or (:disclosure-state pkg) "n/a")
   :publicPerson (boolean (:public-person? pkg))
   :priorityStack (mapv name PRIORITY-STACK) :multiGenFacts MULTI-GEN-FACTS
   :authorizedToPublish (boolean (:authorized-to-publish pkg))
   :published false :cashUsdMicros 0 :serverHeldKey false :live false
   :scoreSurface []})
