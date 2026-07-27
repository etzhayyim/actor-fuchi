(ns fuchi.methods.public-surface-report
  "public_surface_report.cljc — facts-only public surface for covenantal SS (ADR-2607177000).

  Emits markdown + EDN maps of public-person facts from seed. NEVER includes
  priority-rank, share, weight, scores, or percentiles.
  Covers all in-kind rails + dry floor plans + itonami displacement facts.
  Portable .cljc; pure report builders + seed load under bb/nbb (ADR-2607173000)."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.rail-liquidity-warifu :as liquidity]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.hikari-produce-plan :as hprod]
            [fuchi.methods.care-iyashi-produce-plan :as cprod]
            [fuchi.methods.housing-commons-produce-plan :as housprod]
            [fuchi.methods.tooling-okaimono-produce-plan :as tprod]
            [fuchi.methods.compute-murakumo-produce-plan :as cmpprod]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.displacement-surface :as disp]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.displacement-l0-path :as dl0]
            [fuchi.methods.displacement-tenure :as ten]
            [fuchi.methods.displacement-gov :as dgov]
            [fuchi.methods.displacement-scorecard :as dsc]
            [fuchi.methods.ss-offline-path :as ss-path]
            [fuchi.methods.pipeline-audit-ledger :as audit]
            [fuchi.methods.priority-stack :as pstack]
            [fuchi.methods.edn :as edn]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn- pages-deploy-refuse-status
  "Default Pages deploy membrane status (no require of pages-deploy — cycle-safe).
   Includes plan-only design facts. Does not deploy. cash≡0. live=false."
  []
  (let [runbook {:flag "FUCHI_ALLOW_PAGES_DEPLOY"
                 :required-for-gated-plan ["FUCHI_ALLOW_PAGES_DEPLOY=1" "operator-did non-blank"]
                 :scaffold-invokes-wrangler false
                 :scaffold-invokes-cloudflare-api false
                 :side-effect-execute "out-of-band only"
                 :live-disbursement false
                 :deployed false
                 :live false
                 :cash-usd-micros 0
                 :score-surface []
                 :priority-stack PRIORITY-STACK
                 :steps ["write-deploy-package! → refresh public/ static facts"
                         "review index.html / facts.edn (no personal scores)"
                         "optional gated plan: FUCHI_ALLOW_PAGES_DEPLOY=1 + operator-did"
                         "out-of-band: wrangler pages deploy public/"
                         "never enable live sustenance from this package"]
                 :note "plan-only membrane; actual deploy is operator out-of-band"}
        out {:phase :refused
             :deploy-target "cloudflare-pages"
             :admissible false
             :authorized-to-deploy false
             :package-ready true
             :operator-flag "FUCHI_ALLOW_PAGES_DEPLOY"
             :refusal-reason "missing operator process flag 'FUCHI_ALLOW_PAGES_DEPLOY'"
             :wrangler-invoked false
             :cloudflare-api-invoked false
             :operator-runbook runbook
             :deployed false
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "Pages deploy default refuse — static package only; plan-only membrane"}]
    (pp/assert-no-public-scores! (dissoc out :operator-runbook))
    (pp/assert-no-public-scores! runbook)
    out))

(defn- last-seg [s]
  (last (str/split (str s) #":")))

(defn- line* [e]
  (-> (str (or (get e ":envelope/line") (get e :envelope/line) ""))
      (str/replace #"^:" "")
      (str/split #"/")
      last
      str/lower-case))

(defn- imp-for [envs line]
  (reduce + 0 (map #(long (or (get % ":envelope/imputed-usd-micros-yr")
                              (get % :envelope/imputed-usd-micros-yr)
                              0))
                   (filter #(= line (line* %)) envs))))

(defn person-fact-row
  "One public fact row (map). Strips scores."
  [surf]
  (let [row {:did (:did surf)
             :public-person? (boolean (:public-person? surf))
             :covenant (:covenant surf)
             :stage (or (:stage surf) "L2")
             :rails (vec (or (:rails surf) []))
             :imputed-fact (or (:imputed-fact surf) 0)
             :disclosure-status (or (:disclosure-status surf)
                                    (get-in surf [:disclosure-gate :action]))
             :hold-reason-code (:hold-reason-code surf)
             :priority-stack PRIORITY-STACK
             :score-surface []}]
    (pp/assert-no-public-scores! row)
    row))

(defn seed-public-facts
  "All public-person fact rows from a seed graph."
  [seed]
  (mapv person-fact-row (pp/persons-from-seed seed)))

(defn- phase-of [pkg]
  (when pkg (name (:phase pkg))))

(defn- floor-facts
  "Extract non-score floor fields from a dry produce plan (facts only)."
  [plan]
  (when (and plan (not= :refused (:phase plan)))
    (cond-> {:phase (name (:phase plan))
             :produce-executed (boolean (:produce-executed plan false))
             :live false
             :cash-usd-micros 0
             :score-surface []}
      (:kcal-floor-yr plan) (assoc :kcal-floor-yr (:kcal-floor-yr plan))
      (:kwh-floor-yr plan) (assoc :kwh-floor-yr (:kwh-floor-yr plan))
      (:care-hours-floor-yr plan) (assoc :care-hours-floor-yr (:care-hours-floor-yr plan))
      (:housing-months-floor-yr plan) (assoc :housing-months-floor-yr (:housing-months-floor-yr plan))
      (:tool-units-floor-yr plan) (assoc :tool-units-floor-yr (:tool-units-floor-yr plan))
      (:gpu-hours-floor-yr plan) (assoc :gpu-hours-floor-yr (:gpu-hours-floor-yr plan)))))

(defn- safe-plan [plan-fn pkg]
  (when (and pkg (not= :refused (:phase pkg)))
    (try (plan-fn pkg) (catch #?(:clj Exception :cljs :default) _ nil))))

(defn inkind-rail-packages
  "R1 dry packages + dry floor plans for all in-kind rails (facts only; no live execute)."
  [seed]
  (let [surfs (pp/persons-from-seed seed)
        recs (get seed ":maintainer/batch" [])
        env-of (fn [did]
                 (filterv #(= (get % ":envelope/maintainer") did)
                          (get seed ":envelope/batch" [])))]
    (vec
     (for [surf surfs
           :let [did (:did surf)
                 rec (first (filter #(= (get % ":maintainer/did") did) recs))
                 envs (env-of did)
                 food-imp (imp-for envs "food")
                 energy-imp (imp-for envs "energy")
                 care-imp (imp-for envs "care")
                 housing-imp (imp-for envs "housing")
                 tooling-imp (imp-for envs "tooling")
                 compute-imp (imp-for envs "compute")
                 liquidity-imp (imp-for envs "liquidity")
                 drec (pp/disclosure-for-did seed did)
                 person (pp/persons-from-seed-row rec envs drec)
                 hm (dh/from-seed-person person)
                 food-pkg (when (pos? food-imp)
                            (mitsuho/r1-dry-package
                             {:alloc-id (str "food-" (last-seg did)) :subject-did did
                              :imputed-usd-micros-yr food-imp :person person :hold-machine hm}))
                 energy-pkg (when (pos? energy-imp)
                              (hikari/r1-dry-package
                               {:alloc-id (str "energy-" (last-seg did)) :subject-did did
                                :imputed-usd-micros-yr energy-imp :person person :hold-machine hm}))
                 care-pkg (when (pos? care-imp)
                            (care/r1-dry-package
                             {:alloc-id (str "care-" (last-seg did)) :subject-did did
                              :imputed-usd-micros-yr care-imp :person person :hold-machine hm}))
                 housing-pkg (when (pos? housing-imp)
                               (housing/r1-dry-package
                                {:alloc-id (str "housing-" (last-seg did)) :subject-did did
                                 :imputed-usd-micros-yr housing-imp :person person :hold-machine hm}))
                 tooling-pkg (when (pos? tooling-imp)
                               (tooling/r1-dry-package
                                {:alloc-id (str "tooling-" (last-seg did)) :subject-did did
                                 :imputed-usd-micros-yr tooling-imp :person person :hold-machine hm}))
                 compute-pkg (when (pos? compute-imp)
                               (compute/r1-dry-package
                                {:alloc-id (str "compute-" (last-seg did)) :subject-did did
                                 :imputed-usd-micros-yr compute-imp :person person :hold-machine hm}))
                 liquidity-pkg (when (pos? liquidity-imp)
                                 (liquidity/r1-dry-package
                                  {:alloc-id (str "liquidity-" (last-seg did)) :subject-did did
                                   :imputed-usd-micros-yr liquidity-imp :person person :hold-machine hm}))]
           :when (or (pos? food-imp) (pos? energy-imp) (pos? care-imp) (pos? housing-imp)
                     (pos? tooling-imp) (pos? compute-imp) (pos? liquidity-imp))]
       (let [row {:did did
                  :disclosure-state (name (:state hm))
                  :food food-pkg
                  :food-floor (floor-facts (safe-plan mprod/plan-from-r1 food-pkg))
                  :energy energy-pkg
                  :energy-floor (floor-facts (safe-plan hprod/plan-from-r1 energy-pkg))
                  :care care-pkg
                  :care-floor (floor-facts (safe-plan cprod/plan-from-r1 care-pkg))
                  :housing housing-pkg
                  :housing-floor (floor-facts (safe-plan housprod/plan-from-r1 housing-pkg))
                  :tooling tooling-pkg
                  :tooling-floor (floor-facts (safe-plan tprod/plan-from-r1 tooling-pkg))
                  :compute compute-pkg
                  :compute-floor (floor-facts (safe-plan cmpprod/plan-from-r1 compute-pkg))
                  :liquidity liquidity-pkg
                  :cash-usd-micros 0
                  :live false
                  :score-surface []
                  :priority-stack PRIORITY-STACK}]
         (pp/assert-no-public-scores! row)
         row)))))

(defn food-energy-packages
  "Backward-compatible alias: food+energy subset of inkind-rail-packages."
  [seed]
  (mapv (fn [r]
          (select-keys r [:did :disclosure-state :food :energy]))
        (inkind-rail-packages seed)))

(defn l0-demo-fact
  "Optional L0 enroll demo fact for a DID (offline).
   Includes disclosure hold/continuity facts from enroll scaffold (priorities 1+2)."
  [subject-did]
  (let [e (l0/enroll {:subject-did subject-did
                      :vow-text "demo L0 for public surface report"
                      :member-signature (str "sig-" (last-seg subject-did))
                      :covenant "outreach"})]
    {:did subject-did
     :stage "L0"
     :path (or (:path e) "l0-enroll-offline")
     :public-person? (get-in e [:public-person :public-person?])
     :token-stub (get-in e [:vow :token-id])
     :kotoba-cid-stub (get-in e [:vow :kotoba-cid])
     :disclosure-state (when-let [s (:disclosure-state e)] (name s))
     :disclosure-held (boolean (:disclosure-held? e))
     :entitlements-may-flow (boolean (:entitlements-may-flow? e))
     :cash-usd-micros 0
     :live false
     :score-surface []
     :priority-stack PRIORITY-STACK}))

(defn ss-priority-path-public-fact
  "Facts-only projection of ss_offline_path priority (1)(2)(3) demo.
   L0 enroll + disclosure continuity + all rails gated-live DESIGN refuse + R2 refuse.
   No personal scores. cash≡0. live=false."
  ([]
   (ss-priority-path-public-fact
    "did:web:etzhayyim.com:member:ss-priority-demo"))
  ([subject-did]
   (let [path (ss-path/run-food-path
               {:subject-did subject-did
                :food-imputed-usd-micros-yr 2000000000
                :energy-imputed-usd-micros-yr 1500000000
                :care-imputed-usd-micros-yr 1000000000
                :housing-imputed-usd-micros-yr 12000000000
                :tooling-imputed-usd-micros-yr 500000000
                :compute-imputed-usd-micros-yr 800000000
                :liquidity-imputed-usd-micros-yr 1500000000
                :include-disclosure-stress true})
         s (or (:priority-path-summary path) {})
         out {:path (:path path)
              :did subject-did
              :l0-stage (:l0-stage s)
              :l0-published (boolean (:l0-published s))
              :l0-token-stub (:l0-token-stub s)
              :l0-disclosure-state (:l0-disclosure-state s)
              :l0-disclosure-held (boolean (:l0-disclosure-held s))
              :l0-entitlements-may-flow (boolean (:l0-entitlements-may-flow s true))
              :l0-path (:l0-path s)
              :ladder-phase (:ladder-phase s)
              :ladder-from (:ladder-from s)
              :ladder-to (:ladder-to s)
              :ladder-target (:ladder-target s)
              :ladder-steps (or (:ladder-steps s) 0)
              :ladder-rails-hint (vec (or (:ladder-rails-hint s) []))
              :ladder-rails-hint-first (:ladder-rails-hint-first s)
              :ladder-multi-gen-fact (:ladder-multi-gen-fact s)
              :ladder-published false
              :held-stress-ladder-refused (boolean (:held-stress-ladder-refused s))
              :stage-sustenance-stage (:stage-sustenance-stage s)
              :stage-rails (vec (or (:stage-rails s) []))
              :stage-rails-first (:stage-rails-first s)
              :stage-rails-second (:stage-rails-second s)
              :stage-floor-usd-micros-yr (or (:stage-floor-usd-micros-yr s) 0)
              :stage-care-hours-floor-yr (or (:stage-care-hours-floor-yr s) 0)
              :stage-housing-months-floor-yr (or (:stage-housing-months-floor-yr s) 0)
              :stage-land-grant-executed (boolean (:stage-land-grant-executed s))
              :stage-r2-all-refused (boolean (:stage-r2-all-refused s))
              :stage-gated-count (or (:stage-gated-count s) 0)
              :stage-gated-admissible-count (or (:stage-gated-admissible-count s) 0)
              :stage-all-gated-refused (boolean (:stage-all-gated-refused s))
              :stage-care-gated-admissible (boolean (:stage-care-gated-admissible s))
              :stage-mitsuho-gated-admissible (boolean (:stage-mitsuho-gated-admissible s))
              :stage-hikari-gated-admissible (boolean (:stage-hikari-gated-admissible s))
              :stage-live false
              :disclosure-state (:disclosure-state s)
              :entitlements-may-flow? (boolean (:entitlements-may-flow? s))
              :continuity-action (when-let [a (:continuity-action s)]
                                   (if (keyword? a) (name a) (str a)))
              :held-stress-held? (boolean (:held-stress-held? s))
              :held-stress-food-phase (:held-stress-food-phase s)
              ;; priority (3) multi-gen substrate DESIGN first (care → housing → food/energy)
              :care-live-produce (boolean (:care-live-produce s))
              :care-care-first-api-path
              (or (:care-care-first-api-path s) "care-housing-first-path")
              :care-care-first-order-rank
              (or (:care-care-first-order-rank s) 1)
              :care-design-rail-kind
              (or (get-in s [:care-design :rail-kind]) "care-iyashi")
              :care-delivery-executed false
              :housing-live-produce (boolean (:housing-live-produce s))
              :housing-care-first-api-path
              (or (:housing-care-first-api-path s) "care-housing-first-path")
              :housing-care-first-before-rails
              (or (:housing-care-first-before-rails s) ["care"])
              :housing-care-first-order-rank
              (or (:housing-care-first-order-rank s) 2)
              :housing-design-rail-kind
              (or (get-in s [:housing-design :rail-kind]) "housing-commons")
              :mitsuho-r1-phase (:mitsuho-r1-phase s)
              :mitsuho-gated-admissible (boolean (:mitsuho-gated-admissible s))
              :mitsuho-produce-executed false
              :mitsuho-live-produce (boolean (:mitsuho-live-produce s))
              :mitsuho-care-first-api-path
              (or (:mitsuho-care-first-api-path s) "care-first-mitsuho-path")
              :mitsuho-care-first-before-rails
              (or (:mitsuho-care-first-before-rails s) ["care" "housing"])
              :mitsuho-design-rail-kind
              (or (get-in s [:mitsuho-design :rail-kind]) "food-mitsuho")
              :mitsuho-gated-receive-admissible
              (boolean (:mitsuho-gated-receive-admissible s))
              :mitsuho-gated-receive-phase (:mitsuho-gated-receive-phase s)
              :hikari-r1-phase (:hikari-r1-phase s)
              :hikari-gated-admissible (boolean (:hikari-gated-admissible s))
              :hikari-generate-executed false
              :hikari-live-produce (boolean (:hikari-live-produce s))
              :hikari-care-first-api-path
              (or (:hikari-care-first-api-path s) "care-first-hikari-path")
              :hikari-care-first-before-rails
              (or (:hikari-care-first-before-rails s) ["care" "housing"])
              :hikari-design-rail-kind
              (or (get-in s [:hikari-design :rail-kind]) "energy-hikari")
              :hikari-gated-receive-admissible
              (boolean (:hikari-gated-receive-admissible s))
              :hikari-gated-receive-phase (:hikari-gated-receive-phase s)
              ;; vocation + residual DESIGN (after multi-gen + food/energy)
              :tooling-live-produce (boolean (:tooling-live-produce s))
              :tooling-fulfillment-executed false
              :tooling-care-first-api-path
              (or (:tooling-care-first-api-path s) "vocation-recovery-path")
              :tooling-design-rail-kind
              (or (get-in s [:tooling-design :rail-kind]) "tooling-okaimono")
              :compute-live-produce (boolean (:compute-live-produce s))
              :compute-quota-executed false
              :compute-care-first-api-path
              (or (:compute-care-first-api-path s) "vocation-recovery-path")
              :compute-design-rail-kind
              (or (get-in s [:compute-design :rail-kind]) "compute-murakumo")
              :liquidity-live-produce (boolean (:liquidity-live-produce s))
              :liquidity-care-first-api-path
              (or (:liquidity-care-first-api-path s) "liquidity-residual-path")
              :liquidity-design-rail-kind
              (or (get-in s [:liquidity-design :rail-kind]) "liquidity-warifu")
              :all-seven-design-embed-count
              (or (:all-seven-design-embed-count s) 7)
              :all-seven-design-live-produce-never
              (boolean (:all-seven-design-live-produce-never s true))
              :mitsuho-hikari-receive-both-refused
              (boolean (:mitsuho-hikari-receive-both-refused s))
              :mitsuho-gated-produce-admissible
              (boolean (:mitsuho-gated-produce-admissible s))
              :mitsuho-gated-produce-phase (:mitsuho-gated-produce-phase s)
              :hikari-gated-produce-admissible
              (boolean (:hikari-gated-produce-admissible s))
              :hikari-gated-produce-phase (:hikari-gated-produce-phase s)
              :mitsuho-hikari-produce-both-refused
              (boolean (:mitsuho-hikari-produce-both-refused s))
              :mitsuho-hikari-full-chain-refused
              (boolean (:mitsuho-hikari-full-chain-refused s))
              :care-gated-receive-admissible
              (boolean (:care-gated-receive-admissible s))
              :care-gated-receive-phase (:care-gated-receive-phase s)
              :care-mitsuho-hikari-receive-all-refused
              (boolean (:care-mitsuho-hikari-receive-all-refused s))
              :care-gated-produce-admissible
              (boolean (:care-gated-produce-admissible s))
              :care-gated-produce-phase (:care-gated-produce-phase s)
              :care-mitsuho-hikari-produce-all-refused
              (boolean (:care-mitsuho-hikari-produce-all-refused s))
              :care-mitsuho-hikari-full-chain-refused
              (boolean (:care-mitsuho-hikari-full-chain-refused s))
              :care-r1-phase (:care-r1-phase s)
              :care-gated-admissible (boolean (:care-gated-admissible s))
              :housing-r1-phase (:housing-r1-phase s)
              :housing-gated-admissible (boolean (:housing-gated-admissible s))
              :housing-gated-receive-admissible
              (boolean (:housing-gated-receive-admissible s))
              :housing-gated-receive-phase (:housing-gated-receive-phase s)
              :housing-gated-produce-admissible
              (boolean (:housing-gated-produce-admissible s))
              :housing-gated-produce-phase (:housing-gated-produce-phase s)
              :housing-full-chain-refused
              (boolean (:housing-full-chain-refused s))
              :care-housing-mitsuho-hikari-receive-all-refused
              (boolean (:care-housing-mitsuho-hikari-receive-all-refused s))
              :care-housing-mitsuho-hikari-produce-all-refused
              (boolean (:care-housing-mitsuho-hikari-produce-all-refused s))
              :care-housing-mitsuho-hikari-full-chain-refused
              (boolean (:care-housing-mitsuho-hikari-full-chain-refused s))
              :housing-land-grant-executed false
              :tooling-r1-phase (:tooling-r1-phase s)
              :tooling-gated-admissible (boolean (:tooling-gated-admissible s))
              :tooling-gated-receive-admissible
              (boolean (:tooling-gated-receive-admissible s))
              :tooling-gated-receive-phase (:tooling-gated-receive-phase s)
              :tooling-gated-produce-admissible
              (boolean (:tooling-gated-produce-admissible s))
              :tooling-gated-produce-phase (:tooling-gated-produce-phase s)
              :tooling-full-chain-refused
              (boolean (:tooling-full-chain-refused s))
              :compute-r1-phase (:compute-r1-phase s)
              :compute-gated-admissible (boolean (:compute-gated-admissible s))
              :compute-gated-receive-admissible
              (boolean (:compute-gated-receive-admissible s))
              :compute-gated-receive-phase (:compute-gated-receive-phase s)
              :compute-gated-produce-admissible
              (boolean (:compute-gated-produce-admissible s))
              :compute-gated-produce-phase (:compute-gated-produce-phase s)
              :compute-full-chain-refused
              (boolean (:compute-full-chain-refused s))
              :tooling-compute-receive-both-refused
              (boolean (:tooling-compute-receive-both-refused s))
              :tooling-compute-produce-both-refused
              (boolean (:tooling-compute-produce-both-refused s))
              :tooling-compute-full-chain-refused
              (boolean (:tooling-compute-full-chain-refused s))
              :all-inkind-produce-rails-full-chain-refused
              (boolean (:all-inkind-produce-rails-full-chain-refused s))
              :liquidity-r1-phase (:liquidity-r1-phase s)
              :liquidity-gated-admissible (boolean (:liquidity-gated-admissible s))
              :liquidity-gated-receive-admissible
              (boolean (:liquidity-gated-receive-admissible s))
              :liquidity-gated-receive-phase (:liquidity-gated-receive-phase s)
              :liquidity-receive-full-chain-refused
              (boolean (:liquidity-receive-full-chain-refused s))
              :all-seven-rails-receive-membrane-refused
              (boolean (:all-seven-rails-receive-membrane-refused s))
              :liquidity-loan-executed false
              :liquidity-member-principal (boolean (:liquidity-member-principal s true))
              :liquidity-cash-usd-micros 0
              :rails-gated-count (or (:rails-gated-count s) 0)
              :rails-gated-admissible-count (or (:rails-gated-admissible-count s) 0)
              :all-rails-gated-refused (boolean (:all-rails-gated-refused s))
              :r2-status-count (or (:r2-status-count s) 0)
              :r2-executed-count (or (:r2-executed-count s) 0)
              :all-r2-not-executed (boolean (:all-r2-not-executed s true))
              :rail-gated (or (:rail-gated s) {})
              :r2-food-phase (:r2-food-phase s)
              :r2-food-executed (boolean (:r2-food-executed s))
              :r2-energy-phase (:r2-energy-phase s)
              :r2-energy-executed (boolean (:r2-energy-executed s))
              :public-person? (boolean (get-in path [:public-person :public-person?]))
              :live false
              :cash-usd-micros 0
              :score-surface []
              :priority-stack PRIORITY-STACK
              :note "priority path offline — L0 + disclosure + all rails gated refuse + R2 refuse"}]
     (pp/assert-no-public-scores!
      (dissoc out :rail-gated :note
              :care-care-first-api-path
              :housing-care-first-before-rails :housing-care-first-api-path
              :mitsuho-care-first-before-rails :hikari-care-first-before-rails
              :tooling-care-first-api-path :compute-care-first-api-path
              :liquidity-care-first-api-path
              :care-design-rail-kind :housing-design-rail-kind
              :mitsuho-design-rail-kind :hikari-design-rail-kind
              :tooling-design-rail-kind :compute-design-rail-kind
              :liquidity-design-rail-kind
              :ladder-rails-hint :stage-rails))
     out)))

(defn displacement-l0-public-summary
  "Facts-only projection of displacement→L0/L4 batch (no subject scores).
   Includes disclosure open/held and mitsuho/hikari R1→gated refuse facts when present."
  [batch]
  (let [subjects (mapcat :subjects (or (:packages batch) []))
        pkgs (mapv
              (fn [p]
                (let [subs (:subjects p)
                      ten-subs (or (:tenure-subjects p) [])
                      stages (frequencies (map :stage subs))
                      c (:couple p)
                      tc (:tenure-couple p)
                      open-n (count (filter #(or (true? (:entitlements-may-flow? %))
                                                 (= :open (:disclosure-state %))
                                                 (= :open (get-in % [:disclosure-hold :state])))
                                            subs))
                      held-n (count (filter #(or (true? (:disclosure-held? %))
                                                 (= :held (:disclosure-state %))
                                                 (false? (:entitlements-may-flow? %)))
                                            subs))
                      c-pre (:couple-pre-gov p)
                      row {:cohort-id (:cohort-id p)
                           :displacing-actor (:displacing-actor p)
                           :phase (name (:phase p))
                           :subject-count (count subs)
                           :stages stages
                           ;; flowable-first committed (housing held under Council excluded)
                           :committed-usd-micros-yr (or (:committed-usd-micros-yr c) 0)
                           :committed-full-usd-micros-yr
                           (or (:committed-full-usd-micros-yr c)
                               (:committed-usd-micros-yr c-pre)
                               (:committed-usd-micros-yr c)
                               0)
                           :earmark-usd-micros-yr (or (:earmark-usd-micros-yr c)
                                                      (get-in p [:earmark :earmark-usd-micros-yr])
                                                      0)
                           :headroom-usd-micros-yr (or (:headroom-usd-micros-yr c) 0)
                           :gov-flowable-usd-micros
                           (or (:gov-flowable-committed-usd-micros p) 0)
                           :gov-post-ratify-usd-micros
                           (or (:gov-post-ratify-committed-usd-micros p) 0)
                           ;; L6 tenure path (when batch includes tenure + gov package)
                           :tenure-phase (when (:tenure-phase p) (name (:tenure-phase p)))
                           :tenure-subjects (count ten-subs)
                           :tenure-g2 (boolean (and tc (true? (:admissible tc))))
                           :tenure-committed-usd-micros-yr
                           (or (:committed-usd-micros-yr tc) 0)
                           :tenure-committed-full-usd-micros-yr
                           (or (:committed-full-usd-micros-yr tc)
                               (get-in p [:tenure-couple-pre-gov :committed-usd-micros-yr])
                               0)
                           :tenure-gov-flowable-usd-micros
                           (or (:tenure-gov-flowable-committed-usd-micros p) 0)
                           :tenure-gov-post-ratify-usd-micros
                           (or (:tenure-gov-post-ratify-committed-usd-micros p) 0)
                           ;; refused / missing couple → not G2-admissible (no free-riding)
                           :g2-admissible (boolean (and c (true? (:admissible c))))
                           :funded (boolean (or (:funded c)
                                               (get-in p [:earmark :funded])
                                               (get-in p [:couple :funded])))
                           :disclosure-open open-n
                           :disclosure-held held-n
                           :mitsuho-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:food-package :phase])) subs))
                           :mitsuho-gated-refused
                           (count (filter #(false? (get-in % [:food-gated-live-status :admissible]))
                                          (filter :food-gated-live-status subs)))
                           :hikari-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:energy-package :phase])) subs))
                           :hikari-gated-refused
                           (count (filter #(false? (get-in % [:energy-gated-live-status :admissible]))
                                          (filter :energy-gated-live-status subs)))
                           :care-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:care-package :phase])) subs))
                           :care-gated-refused
                           (count (filter #(false? (get-in % [:care-gated-live-status :admissible]))
                                          (filter :care-gated-live-status subs)))
                           :housing-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:housing-package :phase])) subs))
                           :housing-gated-refused
                           (count (filter #(false? (get-in % [:housing-gated-live-status :admissible]))
                                          (filter :housing-gated-live-status subs)))
                           :housing-land-grant-executed
                           (count (filter #(true? (get-in % [:housing-gated-live-status :land-grant-executed]))
                                          (filter :housing-gated-live-status subs)))
                           :tooling-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:tooling-package :phase])) subs))
                           :tooling-gated-refused
                           (count (filter #(false? (get-in % [:tooling-gated-live-status :admissible]))
                                          (filter :tooling-gated-live-status subs)))
                           :compute-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:compute-package :phase])) subs))
                           :compute-gated-refused
                           (count (filter #(false? (get-in % [:compute-gated-live-status :admissible]))
                                          (filter :compute-gated-live-status subs)))
                           :liquidity-r1-dry
                           (count (filter #(= :R1-dry (get-in % [:liquidity-package :phase])) subs))
                           :liquidity-gated-refused
                           (count (filter #(false? (get-in % [:liquidity-gated-live-status :admissible]))
                                          (filter :liquidity-gated-live-status subs)))
                           :liquidity-member-principal
                           (count (filter #(true? (get-in % [:liquidity-package :member-principal]))
                                          subs))
                           :liquidity-loan-executed
                           (count (filter #(true? (get-in % [:liquidity-gated-live-status :loan-executed]))
                                          (filter :liquidity-gated-live-status subs)))
                           :liquidity-cash-usd-micros
                           (reduce + 0 (map #(or (get-in % [:liquidity-package :cash-usd-micros]) 0) subs))
                           ;; L0 subject membranes (gated DESIGN; default refuse; seven-rail)
                           :membrane-subjects
                           (count (filter :membrane-summary (concat subs ten-subs)))
                           :held-stress-subjects
                           (or (:held-stress-subjects p)
                               (count (filter :held-stress (concat subs ten-subs))))
                           :held-stress-ladder-refused-subjects
                           (or (:held-stress-ladder-refused-subjects p)
                               (count (filter :held-stress-ladder-refused
                                              (concat subs ten-subs))))
                           :all-inkind-full-chain-refused-n
                           (count (filter #(true? (get-in % [:membrane-summary
                                                             :all-inkind-produce-rails-full-chain-refused]))
                                          (concat subs ten-subs)))
                           :all-seven-receive-membrane-refused-n
                           (count (filter #(true? (get-in % [:membrane-summary
                                                             :all-seven-rails-receive-membrane-refused]))
                                          (concat subs ten-subs)))
                           :liquidity-recv-refused-n
                           (count (filter #(and (:liquidity-gated-receive-status %)
                                                (false? (get-in % [:liquidity-gated-receive-status
                                                                   :admissible])))
                                          (concat subs ten-subs)))
                           :refusal-reason (:refusal-reason p)
                           :cash-usd-micros 0
                           :live false
                           :score-surface []
                           :priority-stack PRIORITY-STACK}]
                  (pp/assert-no-public-scores! row)
                  row))
              (or (:packages batch) []))]
    {:admissible-cohorts (or (:admissible-cohorts batch) 0)
     :refused-cohorts (or (:refused-cohorts batch) 0)
     :enrolled-subjects (or (:enrolled-subjects batch) 0)
     :stage-counts (or (:stage-counts batch) (frequencies (map :stage subjects)))
     ;; prefer per-package flowable/full sums (post-G7) over bare batch totals
     :committed-usd-micros-yr
     (let [s (reduce + 0 (map #(or (:committed-usd-micros-yr %) 0) pkgs))]
       (if (pos? s) s (or (:committed-usd-micros-yr batch) 0)))
     :disclosure-open (reduce + 0 (map #(or (:disclosure-open %) 0) pkgs))
     :disclosure-held (reduce + 0 (map #(or (:disclosure-held %) 0) pkgs))
     :mitsuho-r1-dry (reduce + 0 (map #(or (:mitsuho-r1-dry %) 0) pkgs))
     :mitsuho-gated-refused (reduce + 0 (map #(or (:mitsuho-gated-refused %) 0) pkgs))
     :hikari-r1-dry (reduce + 0 (map #(or (:hikari-r1-dry %) 0) pkgs))
     :hikari-gated-refused (reduce + 0 (map #(or (:hikari-gated-refused %) 0) pkgs))
     :care-r1-dry (reduce + 0 (map #(or (:care-r1-dry %) 0) pkgs))
     :care-gated-refused (reduce + 0 (map #(or (:care-gated-refused %) 0) pkgs))
     :housing-r1-dry (reduce + 0 (map #(or (:housing-r1-dry %) 0) pkgs))
     :housing-gated-refused (reduce + 0 (map #(or (:housing-gated-refused %) 0) pkgs))
     :housing-land-grant-executed
     (reduce + 0 (map #(or (:housing-land-grant-executed %) 0) pkgs))
     :tooling-r1-dry (reduce + 0 (map #(or (:tooling-r1-dry %) 0) pkgs))
     :tooling-gated-refused (reduce + 0 (map #(or (:tooling-gated-refused %) 0) pkgs))
     :compute-r1-dry (reduce + 0 (map #(or (:compute-r1-dry %) 0) pkgs))
     :compute-gated-refused (reduce + 0 (map #(or (:compute-gated-refused %) 0) pkgs))
     :liquidity-r1-dry (reduce + 0 (map #(or (:liquidity-r1-dry %) 0) pkgs))
     :liquidity-gated-refused (reduce + 0 (map #(or (:liquidity-gated-refused %) 0) pkgs))
     :liquidity-member-principal
     (reduce + 0 (map #(or (:liquidity-member-principal %) 0) pkgs))
     :liquidity-loan-executed
     (reduce + 0 (map #(or (:liquidity-loan-executed %) 0) pkgs))
     :liquidity-cash-usd-micros
     (reduce + 0 (map #(or (:liquidity-cash-usd-micros %) 0) pkgs))
     :membrane-subjects
     (reduce + 0 (map #(or (:membrane-subjects %) 0) pkgs))
     :held-stress-subjects
     (or (:held-stress-subjects batch)
         (reduce + 0 (map #(or (:held-stress-subjects %) 0) pkgs)))
     :held-stress-ladder-refused-subjects
     (or (:held-stress-ladder-refused-subjects batch)
         (reduce + 0 (map #(or (:held-stress-ladder-refused-subjects %) 0) pkgs)))
     :all-inkind-full-chain-refused-n
     (reduce + 0 (map #(or (:all-inkind-full-chain-refused-n %) 0) pkgs))
     :all-seven-receive-membrane-refused-n
     (reduce + 0 (map #(or (:all-seven-receive-membrane-refused-n %) 0) pkgs))
     :liquidity-recv-refused-n
     (reduce + 0 (map #(or (:liquidity-recv-refused-n %) 0) pkgs))
     :earmark-usd-micros-yr
     (reduce + 0 (map #(or (:earmark-usd-micros-yr %) 0) pkgs))
     :headroom-usd-micros-yr
     (reduce + 0 (map #(or (:headroom-usd-micros-yr %) 0) pkgs))
     :committed-full-usd-micros-yr
     (reduce + 0 (map #(or (:committed-full-usd-micros-yr %) 0) pkgs))
     :gov-flowable-usd-micros
     (reduce + 0 (map #(or (:gov-flowable-usd-micros %) 0) pkgs))
     :gov-post-ratify-usd-micros
     (reduce + 0 (map #(or (:gov-post-ratify-usd-micros %) 0) pkgs))
     :tenure-subjects
     (reduce + 0 (map #(or (:tenure-subjects %) 0) pkgs))
     :tenure-g2-cohorts
     (count (filter :tenure-g2 pkgs))
     :tenure-committed-usd-micros-yr
     (reduce + 0 (map #(or (:tenure-committed-usd-micros-yr %) 0) pkgs))
     :tenure-committed-full-usd-micros-yr
     (reduce + 0 (map #(or (:tenure-committed-full-usd-micros-yr %) 0) pkgs))
     :tenure-gov-flowable-usd-micros
     (reduce + 0 (map #(or (:tenure-gov-flowable-usd-micros %) 0) pkgs))
     :tenure-gov-post-ratify-usd-micros
     (reduce + 0 (map #(or (:tenure-gov-post-ratify-usd-micros %) 0) pkgs))
     :g2-admissible-cohorts
     (count (filter :g2-admissible pkgs))
     :packages pkgs
     :cash-usd-micros 0
     :live false
     :score-surface []
     :priority-stack PRIORITY-STACK}))

(defn report-edn
  "Full facts-only report structure."
  [seed & {:keys [include-l0-demo include-itonami include-displacement-l0
                  include-scorecard include-audit include-ss-priority-path]
           :or {include-displacement-l0 true include-scorecard true include-audit true
                include-ss-priority-path true}}]
  (let [facts (seed-public-facts seed)
        rails (inkind-rail-packages seed)
        drows (disp/public-displacement-facts seed)
        itonami-rows (when include-itonami
                       (try
                         (itonami/public-facts-from-itonami
                          (itonami/load-itonami-seed-file) seed)
                         (catch #?(:clj Exception :cljs :default) _ [])))
        dl0-batch (when include-displacement-l0
                    (try
                      ;; L0→L6 tenure + G7 package so public L0 shows flowable-first committed
                      (let [batch0 (dl0/run-default-seed :max-slots 2 :climb-steps 4)
                            seed-itonami (itonami/load-itonami-seed-file)
                            evs (itonami/load-itonami-batch seed-itonami)
                            with-ten (if (seq evs)
                                       (ten/run-batch-with-tenure batch0 evs
                                                                 :target-stage "L6")
                                       batch0)]
                        (dgov/package-batch with-ten))
                      (catch #?(:clj Exception :cljs :default) _ nil)))
        dl0-sum (when dl0-batch (displacement-l0-public-summary dl0-batch))
        scard (when include-scorecard
                (try
                  (if dl0-batch (dsc/build dl0-batch) (dsc/build))
                  (catch #?(:clj Exception :cljs :default) _ nil)))
        audit-sum (when include-audit
                    (try (audit/summary)
                         (catch #?(:clj Exception :cljs :default) _
                           {:runs 0 :live false :cash-usd-micros 0
                            :score-surface []
                            :priority-stack PRIORITY-STACK
                            :any-land-grant-executed? false
                            :all-runs-live-refused true})))
        body {:report/id "fuchi.public-surface"
              :report/adr ["2607177000" "2606032130"]
              :report/priority-stack PRIORITY-STACK
              :report/score-surface []
              :report/cash-usd-micros 0
              :report/live false
              :report/public-persons facts
              :report/rail-packages rails
              :report/displacement drows
              :report/displacement-summary (disp/summary drows)
              :report/itonami-displacement (or itonami-rows [])
              :report/displacement-l0 (or dl0-sum {})
              :report/displacement-scorecard (or scard {})
              :report/pipeline-audit (or audit-sum {})
              :report/pages-deploy-status (pages-deploy-refuse-status)
              :report/priority-stack-offline
              (or (get scard :scorecard/priority-stack-offline)
                  (try (pstack/public-facts (pstack/run-offline {}))
                       (catch #?(:clj Exception :cljs :default) _
                         {:path "priority-stack-offline"
                          :error "priority-stack unavailable"
                          :ok false
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/ss-priority-path
              (when include-ss-priority-path
                (try (ss-priority-path-public-fact)
                     (catch #?(:clj Exception :cljs :default) _
                       {:path "ss-offline-inkind-rails"
                        :error "ss-priority-path unavailable"
                        :live false :cash-usd-micros 0 :score-surface []
                        :priority-stack PRIORITY-STACK})))
              :report/l0-demo (when include-l0-demo
                                (l0-demo-fact "did:web:etzhayyim.com:member:lot"))
              :report/l0-all-seven-enroll
              (or (get scard :scorecard/l0-all-seven-enroll)
                  (try (dsc/l0-all-seven-enroll-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-all-seven-enroll unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-held-all-seven-enroll
              (or (get scard :scorecard/l0-held-all-seven-enroll)
                  (try (dsc/l0-held-all-seven-enroll-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-held-all-seven-enroll unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-exit-reaffirm
              (or (get scard :scorecard/l0-exit-reaffirm)
                  (try (dsc/l0-exit-reaffirm-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-exit-reaffirm unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-falsehood-lift
              (or (get scard :scorecard/l0-falsehood-lift)
                  (try (dsc/l0-falsehood-lift-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-falsehood-lift unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-care-first-mitsuho
              (or (get scard :scorecard/l0-care-first-mitsuho)
                  (try (dsc/l0-care-first-mitsuho-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-care-first-mitsuho unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-care-first-hikari
              (or (get scard :scorecard/l0-care-first-hikari)
                  (try (dsc/l0-care-first-hikari-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-care-first-hikari unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-care-first-mitsuho-hikari
              (or (get scard :scorecard/l0-care-first-mitsuho-hikari)
                  (try (dsc/l0-care-first-mitsuho-hikari-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-care-first-mitsuho-hikari unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-care-housing-first
              (or (get scard :scorecard/l0-care-housing-first)
                  (try (dsc/l0-care-housing-first-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-care-housing-first unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-multi-gen-substrate
              (or (get scard :scorecard/l0-multi-gen-substrate)
                  (try (dsc/l0-multi-gen-substrate-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-multi-gen-substrate unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-full-inkind-substrate
              (or (get scard :scorecard/l0-full-inkind-substrate)
                  (try (dsc/l0-full-inkind-substrate-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-full-inkind-substrate unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-vocation-recovery
              (or (get scard :scorecard/l0-vocation-recovery)
                  (try (dsc/l0-vocation-recovery-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-vocation-recovery unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-liquidity-residual
              (or (get scard :scorecard/l0-liquidity-residual)
                  (try (dsc/l0-liquidity-residual-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-liquidity-residual unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-all-seven-substrate
              (or (get scard :scorecard/l0-all-seven-substrate)
                  (try (dsc/l0-all-seven-substrate-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "l0-all-seven-substrate unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/l0-priority-path-catalog
              (or (get scard :scorecard/l0-priority-path-catalog)
                  (try (dsc/l0-priority-path-catalog-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "priority-path-catalog unavailable"
                          :live false :cash-usd-micros 0 :score-surface []
                          :priority-stack PRIORITY-STACK})))
              :report/rail-care-design
              (or (get scard :scorecard/rail-care-design)
                  (try (dsc/rail-care-design-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "rail-care design unavailable"
                          :live false :cash-usd-micros 0 :score-surface []})))
              :report/rail-housing-design
              (or (get scard :scorecard/rail-housing-design)
                  (try (dsc/rail-housing-design-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "rail-housing design unavailable"
                          :live false :cash-usd-micros 0 :score-surface []})))
              :report/rail-mitsuho-design
              (or (get scard :scorecard/rail-mitsuho-design)
                  (try (dsc/rail-mitsuho-design-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "rail-mitsuho design unavailable"
                          :live false :cash-usd-micros 0 :score-surface []})))
              :report/rail-hikari-design
              (or (get scard :scorecard/rail-hikari-design)
                  (try (dsc/rail-hikari-design-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "rail-hikari design unavailable"
                          :live false :cash-usd-micros 0 :score-surface []})))
              :report/rail-tooling-design
              (or (get scard :scorecard/rail-tooling-design)
                  (try (dsc/rail-tooling-design-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "rail-tooling design unavailable"
                          :live false :cash-usd-micros 0 :score-surface []})))
              :report/rail-compute-design
              (or (get scard :scorecard/rail-compute-design)
                  (try (dsc/rail-compute-design-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "rail-compute design unavailable"
                          :live false :cash-usd-micros 0 :score-surface []})))
              :report/rail-liquidity-design
              (or (get scard :scorecard/rail-liquidity-design)
                  (try (dsc/rail-liquidity-design-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "rail-liquidity design unavailable"
                          :live false :cash-usd-micros 0 :score-surface []})))
              :report/rail-design-catalog
              (or (get scard :scorecard/rail-design-catalog)
                  (try (dsc/rail-design-catalog-fact)
                       (catch #?(:clj Exception :cljs :default) _
                         {:error "rail-design-catalog unavailable"
                          :rail-count 0
                          :live false :cash-usd-micros 0 :score-surface []})))}]
    (doseq [f facts] (pp/assert-no-public-scores! f))
    (doseq [d drows] (pp/assert-no-public-scores! d))
    (when-let [sp (:report/ss-priority-path body)]
      (pp/assert-no-public-scores! (dissoc sp :error)))
    (when-let [rc (:report/rail-care-design body)]
      (pp/assert-no-public-scores!
       (dissoc rc :error :note :api :path :priority-stack :multi-gen-facts
               :care-first-before-rails :care-first-api-path)))
    (when-let [rho (:report/rail-housing-design body)]
      (pp/assert-no-public-scores!
       (dissoc rho :error :note :api :path :priority-stack :multi-gen-facts
               :care-first-before-rails :care-first-api-path)))
    (when-let [rm (:report/rail-mitsuho-design body)]
      (pp/assert-no-public-scores!
       (dissoc rm :error :note :api :path :priority-stack :multi-gen-facts
               :care-first-before-rails :care-first-api-path)))
    (when-let [rh (:report/rail-hikari-design body)]
      (pp/assert-no-public-scores!
       (dissoc rh :error :note :api :path :priority-stack :multi-gen-facts
               :care-first-before-rails :care-first-api-path)))
    (when-let [rt (:report/rail-tooling-design body)]
      (pp/assert-no-public-scores!
       (dissoc rt :error :note :api :path :priority-stack :multi-gen-facts
               :care-first-before-rails :care-first-api-path)))
    (when-let [rco (:report/rail-compute-design body)]
      (pp/assert-no-public-scores!
       (dissoc rco :error :note :api :path :priority-stack :multi-gen-facts
               :care-first-before-rails :care-first-api-path)))
    (when-let [rl (:report/rail-liquidity-design body)]
      (pp/assert-no-public-scores!
       (dissoc rl :error :note :api :path :priority-stack :multi-gen-facts
               :care-first-before-rails :care-first-api-path)))
    (when-let [rcat (:report/rail-design-catalog body)]
      (pp/assert-no-public-scores!
       (dissoc rcat :error :note :rails :order :rail-ids :rail-kinds :invariants
               :priority-stack :api :path)))
    (when-let [l7 (:report/l0-all-seven-enroll body)]
      (pp/assert-no-public-scores!
       (dissoc l7 :error :note :api :path :priority-stack
               :continuity-final-state :ladder-advance-phase)))
    (when-let [lh (:report/l0-held-all-seven-enroll body)]
      (pp/assert-no-public-scores!
       (dissoc lh :error :note :api :path :priority-stack
               :continuity-final-state :ladder-advance-phase)))
    (when-let [ex (:report/l0-exit-reaffirm body)]
      (pp/assert-no-public-scores!
       (dissoc ex :error :note :api :path :priority-stack
               :exit-ladder-phase :reaffirm-ladder-phase)))
    (when-let [fl (:report/l0-falsehood-lift body)]
      (pp/assert-no-public-scores!
       (dissoc fl :error :note :api :path :priority-stack
               :falsehood-ladder-phase :lift-ladder-phase)))
    (when-let [cf (:report/l0-care-first-mitsuho body)]
      (pp/assert-no-public-scores!
       (dissoc cf :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress
               :care-design :mitsuho-design
               :care-first-before-rails :care-first-api-path))
      (when-let [hs (:held-stress cf)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [ch (:report/l0-care-first-hikari body)]
      (pp/assert-no-public-scores!
       (dissoc ch :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress
               :care-design :hikari-design
               :care-first-before-rails :care-first-api-path))
      (when-let [hs (:held-stress ch)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [cfh (:report/l0-care-first-mitsuho-hikari body)]
      (pp/assert-no-public-scores!
       (dissoc cfh :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress
               :care-design :mitsuho-design :hikari-design
               :care-first-before-rails))
      (when-let [hs (:held-stress cfh)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [chs (:report/l0-care-housing-first body)]
      (pp/assert-no-public-scores!
       (dissoc chs :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress))
      (when-let [hs (:held-stress chs)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [mgs (:report/l0-multi-gen-substrate body)]
      (pp/assert-no-public-scores!
       (dissoc mgs :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress))
      (when-let [hs (:held-stress mgs)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [fis (:report/l0-full-inkind-substrate body)]
      (pp/assert-no-public-scores!
       (dissoc fis :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress))
      (when-let [hs (:held-stress fis)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [voc (:report/l0-vocation-recovery body)]
      (pp/assert-no-public-scores!
       (dissoc voc :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress))
      (when-let [hs (:held-stress voc)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [liq (:report/l0-liquidity-residual body)]
      (pp/assert-no-public-scores!
       (dissoc liq :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress))
      (when-let [hs (:held-stress liq)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [a7s (:report/l0-all-seven-substrate body)]
      (pp/assert-no-public-scores!
       (dissoc a7s :error :note :api :path :priority-stack :multi-gen-facts
               :ladder-advance-phase :held-stress))
      (when-let [hs (:held-stress a7s)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    (when-let [cat (:report/l0-priority-path-catalog body)]
      (pp/assert-no-public-scores!
       (dissoc cat :error :note :paths :invariants :priority-stack)))
    body))

(defn report-md
  "Markdown facts-only public surface (no ranks/scores)."
  [seed & {:keys [include-l0-demo include-itonami include-displacement-l0]
           :or {include-displacement-l0 true}}]
  (let [body (report-edn seed :include-l0-demo include-l0-demo :include-itonami include-itonami
                         :include-displacement-l0 include-displacement-l0)
        lines (transient
               ["# fuchi — public surface (facts only)\n"
                (str "Priority: wellbecoming > mago(孫) > ko(子) > present. "
                     "Scores/ranks unrepresentable. cash≡0. live=false.\n")
                "## Public persons\n"
                "| did | covenant | public? | disclosure | rails | imputed |\n"
                "|---|---|---|---|---|---|\n"])]
    (doseq [f (:report/public-persons body)]
      (conj! lines
             (str "| " (last-seg (:did f)) " | " (:covenant f) " | "
                  (:public-person? f) " | " (:disclosure-status f) " | "
                  (str/join "," (:rails f)) " | " (:imputed-fact f) " |\n")))
    (conj! lines "\n## Rail packages (R1 dry + floor plans; no live execute)\n")
    (conj! lines "| did | food | energy | care | housing | tooling | compute | liquidity |\n")
    (conj! lines "|---|---|---|---|---|---|---|---|\n")
    (doseq [r (:report/rail-packages body)]
      (conj! lines
             (str "| " (last-seg (:did r)) " | "
                  (or (phase-of (:food r)) "—") " | "
                  (or (phase-of (:energy r)) "—") " | "
                  (or (phase-of (:care r)) "—") " | "
                  (or (phase-of (:housing r)) "—") " | "
                  (or (phase-of (:tooling r)) "—") " | "
                  (or (phase-of (:compute r)) "—") " | "
                  (or (phase-of (:liquidity r)) "—") " |\n")))
    (conj! lines "\n## Dry floor facts (produce-executed=false)\n")
    (conj! lines "| did | kcal | kWh | care-h | housing-mo | tools | GPU-h |\n")
    (conj! lines "|---|---|---|---|---|---|---|\n")
    (doseq [r (:report/rail-packages body)]
      (conj! lines
             (str "| " (last-seg (:did r)) " | "
                  (or (get-in r [:food-floor :kcal-floor-yr]) "—") " | "
                  (or (get-in r [:energy-floor :kwh-floor-yr]) "—") " | "
                  (or (get-in r [:care-floor :care-hours-floor-yr]) "—") " | "
                  (or (get-in r [:housing-floor :housing-months-floor-yr]) "—") " | "
                  (or (get-in r [:tooling-floor :tool-units-floor-yr]) "—") " | "
                  (or (get-in r [:compute-floor :gpu-hours-floor-yr]) "—") " |\n")))
    (conj! lines "\n## Displacement → earmark (itonami/robotics coupling facts)\n")
    (conj! lines "| actor | cohort | displaced | funded | admissible | earmark USD micros |\n")
    (conj! lines "|---|---|---|---|---|---|\n")
    (doseq [d (:report/displacement body)]
      (conj! lines
             (str "| " (:displacing-actor d) " | " (:cohort-id d) " | "
                  (:displaced-count d) " | " (:funded d) " | " (:admissible d) " | "
                  (:earmark-usd-micros-yr d) " |\n")))
    (when (seq (:report/itonami-displacement body))
      (conj! lines "\n## itonami surplus bridge (offline seed)\n")
      (conj! lines "| actor | cohort | displaced | funded | admissible |\n|---|---|---|---|---|\n")
      (doseq [d (:report/itonami-displacement body)]
        (conj! lines
               (str "| " (:displacing-actor d) " | " (:cohort-id d) " | "
                    (:displaced-count d) " | " (:funded d) " | " (:admissible d) " |\n"))))
    (let [s (:report/displacement-summary body)]
      (conj! lines (str "\nSummary: events=" (:displacement-events s)
                        " admissible=" (:funded-admissible s)
                        " refused=" (:refused s)
                        " total-displaced=" (:total-displaced s) "\n")))
    (when-let [dl0 (:report/displacement-l0 body)]
      (when (seq (:packages dl0))
        (conj! lines "\n## Displacement → L0→L4 enroll (offline; G2 gated)\n")
        (conj! lines (str "admissible-cohorts=" (:admissible-cohorts dl0)
                          " refused-cohorts=" (:refused-cohorts dl0)
                          " enrolled-subjects=" (:enrolled-subjects dl0)
                          " stages=" (pr-str (:stage-counts dl0)) "\n"))
        (conj! lines (str "disclosure-open=" (or (:disclosure-open dl0) 0)
                          " disclosure-held=" (or (:disclosure-held dl0) 0)
                          " g2-admissible-cohorts=" (or (:g2-admissible-cohorts dl0) 0) "\n"))
        (conj! lines (str "earmark-total=" (or (:earmark-usd-micros-yr dl0) 0)
                          " committed-flowable-total=" (or (:committed-usd-micros-yr dl0) 0)
                          " committed-full-total=" (or (:committed-full-usd-micros-yr dl0) 0)
                          " headroom-total=" (or (:headroom-usd-micros-yr dl0) 0) "\n"))
        (conj! lines (str "gov-flowable-total=" (or (:gov-flowable-usd-micros dl0) 0)
                          " gov-post-ratify-total=" (or (:gov-post-ratify-usd-micros dl0) 0) "\n"))
        (conj! lines (str "tenure-subjects=" (or (:tenure-subjects dl0) 0)
                          " tenure-g2-cohorts=" (or (:tenure-g2-cohorts dl0) 0)
                          " tenure-committed-flow=" (or (:tenure-committed-usd-micros-yr dl0) 0)
                          " tenure-committed-full=" (or (:tenure-committed-full-usd-micros-yr dl0) 0) "\n"))
        (conj! lines (str "tenure-gov-flowable=" (or (:tenure-gov-flowable-usd-micros dl0) 0)
                          " tenure-gov-post-ratify=" (or (:tenure-gov-post-ratify-usd-micros dl0) 0) "\n"))
        (conj! lines (str "land-grant-executed-total="
                          (or (:housing-land-grant-executed dl0) 0)
                          " (post-ratify plan keeps land-grant=false; Council-gated)\n"))
        (conj! lines (str "L0 membranes subjects/all-inkind/all-seven/liq-recv-refused="
                          (or (:membrane-subjects dl0) 0) "/"
                          (or (:all-inkind-full-chain-refused-n dl0) 0) "/"
                          (or (:all-seven-receive-membrane-refused-n dl0) 0) "/"
                          (or (:liquidity-recv-refused-n dl0) 0)
                          " (default refuse; liquidity residual member-principal)\n"))
        (conj! lines (str "L0 held-stress subjects/ladder-refused="
                          (or (:held-stress-subjects dl0) 0) "/"
                          (or (:held-stress-ladder-refused-subjects dl0) 0)
                          " (priority-2 stale disclosure; ladder refuse)\n"))
        (conj! lines (str "| actor | cohort | phase | n | g2 | funded | earmark | disc-o/h "
                          "| L4-flow | L4-full | L4-post | ten-n | ten-flow | ten-post "
                          "| land-grant | headroom |\n"
                          "|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|\n"))
        (doseq [p (:packages dl0)]
          (conj! lines
                 (str "| " (:displacing-actor p) " | " (:cohort-id p) " | "
                      (:phase p) " | " (:subject-count p) " | "
                      (:g2-admissible p) " | "
                      (boolean (:funded p)) " | "
                      (or (:earmark-usd-micros-yr p) 0) " | "
                      (or (:disclosure-open p) 0) "/"
                      (or (:disclosure-held p) 0) " | "
                      (:committed-usd-micros-yr p) " | "
                      (or (:committed-full-usd-micros-yr p) 0) " | "
                      (or (:gov-post-ratify-usd-micros p) 0) " | "
                      (or (:tenure-subjects p) 0) " | "
                      (or (:tenure-committed-usd-micros-yr p) 0) " | "
                      (or (:tenure-gov-post-ratify-usd-micros p) 0) " | "
                      (or (:housing-land-grant-executed p) 0) " | "
                      (:headroom-usd-micros-yr p) " |\n"))))
    (when-let [sc (:report/displacement-scorecard body)]
      (when (seq sc)
        (conj! lines "\n## Displacement SS scorecard (offline)\n")
        (conj! lines (str "- all-live-refused: " (:scorecard/all-live-refused sc) "\n"))
        (conj! lines (str "- booked-entries: " (:scorecard/booked-entries sc) "\n"))
        (conj! lines (str "- committed (flowable-first): "
                         (:scorecard/committed-usd-micros-yr sc) "\n"))
        (conj! lines (str "- headroom: " (:scorecard/headroom-usd-micros-yr sc) "\n"))
        (conj! lines (str "- tenure-subjects (L6): " (:scorecard/tenure-subjects sc) "\n"))
        (conj! lines (str "- tenure-stages: " (pr-str (:scorecard/tenure-stage-counts sc)) "\n"))
        (conj! lines (str "- gov-routes: " (pr-str (:scorecard/gov-route-counts sc)) "\n"))
        (conj! lines "  (housing held for council-lv7; multi-gen substrate may dry-flow)\n")
        (conj! lines (str "- gov-flowable / gov-post-ratify: "
                         (or (:scorecard/gov-flowable-committed-usd-micros sc) 0) "/"
                         (or (:scorecard/gov-post-ratify-committed-usd-micros sc) 0) "\n"))
        (conj! lines (str "- tenure-gov-flowable / tenure-gov-post-ratify: "
                         (or (:scorecard/tenure-gov-flowable-committed-usd-micros sc) 0) "/"
                         (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros sc) 0) "\n"))
        (conj! lines (str "- land-grant-executed (post-ratify plan still false): "
                         (or (:scorecard/housing-land-grant-executed sc) 0) "\n"))
        (conj! lines (str "- R2 execute statuses/refused/executed: "
                         (or (:scorecard/r2-status-count sc) 0) "/"
                         (or (:scorecard/r2-refused sc) 0) "/"
                         (or (:scorecard/r2-executed sc) 0)
                         " all-r2-not-executed="
                         (boolean (:scorecard/all-r2-not-executed sc true)) "\n"))
        (conj! lines (str "- L4 disclosure open/held: "
                         (or (:scorecard/l4-disclosure-open sc) 0) "/"
                         (or (:scorecard/l4-disclosure-held sc) 0) "\n"))
        (conj! lines (str "- mitsuho food R1/gated/produce: "
                         (or (:scorecard/mitsuho-r1-dry sc) 0) "/"
                         (or (:scorecard/mitsuho-gated-refused sc) 0) "/"
                         (or (:scorecard/mitsuho-produce-executed sc) 0) "\n"))
        (conj! lines (str "- hikari energy R1/gated/generate: "
                         (or (:scorecard/hikari-r1-dry sc) 0) "/"
                         (or (:scorecard/hikari-gated-refused sc) 0) "/"
                         (or (:scorecard/hikari-generate-executed sc) 0) "\n"))
        (conj! lines (str "- care-iyashi R1/gated/delivery: "
                         (or (:scorecard/care-r1-dry sc) 0) "/"
                         (or (:scorecard/care-gated-refused sc) 0) "/"
                         (or (:scorecard/care-delivery-executed sc) 0) "\n"))
        (conj! lines (str "- housing-commons R1/gated/land-grant: "
                         (or (:scorecard/housing-r1-dry sc) 0) "/"
                         (or (:scorecard/housing-gated-refused sc) 0) "/"
                         (or (:scorecard/housing-land-grant-executed sc) 0) "\n"))
        (conj! lines (str "- housing council-held: "
                         (or (:scorecard/housing-council-held sc) 0) "\n"))
        (conj! lines (str "- tooling-okaimono R1/gated/fulfill: "
                         (or (:scorecard/tooling-r1-dry sc) 0) "/"
                         (or (:scorecard/tooling-gated-refused sc) 0) "/"
                         (or (:scorecard/tooling-fulfillment-executed sc) 0) "\n"))
        (conj! lines (str "- compute-murakumo R1/gated/quota: "
                         (or (:scorecard/compute-r1-dry sc) 0) "/"
                         (or (:scorecard/compute-gated-refused sc) 0) "/"
                         (or (:scorecard/compute-quota-executed sc) 0) "\n"))
        (conj! lines (str "- liquidity-warifu R1/gated/loan: "
                         (or (:scorecard/liquidity-r1-dry sc) 0) "/"
                         (or (:scorecard/liquidity-gated-refused sc) 0) "/"
                         (or (:scorecard/liquidity-loan-executed sc) 0) "\n"))
        (conj! lines (str "- liquidity member-principal / cash-usd-micros: "
                         (or (:scorecard/liquidity-member-principal sc) 0) "/"
                         (or (:scorecard/liquidity-cash-usd-micros sc) 0) "\n"))
        (conj! lines (str "- displacement L0 held-stress subjects / ladder-refused: "
                         (or (:scorecard/displacement-held-stress-subjects sc) 0) "/"
                         (or (:scorecard/displacement-held-stress-ladder-refused sc) 0) "\n"))
        (conj! lines (str "- tenure held-stress subjects / ladder-refused / carried-from-L0: "
                         (or (:scorecard/tenure-held-stress-subjects sc) 0) "/"
                         (or (:scorecard/tenure-held-stress-ladder-refused sc) 0) "/"
                         (or (:scorecard/tenure-held-stress-carried sc) 0) "\n"))
        (conj! lines (str "- gov held-stress subjects / ladder-refused (L4 rows): "
                         (or (:scorecard/gov-held-stress-subjects sc) 0) "/"
                         (or (:scorecard/gov-held-stress-ladder-refused sc) 0) "\n"))
        (conj! lines (str "- tenure-gov held-stress subjects / ladder-refused: "
                         (or (:scorecard/tenure-gov-held-stress-subjects sc) 0) "/"
                         (or (:scorecard/tenure-gov-held-stress-ladder-refused sc) 0) "\n"))
        (when-let [st (:scorecard/all-held-stress sc)]
          (conj! lines "\n### All-disclosure-held stress (priority #2)\n")
          (conj! lines (str "- held-subjects: " (:held-subjects st) "\n"))
          (conj! lines (str "- open-path gov flowable: " (:open-gov-flowable st) "\n"))
          (conj! lines (str "- all-held gov flowable: " (:gov-flowable st) "\n"))
          (conj! lines (str "- land-grant-executed: " (:land-grant-executed st) "\n"))
          (conj! lines (str "- live: " (:live st) " cash: " (:cash-usd-micros st) "\n"))))))
    (when-let [dep (:report/pages-deploy-status body)]
      (conj! lines "\n## Pages deploy (offline membrane, plan-only)\n")
      (conj! lines (str "- phase: " (:phase dep) "\n"))
      (conj! lines (str "- admissible: " (:admissible dep) "\n"))
      (conj! lines (str "- authorized-to-deploy: " (boolean (:authorized-to-deploy dep)) "\n"))
      (conj! lines (str "- package-ready: " (boolean (:package-ready dep true)) "\n"))
      (conj! lines (str "- wrangler-invoked: " (boolean (:wrangler-invoked dep)) "\n"))
      (conj! lines (str "- cloudflare-api-invoked: " (boolean (:cloudflare-api-invoked dep)) "\n"))
      (conj! lines (str "- operator-flag: " (or (:operator-flag dep) "FUCHI_ALLOW_PAGES_DEPLOY") "\n"))
      (conj! lines (str "- deployed: " (:deployed dep) "\n"))
      (conj! lines (str "- live: " (:live dep) " cash: " (:cash-usd-micros dep) "\n"))
      (conj! lines (str "- note: " (or (:note dep) "default refuse") "\n"))
      (when-let [rb (:operator-runbook dep)]
        (conj! lines "- operator runbook (OOB deploy only):\n")
        (doseq [s (or (:steps rb) [])]
          (conj! lines (str "  - " s "\n")))))
    (when-let [au (:report/pipeline-audit body)]
      (when (or (pos? (or (:runs au) 0)) (map? (:last-run au)))
        (conj! lines "\n## Pipeline audit summary (offline, append-only)\n")
        (conj! lines (str "- runs: " (or (:runs au) 0) "\n"))
        (conj! lines (str "- all-runs-live-refused: " (boolean (:all-runs-live-refused au)) "\n"))
        (conj! lines (str "- any-land-grant-executed: " (boolean (:any-land-grant-executed? au)) "\n"))
        (conj! lines (str "- last-run priority-stack ok / L0 / mitsuho gated / held-stress: "
                         (boolean (or (:last-run-priority-stack-ok au)
                                      (:priority-stack-ok au))) "/"
                         (or (:last-run-priority-stack-l0-stage au)
                             (:priority-stack-l0-stage au) "—") "/"
                         (or (:last-run-priority-stack-mitsuho-gated-phase au)
                             (:priority-stack-mitsuho-gated-phase au) "—") "/"
                         (boolean (or (:last-run-priority-stack-mitsuho-held-stress-ladder-refused au)
                                      (:priority-stack-mitsuho-held-stress-ladder-refused au)))
                         "\n"))
        (conj! lines (str "- last-run gov-flowable / gov-post-ratify: "
                         (or (:last-run-gov-flowable-committed-usd-micros au) 0) "/"
                         (or (:last-run-gov-post-ratify-committed-usd-micros au) 0) "\n"))
        (conj! lines (str "- last-run tenure-gov-flowable / tenure-gov-post-ratify: "
                         (or (:last-run-tenure-gov-flowable-committed-usd-micros au) 0) "/"
                         (or (:last-run-tenure-gov-post-ratify-committed-usd-micros au) 0) "\n"))
        (conj! lines (str "- last-run land-grant-executed: "
                         (or (:last-run-housing-land-grant-executed au) 0)
                         " (post-ratify plan keeps land-grant=false)\n"))
        (conj! lines (str "- last-run rail DESIGN catalog rail-count/ok/live-produce-never/all-seven: "
                         (or (:last-run-rail-design-rail-count au) 0) "/"
                         (or (:last-run-rail-design-ok-count au) 0) "/"
                         (boolean (:last-run-rail-design-live-produce-never au true)) "/"
                         (boolean (:last-run-rail-design-all-seven au true))
                         " cash-zero=" (boolean (:last-run-rail-design-all-cash-zero au true))
                         " live-false=" (boolean (:last-run-rail-design-all-live-false au true)) "\n"))
        (conj! lines (str "- last-run care-first DESIGN mitsuho/hikari live-produce: "
                         (boolean (:last-run-l0-care-first-mitsuho-live-produce au)) "/"
                         (boolean (:last-run-l0-care-first-hikari-live-produce au))
                         " ss-care/housing="
                         (boolean (:last-run-ss-care-live-produce au)) "/"
                         (boolean (:last-run-ss-housing-live-produce au))
                         " ss-mitsuho/hikari="
                         (boolean (:last-run-ss-mitsuho-live-produce au)) "/"
                         (boolean (:last-run-ss-hikari-live-produce au)) "\n"))
        (conj! lines (str "- last-run R2 refused/executed: ")
                         (or (:last-run-r2-refused au) 0) "/"
                         (or (:last-run-r2-executed au) 0)
                         " all-r2-not-executed="
                         (boolean (:last-run-all-r2-not-executed au true)) "\n"))
        (conj! lines (str "- last-run SS rails-gated / all-rails-gated-refused: "
                         (or (:last-run-ss-rails-gated-count au) 0) "/"
                         (boolean (:last-run-ss-all-rails-gated-refused au true)) "\n"))
        (conj! lines (str "- last-run SS all-r2-not-executed / l0-published: "
                         (boolean (:last-run-ss-all-r2-not-executed au true)) "/"
                         (boolean (:last-run-ss-l0-published au)) "\n"))
        (conj! lines (str "- last-run SS ladder-to / stage-rails first/second: "
                         (or (:last-run-ss-ladder-to au) "n/a") "/"
                         (or (:last-run-ss-stage-rails-first au) "n/a") "/"
                         (or (:last-run-ss-stage-rails-second au) "n/a") "\n"))
        (conj! lines (str "- last-run SS stage gated count/all-refused/r2-all-refused: "
                         (or (:last-run-ss-stage-gated-count au) 0) "/"
                         (boolean (:last-run-ss-stage-all-gated-refused au true)) "/"
                         (boolean (:last-run-ss-stage-r2-all-refused au true)) "\n"))
        (conj! lines (str "- last-run SS stage care/mitsuho/hikari gated-admissible: "
                         (boolean (:last-run-ss-stage-care-gated-admissible au)) "/"
                         (boolean (:last-run-ss-stage-mitsuho-gated-admissible au)) "/"
                         (boolean (:last-run-ss-stage-hikari-gated-admissible au)) "\n"))
        (conj! lines (str "- last-run SS stage land-grant-executed: "
                         (boolean (:last-run-ss-stage-land-grant-executed au)) "\n"))
        (conj! lines (str "- last-run SS mitsuho/hikari/care gated-receive admissible: "
                         (boolean (:last-run-ss-mitsuho-gated-receive-admissible au)) "/"
                         (boolean (:last-run-ss-hikari-gated-receive-admissible au)) "/"
                         (boolean (:last-run-ss-care-gated-receive-admissible au))
                         " food+energy both-refused="
                         (boolean (:last-run-ss-mitsuho-hikari-receive-both-refused au true))
                         " all-three-refused="
                         (boolean (:last-run-ss-care-mitsuho-hikari-receive-all-refused au true))
                         "\n"))
        (conj! lines (str "- last-run SS mitsuho/hikari/care gated-produce admissible: "
                         (boolean (:last-run-ss-mitsuho-gated-produce-admissible au)) "/"
                         (boolean (:last-run-ss-hikari-gated-produce-admissible au)) "/"
                         (boolean (:last-run-ss-care-gated-produce-admissible au))
                         " produce-all-refused="
                         (boolean (:last-run-ss-care-mitsuho-hikari-produce-all-refused au true))
                         " full-chain-refused="
                         (boolean (:last-run-ss-care-mitsuho-hikari-full-chain-refused au true))
                         "\n"))
        (conj! lines (str "- last-run SS housing gated-receive/produce admissible/full-chain: "
                         (boolean (:last-run-ss-housing-gated-receive-admissible au)) "/"
                         (boolean (:last-run-ss-housing-gated-produce-admissible au)) "/"
                         (boolean (:last-run-ss-housing-full-chain-refused au true))
                         " care+housing+food+energy full-chain-refused="
                         (boolean (:last-run-ss-care-housing-mitsuho-hikari-full-chain-refused au true))
                         "\n"))
        (conj! lines (str "- last-run SS tooling/compute gated-receive/produce full-chain: "
                         (boolean (:last-run-ss-tooling-gated-receive-admissible au)) "/"
                         (boolean (:last-run-ss-tooling-gated-produce-admissible au)) "/"
                         (boolean (:last-run-ss-tooling-full-chain-refused au true)) " · "
                         (boolean (:last-run-ss-compute-gated-receive-admissible au)) "/"
                         (boolean (:last-run-ss-compute-gated-produce-admissible au)) "/"
                         (boolean (:last-run-ss-compute-full-chain-refused au true))
                         " tooling+compute/all-inkind full-chain="
                         (boolean (:last-run-ss-tooling-compute-full-chain-refused au true)) "/"
                         (boolean (:last-run-ss-all-inkind-produce-rails-full-chain-refused au true))
                         "\n"))
        (conj! lines (str "- last-run SS liquidity gated-receive / all-seven receive-membrane: "
                         (boolean (:last-run-ss-liquidity-gated-receive-admissible au)) "/"
                         (boolean (:last-run-ss-liquidity-receive-full-chain-refused au true)) "/"
                         (boolean (:last-run-ss-all-seven-rails-receive-membrane-refused au true))
                         "\n"))
        (conj! lines (str "- cumulative liquidity member-principal / cash-usd-micros: "
                         (or (:total-liquidity-member-principal au) 0) "/"
                         (or (:total-liquidity-cash-usd-micros au) 0) "\n"))
        (conj! lines (str "- live: " (boolean (:live au))
                         " cash: " (or (:cash-usd-micros au) 0) "\n"))))
    (when-let [ps (:report/priority-stack-offline body)]
      (when (map? ps)
        (conj! lines "\n## Priority stack offline SSoT (1)L0 (2)disclosure (3)care-housing→mitsuho+hikari→all-seven\n")
        (conj! lines (str "- ok: " (boolean (:ok ps)) "\n"))
        (conj! lines (str "- (1) L0 stage/public-person/published: "
                         (or (:l0-stage ps) "—") "/"
                         (boolean (:l0-public-person? ps)) "/"
                         (boolean (:l0-published ps)) "\n"))
        (conj! lines (str "- (2) open-may-flow / stale-held / tick-final / continuity-held-steps: "
                         (boolean (:disclosure-open-may-flow ps)) "/"
                         (boolean (:disclosure-stale-held ps)) "/"
                         (or (:disclosure-tick-final ps) "—") "/"
                         (or (:disclosure-continuity-held-steps ps) 0) "\n"))
        (conj! lines (str "- (3) care-housing api/both-refused/land-grant/held-stress: "
                         (or (:care-housing-api-path ps) "—") "/"
                         (boolean (:care-housing-both-refused ps)) "/"
                         (boolean (:care-housing-land-grant-executed ps)) "/"
                         (boolean (:care-housing-held-stress-ladder-refused ps)) "\n"))
        (conj! lines (str "- (3) mitsuho R1/gated/produce / care-first-api / held-stress-ladder: "
                         (or (:mitsuho-r1-phase ps) "—") "/"
                         (or (:mitsuho-gated-phase ps) "—") "/"
                         (boolean (:mitsuho-produce-executed ps)) " · "
                         (or (:mitsuho-care-first-api-path ps) "—") " · "
                         (boolean (:mitsuho-held-stress-ladder-refused ps)) "\n"))
        (conj! lines (str "- (3) hikari R1/gated/produce / care-first-api / held-stress-ladder: "
                         (or (:hikari-r1-phase ps) "—") "/"
                         (or (:hikari-gated-phase ps) "—") "/"
                         (boolean (:hikari-produce-executed ps)) " · "
                         (or (:hikari-care-first-api-path ps) "—") " · "
                         (boolean (:hikari-held-stress-ladder-refused ps)) "\n"))
        (conj! lines (str "- (3) all-seven api/inkind/membrane/liq/loan/land-grant/held-stress: "
                         (or (:all-seven-api-path ps) "—") "/"
                         (boolean (:all-seven-inkind-refused ps)) "/"
                         (boolean (:all-seven-membrane-refused ps)) "/"
                         (boolean (:all-seven-liquidity-refused ps)) "/"
                         (boolean (:all-seven-loan-executed ps)) "/"
                         (boolean (:all-seven-land-grant-executed ps)) "/"
                         (boolean (:all-seven-held-stress-ladder-refused ps)) "\n"))
        (conj! lines (str "- care-first-before-rails food/energy (孫/子): "
                         (pr-str (or (:mitsuho-care-first-before-rails ps) [])) "/"
                         (pr-str (or (:hikari-care-first-before-rails ps) [])) "\n"))
        (conj! lines (str "- l0-paths catalog id/count/all-held-stress: "
                         (or (:l0-paths-design-id ps) "—") "/"
                         (or (:l0-paths-count ps) 0) "/"
                         (boolean (:l0-paths-all-held-stress ps)) "\n"))
        (conj! lines (str "- design-id / order-count: "
                         (or (:design-id ps) "—") "/"
                         (or (:design-order-count ps) 0) "\n"))
        (conj! lines (str "- live / cash / score-surface: "
                         (boolean (:live ps)) "/"
                         (or (:cash-usd-micros ps) 0) "/"
                         (pr-str (or (:score-surface ps) [])) "\n"))
        (when (:error ps)
          (conj! lines (str "- error: " (:error ps) "\n")))))
    (when-let [l7 (:report/l0-all-seven-enroll body)]
      (when (and (map? l7) (not (:error l7)))
        (conj! lines "\n## L0 enroll all-seven rails (priority 1+2+3 smoke)\n")
        (conj! lines (str "- api: " (or (:api l7) "enroll-with-all-seven-rails") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state l7) "n/a") "/"
                         (boolean (:disclosure-held l7)) "/"
                         (boolean (:entitlements-may-flow l7 true)) "\n"))
        (conj! lines (str "- all-inkind / liquidity-receive / all-seven-membrane refused: "
                         (boolean (:all-inkind-produce-rails-full-chain-refused l7)) "/"
                         (boolean (:liquidity-receive-full-chain-refused l7)) "/"
                         (boolean (:all-seven-rails-receive-membrane-refused l7)) "\n"))
        (conj! lines (str "- liquidity member-principal / loan / cash: "
                         (boolean (:liquidity-member-principal l7 true)) "/"
                         (boolean (:liquidity-loan-executed l7)) "/"
                         (or (:liquidity-cash-usd-micros l7) 0) "\n"))
        (conj! lines (str "- land-grant / live / cash-usd-micros: "
                         (boolean (:land-grant-executed l7)) "/"
                         (boolean (:live l7)) "/"
                         (or (:cash-usd-micros l7) 0) "\n"))
        (conj! lines (str "- continuity final/held-steps + ladder phase/refused: "
                         (or (:continuity-final-state l7) "n/a") "/"
                         (or (:continuity-held-steps l7) 0) " · "
                         (or (:ladder-advance-phase l7) "n/a") "/"
                         (boolean (:ladder-advance-refused l7)) "\n"))))
    (when-let [lh (:report/l0-held-all-seven-enroll body)]
      (when (and (map? lh) (not (:error lh)))
        (conj! lines "\n## L0 held all-seven (disclosure stale stress)\n")
        (conj! lines (str "- disclosure held/may-flow: "
                         (boolean (:disclosure-held lh)) "/"
                         (boolean (:entitlements-may-flow lh)) "\n"))
        (conj! lines (str "- all-seven-membrane / ladder-refused: "
                         (boolean (:all-seven-rails-receive-membrane-refused lh)) "/"
                         (boolean (:ladder-advance-refused lh true)) "\n"))
        (conj! lines (str "- loan / land-grant / live / cash: "
                         (boolean (:liquidity-loan-executed lh)) "/"
                         (boolean (:land-grant-executed lh)) "/"
                         (boolean (:live lh)) "/"
                         (or (:cash-usd-micros lh) 0) "\n"))))
    (when-let [ex (:report/l0-exit-reaffirm body)]
      (when (and (map? ex) (not (:error ex)))
        (conj! lines "\n## L0 exit→re-affirm stress (disclosure SM)\n")
        (conj! lines (str "- exit state/suspended/ladder-refused: "
                         (or (:exit-state ex) "n/a") "/"
                         (boolean (:exit-suspended? ex)) "/"
                         (boolean (:exit-ladder-refused ex true)) "\n"))
        (conj! lines (str "- re-affirm state/may-flow/ladder: "
                         (or (:reaffirm-state ex) "n/a") "/"
                         (boolean (:reaffirm-entitlements-may-flow ex true)) "/"
                         (or (:reaffirm-ladder-phase ex) "n/a")
                         "/refused=" (boolean (:reaffirm-ladder-refused ex)) "\n"))
        (conj! lines (str "- live / cash-usd-micros: "
                         (boolean (:live ex)) "/"
                         (or (:cash-usd-micros ex) 0) "\n"))))
    (when-let [fl (:report/l0-falsehood-lift body)]
      (when (and (map? fl) (not (:error fl)))
        (conj! lines "\n## L0 falsehood→lift-hold stress\n")
        (conj! lines (str "- falsehood held/ladder-refused: "
                         (boolean (:falsehood-held? fl true)) "/"
                         (boolean (:falsehood-ladder-refused fl true)) "\n"))
        (conj! lines (str "- lift may-flow/ladder: "
                         (boolean (:lift-entitlements-may-flow fl true)) "/"
                         (or (:lift-ladder-phase fl) "n/a")
                         "/refused=" (boolean (:lift-ladder-refused fl)) "\n"))))
    (when-let [cf (:report/l0-care-first-mitsuho body)]
      (when (and (map? cf) (not (:error cf)))
        (conj! lines "\n## L0 care-first + mitsuho (孫/子 priority)\n")
        (conj! lines (str "- care/mitsuho both-refused: "
                         (boolean (:care-mitsuho-both-refused cf true)) "\n"))
        (conj! lines (str "- care-first-api-path / before-rails: "
                         (or (:care-first-api-path cf) "care-first-mitsuho-path") " / "
                         (str/join "," (or (:care-first-before-rails cf) ["care" "housing"])) "\n"))
        (conj! lines (str "- mitsuho-design rail-kind / live-produce / produce-executed: "
                         (or (get-in cf [:mitsuho-design :rail-kind]) "food-mitsuho") "/"
                         (boolean (:mitsuho-live-produce cf)) "/"
                         (boolean (:mitsuho-produce-executed cf)) "\n"))
        (conj! lines (str "- care-design rail-kind / care-delivery-executed: "
                         (or (get-in cf [:care-design :rail-kind]) "care-iyashi") "/"
                         (boolean (:care-delivery-executed cf)) "\n"))
        (conj! lines (str "- ladder: " (or (:ladder-advance-phase cf) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused cf)) "\n"))
        (when-let [hs (:held-stress cf)]
          (conj! lines (str "- held-stress held/both-refused/ladder-refused: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-mitsuho-both-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live cf)) "/"
                         (or (:cash-usd-micros cf) 0) "\n"))))
    (when-let [ch (:report/l0-care-first-hikari body)]
      (when (and (map? ch) (not (:error ch)))
        (conj! lines "\n## L0 care-first + hikari (孫/子 + energy)\n")
        (conj! lines (str "- care/hikari both-refused: "
                         (boolean (:care-hikari-both-refused ch true)) "\n"))
        (conj! lines (str "- care-first-api-path / before-rails: "
                         (or (:care-first-api-path ch) "care-first-hikari-path") " / "
                         (str/join "," (or (:care-first-before-rails ch) ["care" "housing"])) "\n"))
        (conj! lines (str "- hikari-design rail-kind / live-produce / generate-executed: "
                         (or (get-in ch [:hikari-design :rail-kind]) "energy-hikari") "/"
                         (boolean (:hikari-live-produce ch)) "/"
                         (boolean (:hikari-generate-executed ch)) "\n"))
        (conj! lines (str "- ladder: " (or (:ladder-advance-phase ch) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused ch)) "\n"))
        (when-let [hs (:held-stress ch)]
          (conj! lines (str "- held-stress held/both-refused/ladder-refused: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-hikari-both-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live ch)) "/"
                         (or (:cash-usd-micros ch) 0) "\n"))))
    (when-let [cfh (:report/l0-care-first-mitsuho-hikari body)]
      (when (and (map? cfh) (not (:error cfh)))
        (conj! lines "\n## L0 care-first + mitsuho + hikari (孫/子 dual rail)\n")
        (conj! lines (str "- care/mitsuho/hikari all-refused: "
                         (boolean (:care-mitsuho-hikari-all-refused cfh true)) "\n"))
        (conj! lines (str "- mitsuho+hikari both-refused: "
                         (boolean (:mitsuho-hikari-both-refused cfh true)) "\n"))
        (conj! lines (str "- mitsuho/hikari design live-produce: "
                         (boolean (:mitsuho-live-produce cfh)) "/"
                         (boolean (:hikari-live-produce cfh)) "\n"))
        (conj! lines (str "- ladder: " (or (:ladder-advance-phase cfh) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused cfh)) "\n"))
        (when-let [hs (:held-stress cfh)]
          (conj! lines (str "- held-stress held/all-refused/ladder-refused: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-mitsuho-hikari-all-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live cfh)) "/"
                         (or (:cash-usd-micros cfh) 0) "\n"))))
    (when-let [chs (:report/l0-care-housing-first body)]
      (when (and (map? chs) (not (:error chs)))
        (conj! lines "\n## L0 care+housing multi-gen substrate (孫/子)\n")
        (conj! lines (str "- care/housing both-refused: "
                         (boolean (:care-housing-both-refused chs true)) "\n"))
        (conj! lines (str "- land-grant / ladder: "
                         (boolean (:land-grant-executed chs)) "/"
                         (or (:ladder-advance-phase chs) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused chs)) "\n"))
        (when-let [hs (:held-stress chs)]
          (conj! lines (str "- held-stress held/both-refused/ladder-refused/land-grant: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-housing-both-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "/"
                           (boolean (:land-grant-executed hs)) "\n")))))
    (when-let [mgs (:report/l0-multi-gen-substrate body)]
      (when (and (map? mgs) (not (:error mgs)))
        (conj! lines "\n## L0 multi-gen substrate + mitsuho+hikari (L4 priority)\n")
        (conj! lines (str "- care+housing+food+energy all-refused: "
                         (boolean (:care-housing-mitsuho-hikari-all-refused mgs true)) "\n"))
        (conj! lines (str "- care+housing both / mitsuho+hikari both: "
                         (boolean (:care-housing-both-refused mgs true)) "/"
                         (boolean (:mitsuho-hikari-both-refused mgs true)) "\n"))
        (conj! lines (str "- land-grant / ladder: "
                         (boolean (:land-grant-executed mgs)) "/"
                         (or (:ladder-advance-phase mgs) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused mgs)) "\n"))
        (when-let [hs (:held-stress mgs)]
          (conj! lines (str "- held-stress held/all-refused/ladder-refused/land-grant: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-housing-mitsuho-hikari-all-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "/"
                           (boolean (:land-grant-executed hs)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live mgs)) "/"
                         (or (:cash-usd-micros mgs) 0) "\n"))))
    (when-let [fis (:report/l0-full-inkind-substrate body)]
      (when (and (map? fis) (not (:error fis)))
        (conj! lines "\n## L0 full in-kind substrate (multi-gen + vocation / itonami)\n")
        (conj! lines (str "- six in-kind all-refused: "
                         (boolean (:all-inkind-produce-rails-full-chain-refused fis true)) "\n"))
        (conj! lines (str "- care+housing / mitsuho+hikari / tooling+compute both-refused: "
                         (boolean (:care-housing-both-refused fis true)) "/"
                         (boolean (:mitsuho-hikari-both-refused fis true)) "/"
                         (boolean (:tooling-compute-both-refused fis true)) "\n"))
        (conj! lines (str "- land-grant/fulfillment/quota: "
                         (boolean (:land-grant-executed fis)) "/"
                         (boolean (:fulfillment-executed fis)) "/"
                         (boolean (:quota-executed fis)) "\n"))
        (conj! lines (str "- ladder: " (or (:ladder-advance-phase fis) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused fis)) "\n"))
        (when-let [hs (:held-stress fis)]
          (conj! lines (str "- held-stress held/all-refused/ladder-refused/land-grant/fulfillment/quota: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:all-inkind-produce-rails-full-chain-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "/"
                           (boolean (:land-grant-executed hs)) "/"
                           (boolean (:fulfillment-executed hs)) "/"
                           (boolean (:quota-executed hs)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live fis)) "/"
                         (or (:cash-usd-micros fis) 0) "\n"))))
    (when-let [voc (:report/l0-vocation-recovery body)]
      (when (and (map? voc) (not (:error voc)))
        (conj! lines "\n## L0 vocation recovery (tooling+compute / itonami job-loss)\n")
        (conj! lines (str "- tooling+compute both-refused: "
                         (boolean (:tooling-compute-both-refused voc true)) "\n"))
        (conj! lines (str "- fulfillment/quota: "
                         (boolean (:fulfillment-executed voc)) "/"
                         (boolean (:quota-executed voc)) "\n"))
        (conj! lines (str "- ladder: " (or (:ladder-advance-phase voc) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused voc)) "\n"))
        (when-let [hs (:held-stress voc)]
          (conj! lines (str "- held-stress held/both-refused/ladder-refused/fulfillment/quota: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:tooling-compute-both-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "/"
                           (boolean (:fulfillment-executed hs)) "/"
                           (boolean (:quota-executed hs)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live voc)) "/"
                         (or (:cash-usd-micros voc) 0) "\n"))))
    (when-let [liq (:report/l0-liquidity-residual body)]
      (when (and (map? liq) (not (:error liq)))
        (conj! lines "\n## L0 liquidity residual (warifu member-principal)\n")
        (conj! lines (str "- receive full-chain-refused: "
                         (boolean (:liquidity-receive-full-chain-refused liq true)) "\n"))
        (conj! lines (str "- member-principal / loan / cash: "
                         (boolean (:liquidity-member-principal liq true)) "/"
                         (boolean (:liquidity-loan-executed liq)) "/"
                         (or (:liquidity-cash-usd-micros liq) 0) "\n"))
        (conj! lines (str "- ladder: " (or (:ladder-advance-phase liq) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused liq)) "\n"))
        (when-let [hs (:held-stress liq)]
          (conj! lines (str "- held-stress held/receive-refused/ladder-refused/loan/cash: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:liquidity-receive-full-chain-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "/"
                           (boolean (:liquidity-loan-executed hs)) "/"
                           (or (:liquidity-cash-usd-micros hs) 0) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live liq)) "/"
                         (or (:cash-usd-micros liq) 0) "\n"))))
    (when-let [a7s (:report/l0-all-seven-substrate body)]
      (when (and (map? a7s) (not (:error a7s)))
        (conj! lines "\n## L0 all-seven substrate (capstone multi-gen + vocation + residual)\n")
        (conj! lines (str "- all-inkind / liq-recv / all-seven-membrane: "
                         (boolean (:all-inkind-produce-rails-full-chain-refused a7s true)) "/"
                         (boolean (:liquidity-receive-full-chain-refused a7s true)) "/"
                         (boolean (:all-seven-rails-receive-membrane-refused a7s true)) "\n"))
        (conj! lines (str "- loan / land-grant: "
                         (boolean (:liquidity-loan-executed a7s)) "/"
                         (boolean (:land-grant-executed a7s)) "\n"))
        (conj! lines (str "- ladder: " (or (:ladder-advance-phase a7s) "n/a")
                         "/refused=" (boolean (:ladder-advance-refused a7s)) "\n"))
        (when-let [hs (:held-stress a7s)]
          (conj! lines (str "- held-stress held/membrane/ladder-refused/loan/land-grant: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:all-seven-rails-receive-membrane-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "/"
                           (boolean (:liquidity-loan-executed hs)) "/"
                           (boolean (:land-grant-executed hs)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live a7s)) "/"
                         (or (:cash-usd-micros a7s) 0) "\n"))))
    (when-let [cat (:report/l0-priority-path-catalog body)]
      (when (and (map? cat) (not (:error cat)))
        (conj! lines "\n## L0 offline priority path catalog (discovery)\n")
        (conj! lines (str "- catalog-id: " (or (:catalog-id cat) "fuchi.l0-offline-priority-paths") "\n"))
        (conj! lines (str "- path-count: " (or (:path-count cat) 0) "\n"))
        (conj! lines (str "- held-stress-embed-count: "
                         (or (:held-stress-embed-count cat) 0) "\n"))
        (conj! lines (str "- path-ids: "
                         (str/join "," (map :id (or (:paths cat) []))) "\n"))
        (conj! lines (str "- loan-never/land-grant-never/held-stress-embed-all/cash: "
                         (boolean (get-in cat [:invariants :loan-never] true)) "/"
                         (boolean (get-in cat [:invariants :land-grant-never] true)) "/"
                         (boolean (get-in cat [:invariants :held-stress-embed-all] true)) "/"
                         (or (:cash-usd-micros cat) 0) "\n"))))
    (when-let [rc (:report/rail-care-design body)]
      (when (and (map? rc) (not (:error rc)))
        (conj! lines "\n## rail-care-iyashi DESIGN (priority 3 multi-gen #1)\n")
        (conj! lines (str "- rail-kind: " (or (:rail-kind rc) "care-iyashi") "\n"))
        (conj! lines (str "- care-first-order-rank / api-path: "
                         (or (:care-first-order-rank rc) 1) " / "
                         (or (:care-first-api-path rc) "care-housing-first-path") "\n"))
        (conj! lines (str "- multi-gen-first / care-delivery-executed: "
                         (boolean (:multi-gen-first rc true)) "/"
                         (boolean (:care-delivery-executed rc)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rc) [])) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live rc)) "/"
                         (or (:cash-usd-micros rc) 0) "\n"))))
    (when-let [rho (:report/rail-housing-design body)]
      (when (and (map? rho) (not (:error rho)))
        (conj! lines "\n## rail-housing-commons DESIGN (priority 3 multi-gen #2)\n")
        (conj! lines (str "- rail-kind: " (or (:rail-kind rho) "housing-commons") "\n"))
        (conj! lines (str "- care-first-before-rails: "
                         (str/join "," (or (:care-first-before-rails rho) ["care"])) "\n"))
        (conj! lines (str "- care-first-order-rank / api-path: "
                         (or (:care-first-order-rank rho) 2) " / "
                         (or (:care-first-api-path rho) "care-housing-first-path") "\n"))
        (conj! lines (str "- land-grant-executed / live-produce: "
                         (boolean (:land-grant-executed rho)) "/"
                         (boolean (:live-produce rho)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rho) [])) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live rho)) "/"
                         (or (:cash-usd-micros rho) 0) "\n"))))
    (when-let [rm (:report/rail-mitsuho-design body)]
      (when (and (map? rm) (not (:error rm)))
        (conj! lines "\n## rail-mitsuho DESIGN (priority 3 food R1→gated)\n")
        (conj! lines (str "- rail-kind: " (or (:rail-kind rm) "food-mitsuho") "\n"))
        (conj! lines (str "- care-first-before-rails: "
                         (str/join "," (or (:care-first-before-rails rm) [])) "\n"))
        (conj! lines (str "- care-first-api-path: "
                         (or (:care-first-api-path rm) "care-first-mitsuho-path") "\n"))
        (conj! lines (str "- live-produce / produce-executed: "
                         (boolean (:live-produce rm)) "/"
                         (boolean (:produce-executed rm)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rm) [])) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live rm)) "/"
                         (or (:cash-usd-micros rm) 0) "\n"))))
    (when-let [rh (:report/rail-hikari-design body)]
      (when (and (map? rh) (not (:error rh)))
        (conj! lines "\n## rail-hikari DESIGN (priority 3 energy R1→gated)\n")
        (conj! lines (str "- rail-kind: " (or (:rail-kind rh) "energy-hikari") "\n"))
        (conj! lines (str "- care-first-before-rails: "
                         (str/join "," (or (:care-first-before-rails rh) [])) "\n"))
        (conj! lines (str "- care-first-api-path: "
                         (or (:care-first-api-path rh) "care-first-hikari-path") "\n"))
        (conj! lines (str "- live-produce / generate-executed: "
                         (boolean (:live-produce rh)) "/"
                         (boolean (:generate-executed rh)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rh) [])) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live rh)) "/"
                         (or (:cash-usd-micros rh) 0) "\n"))))
    (when-let [rt (:report/rail-tooling-design body)]
      (when (and (map? rt) (not (:error rt)))
        (conj! lines "\n## rail-tooling-okaimono DESIGN (priority 3 vocation)\n")
        (conj! lines (str "- rail-kind: " (or (:rail-kind rt) "tooling-okaimono") "\n"))
        (conj! lines (str "- care-first-api-path / vocation-recovery: "
                         (or (:care-first-api-path rt) "vocation-recovery-path") " / "
                         (boolean (:vocation-recovery rt true)) "\n"))
        (conj! lines (str "- fulfillment-executed / live-produce: "
                         (boolean (:fulfillment-executed rt)) "/"
                         (boolean (:live-produce rt)) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live rt)) "/"
                         (or (:cash-usd-micros rt) 0) "\n"))))
    (when-let [rco (:report/rail-compute-design body)]
      (when (and (map? rco) (not (:error rco)))
        (conj! lines "\n## rail-compute-murakumo DESIGN (priority 3 vocation)\n")
        (conj! lines (str "- rail-kind: " (or (:rail-kind rco) "compute-murakumo") "\n"))
        (conj! lines (str "- care-first-api-path / vocation-recovery: "
                         (or (:care-first-api-path rco) "vocation-recovery-path") " / "
                         (boolean (:vocation-recovery rco true)) "\n"))
        (conj! lines (str "- quota-executed / live-produce: "
                         (boolean (:quota-executed rco)) "/"
                         (boolean (:live-produce rco)) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live rco)) "/"
                         (or (:cash-usd-micros rco) 0) "\n"))))
    (when-let [rl (:report/rail-liquidity-design body)]
      (when (and (map? rl) (not (:error rl)))
        (conj! lines "\n## rail-liquidity-warifu DESIGN (priority 3 residual)\n")
        (conj! lines (str "- rail-kind: " (or (:rail-kind rl) "liquidity-warifu") "\n"))
        (conj! lines (str "- care-first-api-path / residual-rail: "
                         (or (:care-first-api-path rl) "liquidity-residual-path") " / "
                         (boolean (:residual-rail rl true)) "\n"))
        (conj! lines (str "- member-principal / loan-executed / cash: "
                         (boolean (:member-principal rl true)) "/"
                         (boolean (:loan-executed rl)) "/"
                         (or (:cash-usd-micros rl) 0) "\n"))
        (conj! lines (str "- live: " (boolean (:live rl)) "\n"))))
    (when-let [rcat (:report/rail-design-catalog body)]
      (when (and (map? rcat) (not (:error rcat)))
        (conj! lines "\n## rail DESIGN catalog (all-seven discovery)\n")
        (conj! lines (str "- catalog-id: " (or (:catalog-id rcat) "fuchi.rail-design-catalog") "\n"))
        (conj! lines (str "- rail-count / ok-count: "
                         (or (:rail-count rcat) 0) "/"
                         (or (:ok-count rcat) 0) "\n"))
        (conj! lines (str "- rail-kinds: "
                         (str/join "," (or (:rail-kinds rcat) [])) "\n"))
        (conj! lines (str "- order: "
                         (str/join "→" (or (:order rcat) [])) "\n"))
        (conj! lines (str "- live-produce-never / all-cash-zero / all-live-false: "
                         (boolean (:live-produce-never rcat true)) "/"
                         (boolean (:all-cash-zero rcat true)) "/"
                         (boolean (:all-live-false rcat true)) "\n"))))
    (when-let [sp (:report/ss-priority-path body)]
      (when (and (map? sp) (not (:error sp)))
        (conj! lines "\n## SS priority path (offline L0 + disclosure + mitsuho/hikari)\n")
        (conj! lines (str "- path: " (or (:path sp) "ss-offline-inkind-rails") "\n"))
        (conj! lines (str "- did: " (last-seg (:did sp)) "\n"))
        (conj! lines (str "- (1) L0 stage/published/token-stub: "
                         (:l0-stage sp) "/"
                         (boolean (:l0-published sp)) "/"
                         (or (:l0-token-stub sp) "—") "\n"))
        (conj! lines (str "- (1) L0 enroll disclosure open/held/may-flow: "
                         (or (:l0-disclosure-state sp) "n/a") "/"
                         (boolean (:l0-disclosure-held sp)) "/"
                         (boolean (:l0-entitlements-may-flow sp true))
                         " path=" (or (:l0-path sp) "l0-enroll-offline") "\n"))
        (conj! lines (str "- (1) ladder climb offline: "
                         (or (:ladder-from sp) "L0") "→"
                         (or (:ladder-to sp) "—")
                         " target=" (or (:ladder-target sp) "—")
                         " steps=" (or (:ladder-steps sp) 0)
                         " phase=" (or (:ladder-phase sp) "—")
                         " published=" (boolean (:ladder-published sp)) "\n"))
        (conj! lines (str "- (1) ladder rails-hint (care-first for 孫/子): "
                         (str/join "," (or (:ladder-rails-hint sp) []))
                         " first=" (or (:ladder-rails-hint-first sp) "—") "\n"))
        (conj! lines (str "- (1) stage_sustenance after ladder: stage="
                         (or (:stage-sustenance-stage sp) "—")
                         " rails=" (str/join "," (or (:stage-rails sp) []))
                         " first/second="
                         (or (:stage-rails-first sp) "—") "/"
                         (or (:stage-rails-second sp) "—") "\n"))
        (conj! lines (str "- (1) stage floors care-h/housing-mo/floor-micros/land-grant/r2-all-refused: "
                         (or (:stage-care-hours-floor-yr sp) 0) "/"
                         (or (:stage-housing-months-floor-yr sp) 0) "/"
                         (or (:stage-floor-usd-micros-yr sp) 0) "/"
                         (boolean (:stage-land-grant-executed sp)) "/"
                         (boolean (:stage-r2-all-refused sp)) "\n"))
        (conj! lines (str "- (1) stage gated-live DESIGN count/admissible/all-refused: "
                         (or (:stage-gated-count sp) 0) "/"
                         (or (:stage-gated-admissible-count sp) 0) "/"
                         (boolean (:stage-all-gated-refused sp)) "\n"))
        (conj! lines (str "- (1) stage care/mitsuho/hikari gated-admissible: "
                         (boolean (:stage-care-gated-admissible sp)) "/"
                         (boolean (:stage-mitsuho-gated-admissible sp)) "/"
                         (boolean (:stage-hikari-gated-admissible sp)) "\n"))
        (conj! lines (str "- (2) disclosure open path: state="
                         (:disclosure-state sp)
                         " entitlements-may-flow="
                         (boolean (:entitlements-may-flow? sp)) "\n"))
        (conj! lines (str "- (2) held-stress: held="
                         (boolean (:held-stress-held? sp))
                         " food-r1-phase="
                         (or (:held-stress-food-phase sp) "—")
                         " ladder-refused="
                         (boolean (:held-stress-ladder-refused sp)) "\n"))
        (conj! lines (str "- (3) care/housing DESIGN live-produce / care-first-api (孫/子 first): "
                         (boolean (:care-live-produce sp)) "/"
                         (boolean (:housing-live-produce sp)) " · "
                         (or (:care-care-first-api-path sp) "care-housing-first-path") "/"
                         (or (:housing-care-first-api-path sp) "care-housing-first-path")
                         " ranks="
                         (or (:care-care-first-order-rank sp) 1) "/"
                         (or (:housing-care-first-order-rank sp) 2)
                         " kinds="
                         (or (:care-design-rail-kind sp) "care-iyashi") "/"
                         (or (:housing-design-rail-kind sp) "housing-commons") "\n"))
        (conj! lines (str "- (3) mitsuho/hikari DESIGN live-produce / care-first-api: "
                         (boolean (:mitsuho-live-produce sp)) "/"
                         (boolean (:hikari-live-produce sp)) " · "
                         (or (:mitsuho-care-first-api-path sp) "care-first-mitsuho-path") "/"
                         (or (:hikari-care-first-api-path sp) "care-first-hikari-path") "\n"))
        (conj! lines (str "- (3) tooling/compute/liquidity DESIGN live-produce / care-first-api: "
                         (boolean (:tooling-live-produce sp)) "/"
                         (boolean (:compute-live-produce sp)) "/"
                         (boolean (:liquidity-live-produce sp)) " · "
                         (or (:tooling-care-first-api-path sp) "vocation-recovery-path") "/"
                         (or (:compute-care-first-api-path sp) "vocation-recovery-path") "/"
                         (or (:liquidity-care-first-api-path sp) "liquidity-residual-path")
                         " kinds="
                         (or (:tooling-design-rail-kind sp) "tooling-okaimono") "/"
                         (or (:compute-design-rail-kind sp) "compute-murakumo") "/"
                         (or (:liquidity-design-rail-kind sp) "liquidity-warifu") "\n"))
        (conj! lines (str "- (3) all-seven design embed-count / live-produce-never: "
                         (or (:all-seven-design-embed-count sp) 7) "/"
                         (boolean (:all-seven-design-live-produce-never sp true)) "\n"))
        (conj! lines (str "- (3) mitsuho R1/gated-admissible/produce-executed: "
                         (or (:mitsuho-r1-phase sp) "—") "/"
                         (boolean (:mitsuho-gated-admissible sp)) "/"
                         (boolean (:mitsuho-produce-executed sp)) "\n"))
        (conj! lines (str "- (3) hikari R1/gated-admissible/generate-executed: "
                         (or (:hikari-r1-phase sp) "—") "/"
                         (boolean (:hikari-gated-admissible sp)) "/"
                         (boolean (:hikari-generate-executed sp)) "\n"))
        (conj! lines (str "- (3) mitsuho/hikari gated-receive admissible/phase: "
                         (boolean (:mitsuho-gated-receive-admissible sp)) "/"
                         (or (:mitsuho-gated-receive-phase sp) "—") " · "
                         (boolean (:hikari-gated-receive-admissible sp)) "/"
                         (or (:hikari-gated-receive-phase sp) "—")
                         " both-refused="
                         (boolean (:mitsuho-hikari-receive-both-refused sp)) "\n"))
        (conj! lines (str "- (3) care-iyashi gated-receive (孫/子) admissible/phase: "
                         (boolean (:care-gated-receive-admissible sp)) "/"
                         (or (:care-gated-receive-phase sp) "—")
                         " care+mitsuho+hikari-all-refused="
                         (boolean (:care-mitsuho-hikari-receive-all-refused sp)) "\n"))
        (conj! lines (str "- (3) mitsuho/hikari gated-produce admissible/phase: "
                         (boolean (:mitsuho-gated-produce-admissible sp)) "/"
                         (or (:mitsuho-gated-produce-phase sp) "—") " · "
                         (boolean (:hikari-gated-produce-admissible sp)) "/"
                         (or (:hikari-gated-produce-phase sp) "—")
                         " both-refused="
                         (boolean (:mitsuho-hikari-produce-both-refused sp))
                         " full-chain-refused="
                         (boolean (:mitsuho-hikari-full-chain-refused sp)) "\n"))
        (conj! lines (str "- (3) care-iyashi gated-produce (孫/子) admissible/phase: "
                         (boolean (:care-gated-produce-admissible sp)) "/"
                         (or (:care-gated-produce-phase sp) "—")
                         " care+food+energy produce-all-refused="
                         (boolean (:care-mitsuho-hikari-produce-all-refused sp))
                         " care+food+energy full-chain-refused="
                         (boolean (:care-mitsuho-hikari-full-chain-refused sp)) "\n"))
        (conj! lines (str "- (3) housing-commons gated-receive/produce (孫/子) admissible/phase: "
                         (boolean (:housing-gated-receive-admissible sp)) "/"
                         (or (:housing-gated-receive-phase sp) "—") " · "
                         (boolean (:housing-gated-produce-admissible sp)) "/"
                         (or (:housing-gated-produce-phase sp) "—")
                         " housing full-chain-refused="
                         (boolean (:housing-full-chain-refused sp))
                         " care+housing+food+energy full-chain-refused="
                         (boolean (:care-housing-mitsuho-hikari-full-chain-refused sp)) "\n"))
        (conj! lines (str "- (3) tooling/compute gated-receive/produce admissible/phase: "
                         (boolean (:tooling-gated-receive-admissible sp)) "/"
                         (or (:tooling-gated-receive-phase sp) "—") " · "
                         (boolean (:tooling-gated-produce-admissible sp)) "/"
                         (or (:tooling-gated-produce-phase sp) "—")
                         " · "
                         (boolean (:compute-gated-receive-admissible sp)) "/"
                         (or (:compute-gated-receive-phase sp) "—") " · "
                         (boolean (:compute-gated-produce-admissible sp)) "/"
                         (or (:compute-gated-produce-phase sp) "—")
                         " tooling+compute full-chain-refused="
                         (boolean (:tooling-compute-full-chain-refused sp))
                         " all-inkind-produce-rails full-chain-refused="
                         (boolean (:all-inkind-produce-rails-full-chain-refused sp)) "\n"))
        (conj! lines (str "- (3) liquidity-warifu gated-receive admissible/phase: "
                         (boolean (:liquidity-gated-receive-admissible sp)) "/"
                         (or (:liquidity-gated-receive-phase sp) "—")
                         " receive-full-chain-refused="
                         (boolean (:liquidity-receive-full-chain-refused sp))
                         " all-seven-rails receive-membrane-refused="
                         (boolean (:all-seven-rails-receive-membrane-refused sp)) "\n"))
        (conj! lines (str "- (3) care/housing/tooling/compute/liquidity gated-admissible: "
                         (boolean (:care-gated-admissible sp)) "/"
                         (boolean (:housing-gated-admissible sp)) "/"
                         (boolean (:tooling-gated-admissible sp)) "/"
                         (boolean (:compute-gated-admissible sp)) "/"
                         (boolean (:liquidity-gated-admissible sp)) "\n"))
        (conj! lines (str "- (3) rails-gated-count/admissible/all-rails-gated-refused: "
                         (or (:rails-gated-count sp) 0) "/"
                         (or (:rails-gated-admissible-count sp) 0) "/"
                         (boolean (:all-rails-gated-refused sp)) "\n"))
        (conj! lines (str "- housing land-grant-executed / liquidity loan-executed/cash: "
                         (boolean (:housing-land-grant-executed sp)) "/"
                         (boolean (:liquidity-loan-executed sp)) "/"
                         (or (:liquidity-cash-usd-micros sp) 0) "\n"))
        (conj! lines (str "- R2 statuses/executed/all-not-executed: "
                         (or (:r2-status-count sp) 0) "/"
                         (or (:r2-executed-count sp) 0) "/"
                         (boolean (:all-r2-not-executed sp true)) "\n"))
        (conj! lines (str "- R2 food/energy executed: "
                         (boolean (:r2-food-executed sp)) "/"
                         (boolean (:r2-energy-executed sp))
                         " (phases "
                         (or (:r2-food-phase sp) "—") "/"
                         (or (:r2-energy-phase sp) "—") ")\n"))
        (conj! lines (str "- live: " (boolean (:live sp))
                         " cash: " (or (:cash-usd-micros sp) 0) "\n"))))
    (when-let [l0 (:report/l0-demo body)]
      (conj! lines "\n## L0 demo (offline)\n")
      (conj! lines (str "- did: " (last-seg (:did l0)) " stage=" (:stage l0)
                        " public=" (:public-person? l0) " cash=0 live=false\n")))
    (conj! lines "\n_No personal scores, ranks, or percentiles._\n")
    (apply str (persistent! lines))))

(defn report-html
  "Minimal static HTML public surface (facts only). No live, no scores."
  [seed & {:keys [include-l0-demo include-itonami include-displacement-l0]
           :or {include-displacement-l0 true}}]
  (let [body (report-edn seed :include-l0-demo include-l0-demo :include-itonami include-itonami
                         :include-displacement-l0 include-displacement-l0)
        esc (fn [x] (-> (str x)
                        (str/replace "&" "&amp;")
                        (str/replace "<" "&lt;")
                        (str/replace ">" "&gt;")))
        rows (fn [header cells]
               (str "<tr>" (apply str (map #(str "<" header ">" (esc %) "</" header ">") cells)) "</tr>"))]
    (str
     "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\"/>"
     "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\"/>"
     "<title>fuchi public surface (facts only)</title>"
     "<style>body{font-family:system-ui,sans-serif;margin:1.5rem;line-height:1.4}"
     "table{border-collapse:collapse;width:100%;margin:1rem 0}"
     "th,td{border:1px solid #ccc;padding:.4rem .6rem;text-align:left}"
     "th{background:#f4f4f4}.note{color:#444;font-size:.9rem}</style></head><body>"
     "<h1>fuchi — public surface (facts only)</h1>"
     "<p class=\"note\">Priority: wellbecoming &gt; mago(孫) &gt; ko(子) &gt; present. "
     "cash≡0. live=false. No personal scores or ranks. Floors are dry plans only.</p>"
     "<h2>Public persons</h2><table><thead>"
     (rows "th" ["did" "covenant" "public?" "disclosure" "rails" "imputed"])
     "</thead><tbody>"
     (apply str
            (for [f (:report/public-persons body)]
              (rows "td" [(last-seg (:did f)) (:covenant f) (:public-person? f)
                          (:disclosure-status f) (str/join "," (:rails f))
                          (:imputed-fact f)])))
     "</tbody></table>"
     "<h2>Dry floor plans (produce-executed=false)</h2><table><thead>"
     (rows "th" ["did" "kcal" "kWh" "care-h" "housing-mo" "tools" "GPU-h"])
     "</thead><tbody>"
     (apply str
            (for [r (:report/rail-packages body)]
              (rows "td" [(last-seg (:did r))
                          (or (get-in r [:food-floor :kcal-floor-yr]) "—")
                          (or (get-in r [:energy-floor :kwh-floor-yr]) "—")
                          (or (get-in r [:care-floor :care-hours-floor-yr]) "—")
                          (or (get-in r [:housing-floor :housing-months-floor-yr]) "—")
                          (or (get-in r [:tooling-floor :tool-units-floor-yr]) "—")
                          (or (get-in r [:compute-floor :gpu-hours-floor-yr]) "—")])))
     "</tbody></table>"
     "<h2>Displacement → earmark</h2><table><thead>"
     (rows "th" ["actor" "cohort" "displaced" "funded" "admissible" "earmark"])
     "</thead><tbody>"
     (apply str
            (for [d (:report/displacement body)]
              (rows "td" [(:displacing-actor d) (:cohort-id d) (:displaced-count d)
                          (:funded d) (:admissible d) (:earmark-usd-micros-yr d)])))
     "</tbody></table>"
     (when (seq (:report/itonami-displacement body))
       (str
        "<h2>itonami surplus bridge (offline)</h2><table><thead>"
        (rows "th" ["actor" "cohort" "displaced" "funded" "admissible"])
        "</thead><tbody>"
        (apply str
               (for [d (:report/itonami-displacement body)]
                 (rows "td" [(:displacing-actor d) (:cohort-id d) (:displaced-count d)
                             (:funded d) (:admissible d)])))
        "</tbody></table>"))
     (when (seq (get-in body [:report/displacement-l0 :packages]))
       (str
        "<h2>Displacement → L0→L4 enroll (offline)</h2>"
        "<p class=\"note\">Funded cohorts open L0 climb to L4 multi-gen (care/housing first); unfunded refuse. "
        "enrolled-subjects=" (get-in body [:report/displacement-l0 :enrolled-subjects])
        " refused-cohorts=" (get-in body [:report/displacement-l0 :refused-cohorts])
        " stages=" (pr-str (get-in body [:report/displacement-l0 :stage-counts]))
        " disclosure-open=" (or (get-in body [:report/displacement-l0 :disclosure-open]) 0)
        " disclosure-held=" (or (get-in body [:report/displacement-l0 :disclosure-held]) 0)
        " g2-admissible-cohorts=" (or (get-in body [:report/displacement-l0 :g2-admissible-cohorts]) 0)
        " earmark-total=" (or (get-in body [:report/displacement-l0 :earmark-usd-micros-yr]) 0)
        " committed-flowable-total=" (or (get-in body [:report/displacement-l0 :committed-usd-micros-yr]) 0)
        " committed-full-total=" (or (get-in body [:report/displacement-l0 :committed-full-usd-micros-yr]) 0)
        " tenure-subjects=" (or (get-in body [:report/displacement-l0 :tenure-subjects]) 0)
        " tenure-committed-flow=" (or (get-in body [:report/displacement-l0 :tenure-committed-usd-micros-yr]) 0)
        " gov-post-ratify-total=" (or (get-in body [:report/displacement-l0 :gov-post-ratify-usd-micros]) 0)
        " tenure-gov-post-ratify=" (or (get-in body [:report/displacement-l0 :tenure-gov-post-ratify-usd-micros]) 0)
        " land-grant-executed=" (or (get-in body [:report/displacement-l0 :housing-land-grant-executed]) 0)
        " (post-ratify keeps land-grant=false)."
        " L0 membranes subjects/all-inkind/all-seven/liq-recv="
        (or (get-in body [:report/displacement-l0 :membrane-subjects]) 0) "/"
        (or (get-in body [:report/displacement-l0 :all-inkind-full-chain-refused-n]) 0) "/"
        (or (get-in body [:report/displacement-l0 :all-seven-receive-membrane-refused-n]) 0) "/"
        (or (get-in body [:report/displacement-l0 :liquidity-recv-refused-n]) 0)
        " held-stress subjects/ladder-refused="
        (or (get-in body [:report/displacement-l0 :held-stress-subjects]) 0) "/"
        (or (get-in body [:report/displacement-l0 :held-stress-ladder-refused-subjects]) 0)
        " (default refuse; liquidity residual member-principal).</p>"
        "<table><thead>"
        (rows "th" ["actor" "cohort" "phase" "n" "g2" "funded" "earmark" "disc-o/h"
                    "L4-flow" "L4-full" "L4-post" "ten-n" "ten-flow" "ten-post"
                    "land-grant" "headroom"])
        "</thead><tbody>"
        (apply str
               (for [p (get-in body [:report/displacement-l0 :packages])]
                 (rows "td" [(:displacing-actor p) (:cohort-id p)
                             (:phase p) (:subject-count p)
                             (:g2-admissible p)
                             (boolean (:funded p))
                             (or (:earmark-usd-micros-yr p) 0)
                             (str (or (:disclosure-open p) 0) "/"
                                  (or (:disclosure-held p) 0))
                             (:committed-usd-micros-yr p)
                             (or (:committed-full-usd-micros-yr p) 0)
                             (or (:gov-post-ratify-usd-micros p) 0)
                             (or (:tenure-subjects p) 0)
                             (or (:tenure-committed-usd-micros-yr p) 0)
                             (or (:tenure-gov-post-ratify-usd-micros p) 0)
                             (or (:housing-land-grant-executed p) 0)
                             (:headroom-usd-micros-yr p)])))
        "</tbody></table>"))
     (when (get-in body [:report/displacement-scorecard :scorecard/id])
       (let [sc (:report/displacement-scorecard body)
             st (:scorecard/all-held-stress sc)]
         (str
          "<h2>SS scorecard (offline)</h2>"
          "<p class=\"note\">all-live-refused="
          (:scorecard/all-live-refused sc)
          " booked-entries=" (:scorecard/booked-entries sc)
          " committed-flowable=" (:scorecard/committed-usd-micros-yr sc)
          " gov-routes=" (pr-str (:scorecard/gov-route-counts sc))
          " gov-post-ratify=" (or (:scorecard/gov-post-ratify-committed-usd-micros sc) 0)
          " tenure-gov-post-ratify=" (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros sc) 0)
          " land-grant-executed=" (or (:scorecard/housing-land-grant-executed sc) 0)
          " r2-refused/executed=" (or (:scorecard/r2-refused sc) 0) "/"
          (or (:scorecard/r2-executed sc) 0)
          " all-r2-not-executed=" (boolean (:scorecard/all-r2-not-executed sc true))
          ". Housing held for Council; multi-gen substrate may dry-flow; post-ratify plan keeps land-grant=false; R2 execute default refuse.</p>"
          "<table><thead>"
          (rows "th" ["rail" "R1-dry" "gated-refused" "executed"])
          "</thead><tbody>"
          (rows "td" ["mitsuho food"
                      (or (:scorecard/mitsuho-r1-dry sc) 0)
                      (or (:scorecard/mitsuho-gated-refused sc) 0)
                      (or (:scorecard/mitsuho-produce-executed sc) 0)])
          (rows "td" ["hikari energy"
                      (or (:scorecard/hikari-r1-dry sc) 0)
                      (or (:scorecard/hikari-gated-refused sc) 0)
                      (or (:scorecard/hikari-generate-executed sc) 0)])
          (rows "td" ["care-iyashi"
                      (or (:scorecard/care-r1-dry sc) 0)
                      (or (:scorecard/care-gated-refused sc) 0)
                      (or (:scorecard/care-delivery-executed sc) 0)])
          (rows "td" ["housing-commons"
                      (or (:scorecard/housing-r1-dry sc) 0)
                      (or (:scorecard/housing-gated-refused sc) 0)
                      (or (:scorecard/housing-land-grant-executed sc) 0)])
          (rows "td" ["tooling-okaimono"
                      (or (:scorecard/tooling-r1-dry sc) 0)
                      (or (:scorecard/tooling-gated-refused sc) 0)
                      (or (:scorecard/tooling-fulfillment-executed sc) 0)])
          (rows "td" ["compute-murakumo"
                      (or (:scorecard/compute-r1-dry sc) 0)
                      (or (:scorecard/compute-gated-refused sc) 0)
                      (or (:scorecard/compute-quota-executed sc) 0)])
          (rows "td" ["liquidity-warifu"
                      (or (:scorecard/liquidity-r1-dry sc) 0)
                      (or (:scorecard/liquidity-gated-refused sc) 0)
                      (or (:scorecard/liquidity-loan-executed sc) 0)])
          "</tbody></table>"
          "<p class=\"note\">liquidity residual (housing Council-held): member-principal="
          (or (:scorecard/liquidity-member-principal sc) 0)
          " cash-usd-micros="
          (or (:scorecard/liquidity-cash-usd-micros sc) 0)
          " (fuchi never cash creditor). housing-council-held="
          (or (:scorecard/housing-council-held sc) 0)
          " land-grant-executed="
          (or (:scorecard/housing-land-grant-executed sc) 0)
          ".</p>"
          "<p class=\"note\">displacement L0 membranes subjects/care+housing/all-inkind/all-seven/liq-recv="
          (or (:scorecard/displacement-membrane-subjects sc) 0) "/"
          (or (:scorecard/displacement-care-housing-full-chain-refused sc) 0) "/"
          (or (:scorecard/displacement-all-inkind-full-chain-refused sc) 0) "/"
          (or (:scorecard/displacement-all-seven-receive-membrane-refused sc) 0) "/"
          (or (:scorecard/displacement-liquidity-recv-refused sc) 0)
          " held-stress subjects/ladder-refused="
          (or (:scorecard/displacement-held-stress-subjects sc) 0) "/"
          (or (:scorecard/displacement-held-stress-ladder-refused sc) 0)
          " tenure held-stress subjects/ladder-refused/carried="
          (or (:scorecard/tenure-held-stress-subjects sc) 0) "/"
          (or (:scorecard/tenure-held-stress-ladder-refused sc) 0) "/"
          (or (:scorecard/tenure-held-stress-carried sc) 0)
          " gov held-stress subjects/ladder-refused="
          (or (:scorecard/gov-held-stress-subjects sc) 0) "/"
          (or (:scorecard/gov-held-stress-ladder-refused sc) 0)
          " tenure-gov held-stress subjects/ladder-refused="
          (or (:scorecard/tenure-gov-held-stress-subjects sc) 0) "/"
          (or (:scorecard/tenure-gov-held-stress-ladder-refused sc) 0)
          " (gated DESIGN default refuse).</p>"
          (when st
            (str
             "<h3>All-disclosure-held stress (priority #2)</h3>"
             "<p class=\"note\">held-subjects=" (:held-subjects st)
             " open-gov-flowable=" (:open-gov-flowable st)
             " all-held-gov-flowable=" (:gov-flowable st)
             " land-grant-executed=" (:land-grant-executed st)
             " live=" (:live st)
             " cash=" (:cash-usd-micros st) ".</p>")))))
     (when-let [dep (:report/pages-deploy-status body)]
       (str
        "<h2>Pages deploy (offline membrane, plan-only)</h2>"
        "<p class=\"note\">phase=" (:phase dep)
        " admissible=" (:admissible dep)
        " authorized-to-deploy=" (boolean (:authorized-to-deploy dep))
        " package-ready=" (boolean (:package-ready dep true))
        " wrangler-invoked=" (boolean (:wrangler-invoked dep))
        " cloudflare-api-invoked=" (boolean (:cloudflare-api-invoked dep))
        " operator-flag=" (or (:operator-flag dep) "FUCHI_ALLOW_PAGES_DEPLOY")
        " deployed=" (:deployed dep)
        " live=" (:live dep)
        " cash=" (:cash-usd-micros dep)
        ". " (or (:note dep) "default refuse — static package only")
        " Gated plan still requires flag+operator-did and never invokes wrangler here;"
        " actual deploy is operator out-of-band."
        "</p>"))
     (when-let [au (:report/pipeline-audit body)]
       (when (or (pos? (or (:runs au) 0)) (map? (:last-run au)))
         (str
          "<h2>Pipeline audit summary (offline)</h2>"
          "<p class=\"note\">runs=" (or (:runs au) 0)
          " all-runs-live-refused=" (boolean (:all-runs-live-refused au))
          " any-land-grant-executed=" (boolean (:any-land-grant-executed? au))
          " last-run gov-flowable/gov-post-ratify="
          (or (:last-run-gov-flowable-committed-usd-micros au) 0) "/"
          (or (:last-run-gov-post-ratify-committed-usd-micros au) 0)
          " tenure-gov-flowable/tenure-gov-post-ratify="
          (or (:last-run-tenure-gov-flowable-committed-usd-micros au) 0) "/"
          (or (:last-run-tenure-gov-post-ratify-committed-usd-micros au) 0)
          " land-grant-executed="
          (or (:last-run-housing-land-grant-executed au) 0)
          " (post-ratify keeps land-grant=false)."
          " last-run R2 refused/executed="
          (or (:last-run-r2-refused au) 0) "/"
          (or (:last-run-r2-executed au) 0)
          " all-r2-not-executed="
          (boolean (:last-run-all-r2-not-executed au true))
          " last-run SS rails-gated/all-refused="
          (or (:last-run-ss-rails-gated-count au) 0) "/"
          (boolean (:last-run-ss-all-rails-gated-refused au true))
          " ss-all-r2-not-executed="
          (boolean (:last-run-ss-all-r2-not-executed au true))
          " ladder-to=" (or (:last-run-ss-ladder-to au) "n/a")
          " stage-rails="
          (or (:last-run-ss-stage-rails-first au) "n/a") "/"
          (or (:last-run-ss-stage-rails-second au) "n/a")
          " stage-gated="
          (or (:last-run-ss-stage-gated-count au) 0)
          " stage-all-gated-refused="
          (boolean (:last-run-ss-stage-all-gated-refused au true))
          " stage-care/mitsuho/hikari-gated="
          (boolean (:last-run-ss-stage-care-gated-admissible au)) "/"
          (boolean (:last-run-ss-stage-mitsuho-gated-admissible au)) "/"
          (boolean (:last-run-ss-stage-hikari-gated-admissible au))
          " stage-land-grant="
          (boolean (:last-run-ss-stage-land-grant-executed au))
          " mitsuho/hikari/care-recv="
          (boolean (:last-run-ss-mitsuho-gated-receive-admissible au)) "/"
          (boolean (:last-run-ss-hikari-gated-receive-admissible au)) "/"
          (boolean (:last-run-ss-care-gated-receive-admissible au))
          " recv-all-three-refused="
          (boolean (:last-run-ss-care-mitsuho-hikari-receive-all-refused au true))
          " mitsuho/hikari/care-produce="
          (boolean (:last-run-ss-mitsuho-gated-produce-admissible au)) "/"
          (boolean (:last-run-ss-hikari-gated-produce-admissible au)) "/"
          (boolean (:last-run-ss-care-gated-produce-admissible au))
          " produce-full-chain-refused="
          (boolean (:last-run-ss-care-mitsuho-hikari-full-chain-refused au true))
          " housing-recv/produce="
          (boolean (:last-run-ss-housing-gated-receive-admissible au)) "/"
          (boolean (:last-run-ss-housing-gated-produce-admissible au))
          " housing-full-chain-refused="
          (boolean (:last-run-ss-housing-full-chain-refused au true))
          " care+housing+food+energy-full-chain="
          (boolean (:last-run-ss-care-housing-mitsuho-hikari-full-chain-refused au true))
          " tooling/compute-full-chain="
          (boolean (:last-run-ss-tooling-full-chain-refused au true)) "/"
          (boolean (:last-run-ss-compute-full-chain-refused au true))
          " all-inkind-produce-rails-full-chain="
          (boolean (:last-run-ss-all-inkind-produce-rails-full-chain-refused au true))
          " liquidity-recv/all-seven-membrane="
          (boolean (:last-run-ss-liquidity-gated-receive-admissible au)) "/"
          (boolean (:last-run-ss-all-seven-rails-receive-membrane-refused au true))
          " liquidity member-principal/cash="
          (or (:total-liquidity-member-principal au) 0) "/"
          (or (:total-liquidity-cash-usd-micros au) 0)
          " live=" (boolean (:live au))
          " cash=" (or (:cash-usd-micros au) 0)
          ".</p>")))
     (when-let [ps (:report/priority-stack-offline body)]
       (when (map? ps)
         (str
          "<h2>Priority stack offline SSoT (1)L0 (2)disclosure (3)care-housing→mitsuho+hikari→all-seven</h2>"
          "<p class=\"note\">ok=" (boolean (:ok ps))
          " L0=" (or (:l0-stage ps) "—")
          "/published=" (boolean (:l0-published ps))
          " disclosure open/stale-held/tick-final="
          (boolean (:disclosure-open-may-flow ps)) "/"
          (boolean (:disclosure-stale-held ps)) "/"
          (or (:disclosure-tick-final ps) "—")
          " care-housing=" (or (:care-housing-api-path ps) "—")
          "/both-refused=" (boolean (:care-housing-both-refused ps))
          "/land-grant=" (boolean (:care-housing-land-grant-executed ps))
          " mitsuho R1/gated/produce="
          (or (:mitsuho-r1-phase ps) "—") "/"
          (or (:mitsuho-gated-phase ps) "—") "/"
          (boolean (:mitsuho-produce-executed ps))
          " care-first=" (or (:mitsuho-care-first-api-path ps) "—")
          " held-stress-ladder=" (boolean (:mitsuho-held-stress-ladder-refused ps))
          " hikari R1/gated/produce="
          (or (:hikari-r1-phase ps) "—") "/"
          (or (:hikari-gated-phase ps) "—") "/"
          (boolean (:hikari-produce-executed ps))
          " hikari-care-first=" (or (:hikari-care-first-api-path ps) "—")
          " hikari-held-stress=" (boolean (:hikari-held-stress-ladder-refused ps))
          " all-seven=" (or (:all-seven-api-path ps) "—")
          "/membrane=" (boolean (:all-seven-membrane-refused ps))
          "/loan=" (boolean (:all-seven-loan-executed ps))
          " l0-paths=" (or (:l0-paths-count ps) 0)
          "/all-held=" (boolean (:l0-paths-all-held-stress ps))
          " design-id=" (or (:design-id ps) "—")
          " order=" (or (:design-order-count ps) 0)
          " live=" (boolean (:live ps))
          " cash=" (or (:cash-usd-micros ps) 0)
          ".</p>")))
     (when-let [l7 (:report/l0-all-seven-enroll body)]
       (when (and (map? l7) (not (:error l7)))
         (str
          "<h2>L0 enroll all-seven rails (priority smoke)</h2>"
          "<p class=\"note\">api=" (or (:api l7) "enroll-with-all-seven-rails")
          " disclosure=" (or (:disclosure-state l7) "n/a")
          "/held=" (boolean (:disclosure-held l7))
          "/may-flow=" (boolean (:entitlements-may-flow l7 true))
          " all-inkind/liq-recv/all-seven="
          (boolean (:all-inkind-produce-rails-full-chain-refused l7)) "/"
          (boolean (:liquidity-receive-full-chain-refused l7)) "/"
          (boolean (:all-seven-rails-receive-membrane-refused l7))
          " continuity=" (or (:continuity-final-state l7) "n/a")
          "/held-steps=" (or (:continuity-held-steps l7) 0)
          " ladder=" (or (:ladder-advance-phase l7) "n/a")
          "/refused=" (boolean (:ladder-advance-refused l7))
          " member-principal=" (boolean (:liquidity-member-principal l7 true))
          " loan=" (boolean (:liquidity-loan-executed l7))
          " cash=" (or (:liquidity-cash-usd-micros l7) 0)
          " land-grant=" (boolean (:land-grant-executed l7))
          " live=" (boolean (:live l7))
          ".</p>")))
     (when-let [lh (:report/l0-held-all-seven-enroll body)]
       (when (and (map? lh) (not (:error lh)))
         (str
          "<h2>L0 held all-seven (disclosure stale stress)</h2>"
          "<p class=\"note\">held=" (boolean (:disclosure-held lh true))
          " may-flow=" (boolean (:entitlements-may-flow lh))
          " all-seven-membrane=" (boolean (:all-seven-rails-receive-membrane-refused lh true))
          " ladder=" (or (:ladder-advance-phase lh) "n/a")
          "/refused=" (boolean (:ladder-advance-refused lh true))
          " loan=" (boolean (:liquidity-loan-executed lh))
          " land-grant=" (boolean (:land-grant-executed lh))
          " live=" (boolean (:live lh))
          ".</p>")))
     (when-let [ex (:report/l0-exit-reaffirm body)]
       (when (and (map? ex) (not (:error ex)))
         (str
          "<h2>L0 exit→re-affirm (disclosure SM)</h2>"
          "<p class=\"note\">exit=" (or (:exit-state ex) "n/a")
          "/ladder-refused=" (boolean (:exit-ladder-refused ex true))
          " reaffirm=" (or (:reaffirm-state ex) "n/a")
          "/may-flow=" (boolean (:reaffirm-entitlements-may-flow ex true))
          "/ladder=" (or (:reaffirm-ladder-phase ex) "n/a")
          "/refused=" (boolean (:reaffirm-ladder-refused ex))
          " live=" (boolean (:live ex))
          ".</p>")))
     (when-let [fl (:report/l0-falsehood-lift body)]
       (when (and (map? fl) (not (:error fl)))
         (str
          "<h2>L0 falsehood→lift-hold</h2>"
          "<p class=\"note\">falsehood-held=" (boolean (:falsehood-held? fl true))
          " ladder-refused=" (boolean (:falsehood-ladder-refused fl true))
          " lift=" (or (:lift-state fl) "n/a")
          "/ladder=" (or (:lift-ladder-phase fl) "n/a")
          "/refused=" (boolean (:lift-ladder-refused fl))
          ".</p>")))
     (when-let [cf (:report/l0-care-first-mitsuho body)]
       (when (and (map? cf) (not (:error cf)))
         (str
          "<h2>L0 care-first + mitsuho (孫/子)</h2>"
          "<p class=\"note\">both-refused=" (boolean (:care-mitsuho-both-refused cf true))
          " care-first-api=" (or (:care-first-api-path cf) "care-first-mitsuho-path")
          " mitsuho-live-produce=" (boolean (:mitsuho-live-produce cf))
          " mitsuho-produce-executed=" (boolean (:mitsuho-produce-executed cf))
          " care-delivery-executed=" (boolean (:care-delivery-executed cf))
          " ladder=" (or (:ladder-advance-phase cf) "n/a")
          "/refused=" (boolean (:ladder-advance-refused cf))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused cf))
          " live=" (boolean (:live cf))
          ".</p>")))
     (when-let [ch (:report/l0-care-first-hikari body)]
       (when (and (map? ch) (not (:error ch)))
         (str
          "<h2>L0 care-first + hikari (孫/子 + energy)</h2>"
          "<p class=\"note\">both-refused=" (boolean (:care-hikari-both-refused ch true))
          " care-first-api=" (or (:care-first-api-path ch) "care-first-hikari-path")
          " hikari-live-produce=" (boolean (:hikari-live-produce ch))
          " hikari-generate-executed=" (boolean (:hikari-generate-executed ch))
          " ladder=" (or (:ladder-advance-phase ch) "n/a")
          "/refused=" (boolean (:ladder-advance-refused ch))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused ch))
          " live=" (boolean (:live ch))
          ".</p>")))
     (when-let [cfh (:report/l0-care-first-mitsuho-hikari body)]
       (when (and (map? cfh) (not (:error cfh)))
         (str
          "<h2>L0 care-first + mitsuho + hikari (孫/子 dual rail)</h2>"
          "<p class=\"note\">all-refused=" (boolean (:care-mitsuho-hikari-all-refused cfh true))
          " mitsuho+hikari-both=" (boolean (:mitsuho-hikari-both-refused cfh true))
          " mitsuho-live-produce=" (boolean (:mitsuho-live-produce cfh))
          " hikari-live-produce=" (boolean (:hikari-live-produce cfh))
          " ladder=" (or (:ladder-advance-phase cfh) "n/a")
          "/refused=" (boolean (:ladder-advance-refused cfh))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused cfh))
          " live=" (boolean (:live cfh))
          " cash=" (or (:cash-usd-micros cfh) 0)
          ".</p>")))
     (when-let [chs (:report/l0-care-housing-first body)]
       (when (and (map? chs) (not (:error chs)))
         (str
          "<h2>L0 care+housing multi-gen substrate (孫/子)</h2>"
          "<p class=\"note\">both-refused=" (boolean (:care-housing-both-refused chs true))
          " land-grant=" (boolean (:land-grant-executed chs))
          " ladder=" (or (:ladder-advance-phase chs) "n/a")
          "/refused=" (boolean (:ladder-advance-refused chs))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused chs))
          " live=" (boolean (:live chs))
          ".</p>")))
     (when-let [mgs (:report/l0-multi-gen-substrate body)]
       (when (and (map? mgs) (not (:error mgs)))
         (str
          "<h2>L0 multi-gen substrate + mitsuho+hikari (L4 priority)</h2>"
          "<p class=\"note\">all-refused="
          (boolean (:care-housing-mitsuho-hikari-all-refused mgs true))
          " care+housing=" (boolean (:care-housing-both-refused mgs true))
          " mitsuho+hikari=" (boolean (:mitsuho-hikari-both-refused mgs true))
          " land-grant=" (boolean (:land-grant-executed mgs))
          " ladder=" (or (:ladder-advance-phase mgs) "n/a")
          "/refused=" (boolean (:ladder-advance-refused mgs))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused mgs))
          " live=" (boolean (:live mgs))
          " cash=" (or (:cash-usd-micros mgs) 0)
          ".</p>")))
     (when-let [fis (:report/l0-full-inkind-substrate body)]
       (when (and (map? fis) (not (:error fis)))
         (str
          "<h2>L0 full in-kind substrate (multi-gen + vocation / itonami)</h2>"
          "<p class=\"note\">six-all-refused="
          (boolean (:all-inkind-produce-rails-full-chain-refused fis true))
          " tooling+compute=" (boolean (:tooling-compute-both-refused fis true))
          " land-grant/fulfillment/quota="
          (boolean (:land-grant-executed fis)) "/"
          (boolean (:fulfillment-executed fis)) "/"
          (boolean (:quota-executed fis))
          " ladder=" (or (:ladder-advance-phase fis) "n/a")
          "/refused=" (boolean (:ladder-advance-refused fis))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused fis))
          " live=" (boolean (:live fis))
          " cash=" (or (:cash-usd-micros fis) 0)
          ".</p>")))
     (when-let [voc (:report/l0-vocation-recovery body)]
       (when (and (map? voc) (not (:error voc)))
         (str
          "<h2>L0 vocation recovery (tooling+compute / itonami job-loss)</h2>"
          "<p class=\"note\">both-refused="
          (boolean (:tooling-compute-both-refused voc true))
          " fulfillment/quota="
          (boolean (:fulfillment-executed voc)) "/"
          (boolean (:quota-executed voc))
          " ladder=" (or (:ladder-advance-phase voc) "n/a")
          "/refused=" (boolean (:ladder-advance-refused voc))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused voc))
          " live=" (boolean (:live voc))
          " cash=" (or (:cash-usd-micros voc) 0)
          ".</p>")))
     (when-let [liq (:report/l0-liquidity-residual body)]
       (when (and (map? liq) (not (:error liq)))
         (str
          "<h2>L0 liquidity residual (warifu member-principal)</h2>"
          "<p class=\"note\">receive-refused="
          (boolean (:liquidity-receive-full-chain-refused liq true))
          " member-principal=" (boolean (:liquidity-member-principal liq true))
          " loan=" (boolean (:liquidity-loan-executed liq))
          " liq-cash=" (or (:liquidity-cash-usd-micros liq) 0)
          " ladder=" (or (:ladder-advance-phase liq) "n/a")
          "/refused=" (boolean (:ladder-advance-refused liq))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused liq))
          " live=" (boolean (:live liq))
          " cash=" (or (:cash-usd-micros liq) 0)
          ".</p>")))
     (when-let [a7s (:report/l0-all-seven-substrate body)]
       (when (and (map? a7s) (not (:error a7s)))
         (str
          "<h2>L0 all-seven substrate (capstone multi-gen + vocation + residual)</h2>"
          "<p class=\"note\">all-inkind/liq/membrane="
          (boolean (:all-inkind-produce-rails-full-chain-refused a7s true)) "/"
          (boolean (:liquidity-receive-full-chain-refused a7s true)) "/"
          (boolean (:all-seven-rails-receive-membrane-refused a7s true))
          " loan/land-grant="
          (boolean (:liquidity-loan-executed a7s)) "/"
          (boolean (:land-grant-executed a7s))
          " ladder=" (or (:ladder-advance-phase a7s) "n/a")
          "/refused=" (boolean (:ladder-advance-refused a7s))
          " held-stress-ladder-refused=" (boolean (:held-stress-ladder-refused a7s))
          " live=" (boolean (:live a7s))
          " cash=" (or (:cash-usd-micros a7s) 0)
          ".</p>")))
     (when-let [cat (:report/l0-priority-path-catalog body)]
       (when (and (map? cat) (not (:error cat)))
         (str
          "<h2>L0 offline priority path catalog (discovery)</h2>"
          "<p class=\"note\">catalog-id=" (or (:catalog-id cat) "fuchi.l0-offline-priority-paths")
          " path-count=" (or (:path-count cat) 0)
          " held-stress-embed-count=" (or (:held-stress-embed-count cat) 0)
          " path-ids=" (str/join "," (map :id (or (:paths cat) [])))
          " loan-never/land-grant-never/held-stress-embed-all="
          (boolean (get-in cat [:invariants :loan-never] true)) "/"
          (boolean (get-in cat [:invariants :land-grant-never] true)) "/"
          (boolean (get-in cat [:invariants :held-stress-embed-all] true))
          " live=" (boolean (:live cat))
          " cash=" (or (:cash-usd-micros cat) 0)
          ".</p>")))
     (when-let [rc (:report/rail-care-design body)]
       (when (and (map? rc) (not (:error rc)))
         (str
          "<h2>rail-care-iyashi DESIGN (priority 3 multi-gen #1)</h2>"
          "<p class=\"note\">rail-kind=" (or (:rail-kind rc) "care-iyashi")
          " care-first-order-rank=" (or (:care-first-order-rank rc) 1)
          " care-first-api=" (or (:care-first-api-path rc) "care-housing-first-path")
          " multi-gen-first=" (boolean (:multi-gen-first rc true))
          " care-delivery-executed=" (boolean (:care-delivery-executed rc))
          " live=" (boolean (:live rc))
          " cash=" (or (:cash-usd-micros rc) 0)
          ".</p>")))
     (when-let [rho (:report/rail-housing-design body)]
       (when (and (map? rho) (not (:error rho)))
         (str
          "<h2>rail-housing-commons DESIGN (priority 3 multi-gen #2)</h2>"
          "<p class=\"note\">rail-kind=" (or (:rail-kind rho) "housing-commons")
          " care-first-before=" (str/join "," (or (:care-first-before-rails rho) ["care"]))
          " care-first-order-rank=" (or (:care-first-order-rank rho) 2)
          " care-first-api=" (or (:care-first-api-path rho) "care-housing-first-path")
          " land-grant-executed=" (boolean (:land-grant-executed rho))
          " live=" (boolean (:live rho))
          " cash=" (or (:cash-usd-micros rho) 0)
          ".</p>")))
     (when-let [rm (:report/rail-mitsuho-design body)]
       (when (and (map? rm) (not (:error rm)))
         (str
          "<h2>rail-mitsuho DESIGN (priority 3 food R1→gated)</h2>"
          "<p class=\"note\">rail-kind=" (or (:rail-kind rm) "food-mitsuho")
          " care-first-before=" (str/join "," (or (:care-first-before-rails rm) []))
          " care-first-api=" (or (:care-first-api-path rm) "care-first-mitsuho-path")
          " live-produce=" (boolean (:live-produce rm))
          " produce-executed=" (boolean (:produce-executed rm))
          " live=" (boolean (:live rm))
          " cash=" (or (:cash-usd-micros rm) 0)
          ".</p>")))
     (when-let [rh (:report/rail-hikari-design body)]
       (when (and (map? rh) (not (:error rh)))
         (str
          "<h2>rail-hikari DESIGN (priority 3 energy R1→gated)</h2>"
          "<p class=\"note\">rail-kind=" (or (:rail-kind rh) "energy-hikari")
          " care-first-before=" (str/join "," (or (:care-first-before-rails rh) []))
          " care-first-api=" (or (:care-first-api-path rh) "care-first-hikari-path")
          " live-produce=" (boolean (:live-produce rh))
          " generate-executed=" (boolean (:generate-executed rh))
          " live=" (boolean (:live rh))
          " cash=" (or (:cash-usd-micros rh) 0)
          ".</p>")))
     (when-let [rt (:report/rail-tooling-design body)]
       (when (and (map? rt) (not (:error rt)))
         (str
          "<h2>rail-tooling-okaimono DESIGN (priority 3 vocation)</h2>"
          "<p class=\"note\">rail-kind=" (or (:rail-kind rt) "tooling-okaimono")
          " care-first-api=" (or (:care-first-api-path rt) "vocation-recovery-path")
          " vocation-recovery=" (boolean (:vocation-recovery rt true))
          " fulfillment-executed=" (boolean (:fulfillment-executed rt))
          " live=" (boolean (:live rt))
          " cash=" (or (:cash-usd-micros rt) 0)
          ".</p>")))
     (when-let [rco (:report/rail-compute-design body)]
       (when (and (map? rco) (not (:error rco)))
         (str
          "<h2>rail-compute-murakumo DESIGN (priority 3 vocation)</h2>"
          "<p class=\"note\">rail-kind=" (or (:rail-kind rco) "compute-murakumo")
          " care-first-api=" (or (:care-first-api-path rco) "vocation-recovery-path")
          " vocation-recovery=" (boolean (:vocation-recovery rco true))
          " quota-executed=" (boolean (:quota-executed rco))
          " live=" (boolean (:live rco))
          " cash=" (or (:cash-usd-micros rco) 0)
          ".</p>")))
     (when-let [rl (:report/rail-liquidity-design body)]
       (when (and (map? rl) (not (:error rl)))
         (str
          "<h2>rail-liquidity-warifu DESIGN (priority 3 residual)</h2>"
          "<p class=\"note\">rail-kind=" (or (:rail-kind rl) "liquidity-warifu")
          " care-first-api=" (or (:care-first-api-path rl) "liquidity-residual-path")
          " residual-rail=" (boolean (:residual-rail rl true))
          " member-principal=" (boolean (:member-principal rl true))
          " loan-executed=" (boolean (:loan-executed rl))
          " live=" (boolean (:live rl))
          " cash=" (or (:cash-usd-micros rl) 0)
          ".</p>")))
     (when-let [rcat (:report/rail-design-catalog body)]
       (when (and (map? rcat) (not (:error rcat)))
         (str
          "<h2>rail DESIGN catalog (all-seven discovery)</h2>"
          "<p class=\"note\">catalog-id=" (or (:catalog-id rcat) "fuchi.rail-design-catalog")
          " rail-count=" (or (:rail-count rcat) 0)
          " ok-count=" (or (:ok-count rcat) 0)
          " rail-kinds=" (str/join "," (or (:rail-kinds rcat) []))
          " order=" (str/join "→" (or (:order rcat) []))
          " live-produce-never=" (boolean (:live-produce-never rcat true))
          " all-cash-zero=" (boolean (:all-cash-zero rcat true))
          " all-live-false=" (boolean (:all-live-false rcat true))
          ".</p>")))
     (when-let [sp (:report/ss-priority-path body)]
       (when (and (map? sp) (not (:error sp)))
         (str
          "<h2>SS priority path (offline)</h2>"
          "<p class=\"note\">(1) L0 stage=" (:l0-stage sp)
          " published=" (boolean (:l0-published sp))
          " token-stub=" (or (:l0-token-stub sp) "—")
          " L0-disclosure=" (or (:l0-disclosure-state sp) "n/a")
          "/held=" (boolean (:l0-disclosure-held sp))
          "/may-flow=" (boolean (:l0-entitlements-may-flow sp true))
          " L0-path=" (or (:l0-path sp) "l0-enroll-offline")
          " ladder=" (or (:ladder-from sp) "L0") "→" (or (:ladder-to sp) "—")
          " rails-hint-first=" (or (:ladder-rails-hint-first sp) "—")
          " ladder-published=" (boolean (:ladder-published sp))
          " stage=" (or (:stage-sustenance-stage sp) "—")
          " stage-rails-first/second="
          (or (:stage-rails-first sp) "—") "/"
          (or (:stage-rails-second sp) "—")
          " stage-r2-all-refused=" (boolean (:stage-r2-all-refused sp))
          " stage-all-gated-refused=" (boolean (:stage-all-gated-refused sp))
          " stage-gated=" (or (:stage-gated-count sp) 0)
          " care/mitsuho/hikari-gated="
          (boolean (:stage-care-gated-admissible sp)) "/"
          (boolean (:stage-mitsuho-gated-admissible sp)) "/"
          (boolean (:stage-hikari-gated-admissible sp))
          " land-grant=" (boolean (:stage-land-grant-executed sp))
          ". (2) disclosure=" (:disclosure-state sp)
          " entitlements-may-flow=" (boolean (:entitlements-may-flow? sp))
          " held-stress-held=" (boolean (:held-stress-held? sp))
          " held-food-r1=" (or (:held-stress-food-phase sp) "—")
          " held-ladder-refused=" (boolean (:held-stress-ladder-refused sp))
          ". (3) care/housing DESIGN live-produce="
          (boolean (:care-live-produce sp)) "/"
          (boolean (:housing-live-produce sp))
          " care-first-api="
          (or (:care-care-first-api-path sp) "care-housing-first-path") "/"
          (or (:housing-care-first-api-path sp) "care-housing-first-path")
          " kinds="
          (or (:care-design-rail-kind sp) "care-iyashi") "/"
          (or (:housing-design-rail-kind sp) "housing-commons")
          " mitsuho/hikari DESIGN live-produce="
          (boolean (:mitsuho-live-produce sp)) "/"
          (boolean (:hikari-live-produce sp))
          " care-first-api="
          (or (:mitsuho-care-first-api-path sp) "care-first-mitsuho-path") "/"
          (or (:hikari-care-first-api-path sp) "care-first-hikari-path")
          " tooling/compute/liq live-produce="
          (boolean (:tooling-live-produce sp)) "/"
          (boolean (:compute-live-produce sp)) "/"
          (boolean (:liquidity-live-produce sp))
          " all-seven-embed="
          (or (:all-seven-design-embed-count sp) 7) "/"
          (boolean (:all-seven-design-live-produce-never sp true))
          " mitsuho/hikari gated="
          (boolean (:mitsuho-gated-admissible sp)) "/"
          (boolean (:hikari-gated-admissible sp))
          " gated-receive="
          (boolean (:mitsuho-gated-receive-admissible sp)) "/"
          (boolean (:hikari-gated-receive-admissible sp))
          " care-recv="
          (boolean (:care-gated-receive-admissible sp))
          " food+energy+care-recv-all-refused="
          (boolean (:care-mitsuho-hikari-receive-all-refused sp))
          " produce-gated="
          (boolean (:mitsuho-gated-produce-admissible sp)) "/"
          (boolean (:hikari-gated-produce-admissible sp))
          " care-produce="
          (boolean (:care-gated-produce-admissible sp))
          " full-chain-refused="
          (boolean (:mitsuho-hikari-full-chain-refused sp))
          " care+food+energy-full-chain="
          (boolean (:care-mitsuho-hikari-full-chain-refused sp))
          " housing-recv/produce="
          (boolean (:housing-gated-receive-admissible sp)) "/"
          (boolean (:housing-gated-produce-admissible sp))
          " housing-full-chain="
          (boolean (:housing-full-chain-refused sp))
          " care+housing+food+energy-full-chain="
          (boolean (:care-housing-mitsuho-hikari-full-chain-refused sp))
          " tooling/compute-recv/produce="
          (boolean (:tooling-gated-receive-admissible sp)) "/"
          (boolean (:tooling-gated-produce-admissible sp)) " · "
          (boolean (:compute-gated-receive-admissible sp)) "/"
          (boolean (:compute-gated-produce-admissible sp))
          " tooling+compute-full-chain="
          (boolean (:tooling-compute-full-chain-refused sp))
          " all-inkind-produce-rails-full-chain="
          (boolean (:all-inkind-produce-rails-full-chain-refused sp))
          " liquidity-recv="
          (boolean (:liquidity-gated-receive-admissible sp))
          " all-seven-rails-receive-membrane="
          (boolean (:all-seven-rails-receive-membrane-refused sp))
          " care/housing/tooling/compute/liquidity gated="
          (boolean (:care-gated-admissible sp)) "/"
          (boolean (:housing-gated-admissible sp)) "/"
          (boolean (:tooling-gated-admissible sp)) "/"
          (boolean (:compute-gated-admissible sp)) "/"
          (boolean (:liquidity-gated-admissible sp))
          " rails-gated=" (or (:rails-gated-count sp) 0)
          " all-rails-gated-refused=" (boolean (:all-rails-gated-refused sp))
          " land-grant=" (boolean (:housing-land-grant-executed sp))
          " R2 statuses/executed="
          (or (:r2-status-count sp) 0) "/"
          (or (:r2-executed-count sp) 0)
          " all-r2-not-executed=" (boolean (:all-r2-not-executed sp true))
          " live=" (boolean (:live sp))
          " cash=" (or (:cash-usd-micros sp) 0)
          ".</p>")))
     "<p class=\"note\">G2: no live displacement without a funded cohort. "
     "Recipient scores are unrepresentable. Live rails default refuse. cash≡0.</p>"
     "</body></html>")))

(defn- out-join
  "Join actor-dir/out/<name> (portable)."
  [name]
  #?(:clj (str (java.io.File. (java.io.File. (edn/actor-dir) "out") name))
     :cljs (let [path (js/require "node:path")]
             (.join path (edn/actor-dir) "out" name))))

(defn- ensure-out-dir!
  []
  (let [d #?(:clj (java.io.File. (edn/actor-dir) "out")
             :cljs (let [path (js/require "node:path")]
                     (.join path (edn/actor-dir) "out")))]
    #?(:clj (.mkdirs d)
       :cljs (let [fs (js/require "node:fs")]
               (when-not (.existsSync fs d)
                 (.mkdirSync fs d #js {:recursive true}))))
    d))

(defn- write-text! [file-path content]
  #?(:clj (spit (str file-path) content)
     :cljs (.writeFileSync (js/require "node:fs") (str file-path) (str content) "utf8")))

(defn write-report!
  "Write out/public-surface.{md,edn,html} from seed. Portable under nbb and bb.
   Never deploys; facts only; cash≡0; no scores."
  ([]
   (let [seed (edn/load-data "seed-sustenance-graph.kotoba.edn")
         _ (ensure-out-dir!)
         md-path (out-join "public-surface.md")
         edn-path (out-join "public-surface.edn")
         html-path (out-join "public-surface.html")]
     (write-text! md-path (report-md seed :include-l0-demo true :include-itonami true))
     (write-text! edn-path (pr-str (report-edn seed :include-l0-demo true :include-itonami true)))
     (write-text! html-path (report-html seed :include-l0-demo true :include-itonami true))
     {:md md-path :edn edn-path :html html-path})))
