(ns fuchi.methods.displacement-scorecard
  "displacement_scorecard.cljc — offline end-to-end scorecard for robotics/itonami SS path.

  Runs itonami seed → L0→L4 enroll/book/G2 → optional L4→L6 tenure → live-gate refuse matrix.
  Emits facts-only MD/EDN. cash≡0. no scores. live=false throughout.
  Portable .cljc; seed load + package build under bb and nbb."
  (:require [clojure.string :as str]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.live-gate :as live-gate]
            [fuchi.methods.displacement-l0-path :as dl0]
            [fuchi.methods.displacement-tenure :as ten]
            [fuchi.methods.displacement-gov :as dgov]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.itonami-surplus-ledger :as led]
            [fuchi.methods.ss-offline-path :as ss-path]
            [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.rail-liquidity-warifu :as liquidity]
            [fuchi.methods.priority-stack :as pstack]
            [fuchi.methods.edn :as edn]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(defn- l0-all-seven-fact-from-enroll
  "Project enroll-with-all-seven result (+ optional continuity/ladder) to facts map."
  [e & {:keys [api note continuity ladder]
        :or {api "enroll-with-all-seven-rails"
             note "L0 all-seven enroll scaffold — default refuse; scorecard smoke"}}]
  (let [out {:path (or (:path e) "l0-enroll-offline")
             :api api
             :l0-stage (or (get-in e [:public-person :stage]) "L0")
             :l0-published (boolean (get-in e [:vow :published]))
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
             :liquidity-member-principal (boolean (:liquidity-member-principal e true))
             :liquidity-loan-executed false
             :liquidity-cash-usd-micros 0
             :land-grant-executed false
             :continuity-final-state (when continuity (:final-state continuity))
             :continuity-held-steps (or (:held-steps continuity) 0)
             :ladder-advance-phase (when ladder (name (:phase ladder)))
             :ladder-advance-refused (boolean (= :refused (:phase ladder)))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note note}]
    (pp/assert-no-public-scores!
     (dissoc out :note :api :path :priority-stack :continuity-final-state
             :ladder-advance-phase))
    out))

(defn l0-all-seven-enroll-fact
  "Priority (1)+(2)+(3) smoke: enroll-with-all-seven-rails facts only.
   Also records continuity stress (open→held→open) + open-path ladder advance ok.
   cash≡0. live=false. loan never. land-grant never. no scores."
  []
  (try
    (let [e (l0/enroll-with-all-seven-rails
             {:subject-did "did:web:etzhayyim.com:member:l0-all-seven-demo"
              :vow-text "L0 all-seven offline scorecard demo — multi-gen + vocation residual"
              :member-signature "sig-l0-all-seven-demo"
              :covenant "outreach"})
          cont (l0/continuity-stress e)
          lad (l0/try-ladder-advance e :member-signature "sig-l0-all-seven-demo")]
      (l0-all-seven-fact-from-enroll e
                                     :continuity cont
                                     :ladder lad
                                     :note "L0 all-seven open path + continuity stress + ladder advance"))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-all-seven-enroll unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-held-all-seven-enroll-fact
  "Priority (2) stress: enroll-with-all-seven with stale disclosure at enroll.
   Hold freezes entitlements; rail R1 packages refuse; ladder advance refused.
   cash≡0. live=false. loan never. no scores."
  []
  (try
    (let [e (l0/enroll-with-all-seven-rails
             {:subject-did "did:web:etzhayyim.com:member:l0-held-all-seven-demo"
              :vow-text "L0 held all-seven offline stress — disclosure stale"
              :member-signature "sig-l0-held-all-seven-demo"
              :covenant "outreach"
              :disclosure l0/STALE-DISC})
          lad (l0/try-ladder-advance e :member-signature "sig-l0-held-all-seven-demo")]
      (l0-all-seven-fact-from-enroll e
                                     :api "enroll-with-all-seven-rails+stale-disclosure"
                                     :ladder lad
                                     :note "L0 held all-seven — R1 refuse via hold; ladder refused"))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-held-all-seven-enroll unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-exit-reaffirm-fact
  "Priority (2) exit SM stress: open enroll → exit-suspend → re-affirm(fresh).
   Ladder refused while exit-suspended; advances after re-affirm when open.
   cash≡0. live=false. no scores."
  []
  (try
    (let [e (l0/enroll
             {:subject-did "did:web:etzhayyim.com:member:l0-exit-reaffirm-demo"
              :vow-text "L0 exit/re-affirm offline stress"
              :member-signature "sig-l0-exit-reaffirm-demo"
              :covenant "outreach"})
          st (l0/exit-reaffirm-stress e :member-signature "sig-l0-exit-reaffirm-demo")
          out (assoc st
                     :api "exit-suspend→re-affirm"
                     :l0-stage "L0"
                     :l0-published false
                     :live false
                     :cash-usd-micros 0
                     :score-surface []
                     :priority-stack PRIORITY-STACK)]
      (pp/assert-no-public-scores! (dissoc out :note :api :path :priority-stack))
      out)
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-exit-reaffirm unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-falsehood-lift-fact
  "Priority (2) falsehood→lift-hold stress. Ladder refused while held; advances after lift.
   cash≡0. live=false. no scores."
  []
  (try
    (let [e (l0/enroll
             {:subject-did "did:web:etzhayyim.com:member:l0-falsehood-lift-demo"
              :vow-text "L0 falsehood/lift-hold offline stress"
              :member-signature "sig-l0-falsehood-lift-demo"
              :covenant "outreach"})
          st (l0/falsehood-lift-stress e :member-signature "sig-l0-falsehood-lift-demo")
          out (assoc st
                     :api "falsehood→lift-hold"
                     :l0-stage "L0"
                     :l0-published false
                     :live false
                     :cash-usd-micros 0
                     :score-surface []
                     :priority-stack PRIORITY-STACK)]
      (pp/assert-no-public-scores! (dissoc out :note :api :path :priority-stack))
      out)
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-falsehood-lift unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn rail-care-design-fact
  "Priority (3) multi-gen first: care-iyashi R1→gated DESIGN (孫/子 substrate #1)."
  []
  (try
    (let [d (care/design-public-facts)]
      (assoc d :path "rail-care-iyashi-design" :api "design-public-facts"
             :live false :cash-usd-micros 0 :score-surface []))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "rail-care design unavailable")
       :live false :cash-usd-micros 0 :score-surface []})))

(defn rail-housing-design-fact
  "Priority (3) multi-gen: housing-commons R1→gated DESIGN after care; land-grant never."
  []
  (try
    (let [d (housing/design-public-facts)]
      (assoc d :path "rail-housing-commons-design" :api "design-public-facts"
             :live false :cash-usd-micros 0 :score-surface []))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "rail-housing design unavailable")
       :live false :cash-usd-micros 0 :score-surface []})))

(defn rail-mitsuho-design-fact
  "Priority (3) single-rail food R1→gated DESIGN facts (care-first after care/housing)."
  []
  (try
    (let [d (mitsuho/design-public-facts)]
      (assoc d :path "rail-mitsuho-design" :api "design-public-facts"
             :live false :cash-usd-micros 0 :score-surface []))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "rail-mitsuho design unavailable")
       :live false :cash-usd-micros 0 :score-surface []})))

(defn rail-hikari-design-fact
  "Priority (3) single-rail energy R1→gated DESIGN facts (care-first after care/housing)."
  []
  (try
    (let [d (hikari/design-public-facts)]
      (assoc d :path "rail-hikari-design" :api "design-public-facts"
             :live false :cash-usd-micros 0 :score-surface []))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "rail-hikari design unavailable")
       :live false :cash-usd-micros 0 :score-surface []})))

(defn rail-tooling-design-fact
  "Priority (3) vocation: tooling-okaimono R1→gated DESIGN after care/housing; fulfillment never."
  []
  (try
    (let [d (tooling/design-public-facts)]
      (assoc d :path "rail-tooling-okaimono-design" :api "design-public-facts"
             :live false :cash-usd-micros 0 :score-surface []))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "rail-tooling design unavailable")
       :live false :cash-usd-micros 0 :score-surface []})))

(defn rail-compute-design-fact
  "Priority (3) vocation: compute-murakumo R1→gated DESIGN after care/housing; quota never."
  []
  (try
    (let [d (compute/design-public-facts)]
      (assoc d :path "rail-compute-murakumo-design" :api "design-public-facts"
             :live false :cash-usd-micros 0 :score-surface []))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "rail-compute design unavailable")
       :live false :cash-usd-micros 0 :score-surface []})))

(defn rail-liquidity-design-fact
  "Priority (3) residual: liquidity-warifu R1→gated DESIGN; member-principal; loan never; cash≡0."
  []
  (try
    (let [d (liquidity/design-public-facts)]
      (assoc d :path "rail-liquidity-warifu-design" :api "design-public-facts"
             :live false :cash-usd-micros 0 :score-surface []))
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "rail-liquidity design unavailable")
       :live false :cash-usd-micros 0 :score-surface []})))

(def RAIL-DESIGN-ORDER
  "Care-first multi-gen order for single-rail DESIGN discovery (priority 3)."
  ["care-iyashi" "housing-commons" "food-mitsuho" "energy-hikari"
   "tooling-okaimono" "compute-murakumo" "liquidity-warifu"])

(defn- rail-design-entry
  "Project a rail-*-design-fact map into a catalog entry (facts only)."
  [order d]
  (let [err (:error d)]
    (cond-> {:id (or (:path d) (str "rail-" order))
             :order order
             :rail-kind (or (:rail-kind d) "n/a")
             :provider-did (or (:provider-did d) "n/a")
             :care-first-api-path (or (:care-first-api-path d) "n/a")
             :care-first-before-rails (or (:care-first-before-rails d) [])
             :live-produce (boolean (:live-produce d))
             :live (boolean (:live d))
             :cash-usd-micros (or (:cash-usd-micros d) 0)
             :score-surface []
             :priority-stack PRIORITY-STACK}
      err (assoc :error err)
      (some? (:care-delivery-executed d))
      (assoc :care-delivery-executed (boolean (:care-delivery-executed d)))
      (some? (:land-grant-executed d))
      (assoc :land-grant-executed (boolean (:land-grant-executed d)))
      (some? (:produce-executed d))
      (assoc :produce-executed (boolean (:produce-executed d)))
      (some? (:generate-executed d))
      (assoc :generate-executed (boolean (:generate-executed d)))
      (some? (:fulfillment-executed d))
      (assoc :fulfillment-executed (boolean (:fulfillment-executed d)))
      (some? (:quota-executed d))
      (assoc :quota-executed (boolean (:quota-executed d)))
      (some? (:loan-executed d))
      (assoc :loan-executed (boolean (:loan-executed d)))
      (some? (:member-principal d))
      (assoc :member-principal (boolean (:member-principal d)))
      (some? (:vocation-recovery d))
      (assoc :vocation-recovery (boolean (:vocation-recovery d)))
      (some? (:residual-rail d))
      (assoc :residual-rail (boolean (:residual-rail d)))
      (some? (:multi-gen-first d))
      (assoc :multi-gen-first (boolean (:multi-gen-first d))))))

(defn rail-design-catalog-fact
  "Discovery catalog of all seven single-rail R1→gated DESIGN facts (priority 3).
   Parity with l0 priority-path-catalog: generators only; cash≡0; live refuse; no scores.
   Optional :rails reuses precomputed rail-*-design-fact maps (scorecard build)."
  [& {:keys [rails] :or {rails nil}}]
  (try
    (let [facts (or rails
                    [(rail-care-design-fact)
                     (rail-housing-design-fact)
                     (rail-mitsuho-design-fact)
                     (rail-hikari-design-fact)
                     (rail-tooling-design-fact)
                     (rail-compute-design-fact)
                     (rail-liquidity-design-fact)])
          rails (mapv rail-design-entry (range 1 (inc (count facts))) facts)
          ok (filterv #(nil? (:error %)) rails)
          n (count rails)
          live-produce-never (every? #(false? (:live-produce %)) ok)
          cash-zero (every? #(zero? (or (:cash-usd-micros %) 0)) ok)
          live-false (every? #(false? (:live %)) ok)
          out {:catalog-id "fuchi.rail-design-catalog"
               :api "rail-design-catalog-fact"
               :path "rail-design-catalog"
               :priority-stack PRIORITY-STACK
               :rail-count n
               :ok-count (count ok)
               :order RAIL-DESIGN-ORDER
               :rails rails
               :rail-ids (mapv :id rails)
               :rail-kinds (mapv :rail-kind rails)
               :invariants {:cash-usd-micros 0
                            :live false
                            :score-surface []
                            :live-produce-never true
                            :loan-never true
                            :land-grant-never true
                            :public-person-facts-only true
                            :all-seven-design true}
               :live-produce-never live-produce-never
               :all-cash-zero cash-zero
               :all-live-false live-false
               :live false
               :cash-usd-micros 0
               :score-surface []
               :note "offline all-seven rail DESIGN catalog — R1→gated only; no live side-effects"}]
      (pp/assert-no-public-scores!
       (dissoc out :note :rails :order :rail-ids :rail-kinds :invariants :priority-stack))
      (doseq [r ok]
        (pp/assert-no-public-scores!
         (dissoc r :care-first-before-rails :care-first-api-path :priority-stack
                 :provider-did :id :rail-kind)))
      out)
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "rail-design-catalog unavailable")
       :catalog-id "fuchi.rail-design-catalog"
       :rail-count 0
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-care-first-mitsuho-fact
  "Priority (1)+(2)+(3 care-first 孫/子 then food): enroll+care+mitsuho+ladder.
   Both full-chains refuse offline; ladder advances when open. cash≡0."
  []
  (try
    (l0/care-first-mitsuho-path
     {:subject-did "did:web:etzhayyim.com:member:l0-care-first-demo"
      :vow-text "L0 care-first + mitsuho offline priority path"
      :member-signature "sig-l0-care-first-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-care-first-mitsuho unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-care-first-hikari-fact
  "Priority (1)+(2)+(3 care-first then energy-hikari). Both refuse offline. cash≡0."
  []
  (try
    (l0/care-first-hikari-path
     {:subject-did "did:web:etzhayyim.com:member:l0-care-first-hikari-demo"
      :vow-text "L0 care-first + hikari offline priority path"
      :member-signature "sig-l0-care-first-hikari-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-care-first-hikari unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-care-first-mitsuho-hikari-fact
  "Priority (1)+(2)+(3 care-first then food-mitsuho + energy-hikari).
   All three full-chains refuse offline; ladder advances when open. cash≡0."
  []
  (try
    (l0/care-first-mitsuho-hikari-path
     {:subject-did "did:web:etzhayyim.com:member:l0-care-first-mitsuho-hikari-demo"
      :vow-text "L0 care-first + mitsuho + hikari offline priority path"
      :member-signature "sig-l0-care-first-mitsuho-hikari-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-care-first-mitsuho-hikari unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-care-housing-first-fact
  "Priority (1)+(2)+(3 care+housing multi-gen substrate 孫/子). land-grant never. cash≡0."
  []
  (try
    (l0/care-housing-first-path
     {:subject-did "did:web:etzhayyim.com:member:l0-care-housing-first-demo"
      :vow-text "L0 care+housing multi-gen substrate offline"
      :member-signature "sig-l0-care-housing-first-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-care-housing-first unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-multi-gen-substrate-fact
  "Priority (1)+(2)+(3 L4 multi-gen): care+housing then mitsuho+hikari + ladder.
   All four refuse offline; land-grant never. cash≡0."
  []
  (try
    (l0/multi-gen-substrate-path
     {:subject-did "did:web:etzhayyim.com:member:l0-multi-gen-substrate-demo"
      :vow-text "L0 multi-gen substrate + mitsuho+hikari offline priority path"
      :member-signature "sig-l0-multi-gen-substrate-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-multi-gen-substrate unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-full-inkind-substrate-fact
  "Priority (1)+(2)+(3 itonami vocation): six in-kind rails + ladder.
   All refuse offline; land-grant/fulfillment/quota never. cash≡0."
  []
  (try
    (l0/full-inkind-substrate-path
     {:subject-did "did:web:etzhayyim.com:member:l0-full-inkind-substrate-demo"
      :vow-text "L0 full in-kind substrate (multi-gen + vocation) offline priority path"
      :member-signature "sig-l0-full-inkind-substrate-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-full-inkind-substrate unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-vocation-recovery-fact
  "Priority (1)+(2)+(3 vocation-only itonami): tooling+compute + ladder.
   Both refuse offline; fulfillment/quota never. cash≡0."
  []
  (try
    (l0/vocation-recovery-path
     {:subject-did "did:web:etzhayyim.com:member:l0-vocation-recovery-demo"
      :vow-text "L0 vocation recovery (tooling+compute) offline for itonami displacement"
      :member-signature "sig-l0-vocation-recovery-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-vocation-recovery unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-liquidity-residual-fact
  "Priority (1)+(2)+(3 residual N4): liquidity warifu member-principal + ladder.
   Receive membrane refuse offline; loan never; cash≡0."
  []
  (try
    (l0/liquidity-residual-path
     {:subject-did "did:web:etzhayyim.com:member:l0-liquidity-residual-demo"
      :vow-text "L0 liquidity residual (warifu member-principal) offline priority path"
      :member-signature "sig-l0-liquidity-residual-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-liquidity-residual unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-all-seven-substrate-fact
  "Priority (1)+(2)+(3 all-seven capstone): six in-kind + liquidity residual + ladder.
   All membranes refuse offline; loan/land-grant/fulfillment/quota never. cash≡0.
   Complements l0-all-seven-enroll-fact (which also records continuity stress)."
  []
  (try
    (l0/all-seven-substrate-path
     {:subject-did "did:web:etzhayyim.com:member:l0-all-seven-substrate-demo"
      :vow-text "L0 all-seven substrate offline priority path — multi-gen + vocation + residual"
      :member-signature "sig-l0-all-seven-substrate-demo"
      :covenant "outreach"})
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "l0-all-seven-substrate unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn l0-priority-path-catalog-fact
  "Discovery: offline priority ladder-path catalog (facts only; does not run paths)."
  []
  (try
    (l0/priority-path-catalog)
    (catch #?(:clj Exception :cljs :default) ex
      {:error (or (ex-message ex) "priority-path-catalog unavailable")
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn live-refuse-matrix
  "All live legs default refuse status (facts only)."
  ([]
   (live-refuse-matrix {}))
  ([env]
   (mapv
    (fn [[leg _]]
      (let [st (live-gate/gate-status (live-gate/make-live-gate {:leg leg}) env)]
        {:leg leg
         :admissible (boolean (get st "admissible"))
         :reason (get st "reason")
         :live false
         :cash-usd-micros 0
         :score-surface []}))
    live-gate/LEG-POLICY)))

(defn- r2-statuses-from-packages
  "Collect R2 execute-membrane status maps from gov subjects (+ subject fallback)."
  [pkgs]
  (vec
   (mapcat
    (fn [p]
      (concat
       (mapcat (fn [g] (vals (or (:r2-by-rail g) {})))
               (concat (or (:gov-subjects p) [])
                       (or (:tenure-gov-subjects p) [])))
       (keep :r2-execute-status
             (concat (or (:subjects p) [])
                     (or (:tenure-subjects p) [])))))
    pkgs)))

(defn ss-priority-path-scorecard-fact
  "Embed ss_offline_path priority (1)(2)(3) demo into scorecard (facts only).
   Full rails gated-live DESIGN refuse + R2 refuse. cash≡0. live=false."
  []
  (try
    (let [path (ss-path/run-food-path
                {:subject-did "did:web:etzhayyim.com:member:ss-scorecard-demo"
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
               :l0-stage (:l0-stage s)
               :l0-published (boolean (:l0-published s))
               :l0-disclosure-state (:l0-disclosure-state s)
               :l0-disclosure-held (boolean (:l0-disclosure-held s))
               :l0-entitlements-may-flow (boolean (:l0-entitlements-may-flow s true))
               :l0-path (:l0-path s)
               :ladder-from (:ladder-from s)
               :ladder-to (:ladder-to s)
               :ladder-target (:ladder-target s)
               :ladder-steps (or (:ladder-steps s) 0)
               :ladder-phase (:ladder-phase s)
               :ladder-rails-hint-first (:ladder-rails-hint-first s)
               :ladder-published false
               :held-stress-ladder-refused (boolean (:held-stress-ladder-refused s))
               :stage-sustenance-stage (:stage-sustenance-stage s)
               :stage-rails-first (:stage-rails-first s)
               :stage-rails-second (:stage-rails-second s)
               :stage-floor-usd-micros-yr (or (:stage-floor-usd-micros-yr s) 0)
               :stage-care-hours-floor-yr (or (:stage-care-hours-floor-yr s) 0)
               :stage-housing-months-floor-yr (or (:stage-housing-months-floor-yr s) 0)
               :stage-land-grant-executed (boolean (:stage-land-grant-executed s))
               :stage-r2-all-refused (boolean (:stage-r2-all-refused s))
               :stage-gated-count (or (:stage-gated-count s) 0)
               :stage-all-gated-refused (boolean (:stage-all-gated-refused s))
               :stage-care-gated-admissible (boolean (:stage-care-gated-admissible s))
               :stage-mitsuho-gated-admissible (boolean (:stage-mitsuho-gated-admissible s))
               :stage-hikari-gated-admissible (boolean (:stage-hikari-gated-admissible s))
               :disclosure-state (:disclosure-state s)
               :entitlements-may-flow? (boolean (:entitlements-may-flow? s))
               :held-stress-held? (boolean (:held-stress-held? s))
               :held-stress-food-phase (:held-stress-food-phase s)
               :rails-gated-count (or (:rails-gated-count s) 0)
               :rails-gated-admissible-count (or (:rails-gated-admissible-count s) 0)
               :all-rails-gated-refused (boolean (:all-rails-gated-refused s))
               :r2-status-count (or (:r2-status-count s) 0)
               :r2-executed-count (or (:r2-executed-count s) 0)
               :all-r2-not-executed (boolean (:all-r2-not-executed s true))
               :mitsuho-gated-admissible (boolean (:mitsuho-gated-admissible s))
               :hikari-gated-admissible (boolean (:hikari-gated-admissible s))
               ;; multi-gen substrate DESIGN first (care → housing), then food/energy
               :care-live-produce (boolean (:care-live-produce s))
               :housing-live-produce (boolean (:housing-live-produce s))
               :care-care-first-api-path
               (or (:care-care-first-api-path s) "care-housing-first-path")
               :housing-care-first-api-path
               (or (:housing-care-first-api-path s) "care-housing-first-path")
               :care-care-first-order-rank
               (or (:care-care-first-order-rank s) 1)
               :housing-care-first-order-rank
               (or (:housing-care-first-order-rank s) 2)
               :care-design-rail-kind
               (or (get-in s [:care-design :rail-kind]) "care-iyashi")
               :housing-design-rail-kind
               (or (get-in s [:housing-design :rail-kind]) "housing-commons")
               :mitsuho-live-produce (boolean (:mitsuho-live-produce s))
               :hikari-live-produce (boolean (:hikari-live-produce s))
               :mitsuho-care-first-api-path
               (or (:mitsuho-care-first-api-path s) "care-first-mitsuho-path")
               :hikari-care-first-api-path
               (or (:hikari-care-first-api-path s) "care-first-hikari-path")
               :mitsuho-care-first-before-rails
               (or (:mitsuho-care-first-before-rails s) ["care" "housing"])
               :hikari-care-first-before-rails
               (or (:hikari-care-first-before-rails s) ["care" "housing"])
               :mitsuho-design-rail-kind
               (or (get-in s [:mitsuho-design :rail-kind]) "food-mitsuho")
               :hikari-design-rail-kind
               (or (get-in s [:hikari-design :rail-kind]) "energy-hikari")
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
               :mitsuho-gated-receive-admissible
               (boolean (:mitsuho-gated-receive-admissible s))
               :hikari-gated-receive-admissible
               (boolean (:hikari-gated-receive-admissible s))
               :mitsuho-hikari-receive-both-refused
               (boolean (:mitsuho-hikari-receive-both-refused s))
               :care-gated-receive-admissible
               (boolean (:care-gated-receive-admissible s))
               :care-mitsuho-hikari-receive-all-refused
               (boolean (:care-mitsuho-hikari-receive-all-refused s))
               :mitsuho-gated-produce-admissible
               (boolean (:mitsuho-gated-produce-admissible s))
               :hikari-gated-produce-admissible
               (boolean (:hikari-gated-produce-admissible s))
               :mitsuho-hikari-produce-both-refused
               (boolean (:mitsuho-hikari-produce-both-refused s))
               :mitsuho-hikari-full-chain-refused
               (boolean (:mitsuho-hikari-full-chain-refused s))
               :care-gated-produce-admissible
               (boolean (:care-gated-produce-admissible s))
               :care-mitsuho-hikari-produce-all-refused
               (boolean (:care-mitsuho-hikari-produce-all-refused s))
               :care-mitsuho-hikari-full-chain-refused
               (boolean (:care-mitsuho-hikari-full-chain-refused s))
               :care-gated-admissible (boolean (:care-gated-admissible s))
               :housing-gated-receive-admissible
               (boolean (:housing-gated-receive-admissible s))
               :housing-gated-produce-admissible
               (boolean (:housing-gated-produce-admissible s))
               :housing-full-chain-refused
               (boolean (:housing-full-chain-refused s))
               :care-housing-mitsuho-hikari-receive-all-refused
               (boolean (:care-housing-mitsuho-hikari-receive-all-refused s))
               :care-housing-mitsuho-hikari-produce-all-refused
               (boolean (:care-housing-mitsuho-hikari-produce-all-refused s))
               :care-housing-mitsuho-hikari-full-chain-refused
               (boolean (:care-housing-mitsuho-hikari-full-chain-refused s))
               :tooling-gated-receive-admissible
               (boolean (:tooling-gated-receive-admissible s))
               :tooling-gated-produce-admissible
               (boolean (:tooling-gated-produce-admissible s))
               :tooling-full-chain-refused
               (boolean (:tooling-full-chain-refused s))
               :compute-gated-receive-admissible
               (boolean (:compute-gated-receive-admissible s))
               :compute-gated-produce-admissible
               (boolean (:compute-gated-produce-admissible s))
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
               :liquidity-gated-receive-admissible
               (boolean (:liquidity-gated-receive-admissible s))
               :liquidity-receive-full-chain-refused
               (boolean (:liquidity-receive-full-chain-refused s))
               :all-seven-rails-receive-membrane-refused
               (boolean (:all-seven-rails-receive-membrane-refused s))
               :housing-land-grant-executed (boolean (:housing-land-grant-executed s))
               :liquidity-loan-executed (boolean (:liquidity-loan-executed s))
               :liquidity-cash-usd-micros 0
               :live false
               :cash-usd-micros 0
               :score-surface []
               :priority-stack PRIORITY-STACK
               :note "ss priority path offline — embedded in displacement scorecard"}]
      (pp/assert-no-public-scores! out)
      out)
    (catch #?(:clj Exception :cljs :default) _
      {:path "ss-offline-inkind-rails"
       :error "ss-priority-path unavailable"
       :all-rails-gated-refused true
       :all-r2-not-executed true
       :rails-gated-count 0
       :r2-status-count 0
       :r2-executed-count 0
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK})))

(defn r2-execute-summary
  "Facts-only R2 membrane aggregate: default refuse, executed always 0 offline."
  [pkgs]
  (let [sts (r2-statuses-from-packages pkgs)
        executed (count (filter #(true? (:executed %)) sts))
        refused (count (filter #(not (true? (:executed %))) sts))
        by-rail (frequencies
                 (mapcat (fn [p]
                           (mapcat #(keys (or (:r2-by-rail %) {}))
                                   (concat (or (:gov-subjects p) [])
                                           (or (:tenure-gov-subjects p) []))))
                         pkgs))
        out {:r2-status-count (count sts)
             :r2-refused refused
             :r2-executed executed
             :r2-by-rail by-rail
             :all-r2-not-executed (zero? executed)
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :note "R2 execute membrane — default refuse; scaffold never side-effects"}]
    (pp/assert-no-public-scores! (dissoc out :r2-by-rail))
    out))

(defn- actor-dir
  []
  (edn/actor-dir))

(defn- load-events
  []
  (try
    (itonami/load-itonami-batch (edn/load-data "itonami-displacement-events.edn"))
    (catch #?(:clj Exception :cljs :default) _ [])))

(defn- ensure-tenure-and-gov
  "Attach L6 tenure (if missing) then G7 package. Public/report paths often pass bare L0 batch."
  [batch]
  (let [needs-tenure? (and (seq (:packages batch))
                           (not (some :tenure-phase (:packages batch))))
        with-ten (if-not needs-tenure?
                   batch
                   (try
                     (let [events (load-events)
                           target (or (:tenure-target batch) "L6")]
                       (if (seq events)
                         (ten/run-batch-with-tenure batch events :target-stage target)
                         batch))
                     (catch #?(:clj Exception :cljs :default) _ batch)))
        with-gov (if (and (seq (:packages with-ten)) (not (:gov-packaged? with-ten)))
                   (dgov/package-batch with-ten)
                   with-ten)]
    with-gov))

(defn build
  "Full offline scorecard. Default: L4 enroll + L6 tenure + G7 gov package on admissible cohorts.
   Bare L0 batches (e.g. from public surface) get tenure + gov attached automatically.
   Portable under bb and nbb."
  ([]
   (let [batch (dl0/run-default-seed :max-slots 2 :climb-steps 4)]
     (build batch)))
  ([batch]
   (let [batch (ensure-tenure-and-gov batch)
         pkgs (or (:packages batch) [])
         enrolled (filter #(= :offline-enrolled (:phase %)) pkgs)
         refused (filter #(#{:refused :refused-over-earmark} (:phase %)) pkgs)
         subjects (mapcat :subjects enrolled)
         tenure-subjects (mapcat :tenure-subjects pkgs)
         tenure-ok (filter #(= :tenure-offline (:tenure-phase %)) pkgs)
         live-legs (live-refuse-matrix)
         ledger (try
                  (let [itonami-seed (edn/load-data "itonami-displacement-events.edn")
                        fuchi (edn/load-data "seed-sustenance-graph.kotoba.edn")]
                    (led/ledger-summary (led/build-ledger itonami-seed fuchi)))
                  (catch #?(:clj Exception :cljs :default) _
                    {:events 0 :live false :cash-to-workers-usd-micros 0}))
         body {:scorecard/id "fuchi.displacement-ss-offline"
               :scorecard/adr ["2607177000" "2606032130" "2606052300"]
               :scorecard/priority-stack PRIORITY-STACK
               :scorecard/live false
               :scorecard/cash-usd-micros 0
               :scorecard/score-surface []
               :scorecard/batch-path (:path batch)
               :scorecard/admissible-cohorts (count enrolled)
               :scorecard/refused-cohorts (count refused)
               :scorecard/enrolled-subjects (count subjects)
               :scorecard/stage-counts (frequencies (map :stage subjects))
               :scorecard/tenure-target (or (:tenure-target batch) "L6")
               :scorecard/tenure-admissible-cohorts
               (or (:tenure-admissible-cohorts batch) (count tenure-ok))
               :scorecard/tenure-subjects
               (let [n (:tenure-subjects batch)]
                 (if (number? n) n (count tenure-subjects)))
               :scorecard/tenure-stage-counts
               (frequencies (map :stage tenure-subjects))
               :scorecard/committed-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:couple :committed-usd-micros-yr]) 0) enrolled))
               :scorecard/headroom-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:couple :headroom-usd-micros-yr]) 0) enrolled))
               :scorecard/committed-post-ratify-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:couple-post-ratify :committed-usd-micros-yr])
                                     (get-in % [:couple :committed-post-ratify-usd-micros-yr])
                                     0)
                                enrolled))
               :scorecard/tenure-committed-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:tenure-couple :committed-usd-micros-yr]) 0)
                                tenure-ok))
               :scorecard/tenure-committed-post-ratify-usd-micros-yr
               (reduce + 0 (map #(or (get-in % [:tenure-couple-post-ratify :committed-usd-micros-yr])
                                     0)
                                tenure-ok))
               :scorecard/booked-entries
               (reduce + 0 (map #(or (get-in % [:booking :entry-count]) 0) subjects))
               :scorecard/tenure-booked-entries
               (reduce + 0 (map #(or (get-in % [:booking :entry-count]) 0) tenure-subjects))
               :scorecard/live-legs live-legs
               :scorecard/all-live-refused (every? #(false? (:admissible %)) live-legs)
               :scorecard/gov-route-counts (or (:gov-route-counts batch) {})
               :scorecard/gov-flowable-committed-usd-micros
               (or (:gov-flowable-committed-usd-micros batch) 0)
               :scorecard/gov-post-ratify-committed-usd-micros
               (or (:gov-post-ratify-committed-usd-micros batch) 0)
               :scorecard/tenure-gov-route-counts (or (:tenure-gov-route-counts batch) {})
               :scorecard/tenure-gov-flowable-committed-usd-micros
               (or (:tenure-gov-flowable-committed-usd-micros batch) 0)
               :scorecard/tenure-gov-post-ratify-committed-usd-micros
               (or (:tenure-gov-post-ratify-committed-usd-micros batch) 0)
               :scorecard/l4-disclosure-open
               (count (filter #(or (true? (:entitlements-may-flow? %))
                                   (and (nil? (:entitlements-may-flow? %))
                                        (not (true? (:disclosure-held? %)))
                                        (not= :held (get-in % [:disclosure-hold :state]))
                                        (not= :exit-suspended (get-in % [:disclosure-hold :state]))))
                              subjects))
               :scorecard/l4-disclosure-held
               (count (filter #(or (true? (:disclosure-held? %))
                                   (= :held (get-in % [:disclosure-hold :state]))
                                   (false? (:entitlements-may-flow? %)))
                              subjects))
               :scorecard/tenure-disclosure-open
               (or (:tenure-disclosure-open batch)
                   (count (filter :entitlements-may-flow? tenure-subjects)))
               :scorecard/tenure-disclosure-held
               (or (:tenure-disclosure-held batch)
                   (count (filter :disclosure-held? tenure-subjects)))
               ;; Priority #3 substrate rails: mitsuho food + hikari energy R1→gated-live
               :scorecard/mitsuho-r1-dry
               (count (filter #(= :R1-dry (get-in % [:food-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/mitsuho-gated-refused
               (count (filter #(and (:food-gated-live-status %)
                                    (false? (get-in % [:food-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/mitsuho-produce-executed
               (count (filter #(true? (get-in % [:food-produce-plan :produce-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/hikari-r1-dry
               (count (filter #(= :R1-dry (get-in % [:energy-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/hikari-gated-refused
               (count (filter #(and (:energy-gated-live-status %)
                                    (false? (get-in % [:energy-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/hikari-generate-executed
               (count (filter #(true? (get-in % [:energy-produce-plan :generate-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/care-r1-dry
               (count (filter #(= :R1-dry (get-in % [:care-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/care-gated-refused
               (count (filter #(and (:care-gated-live-status %)
                                    (false? (get-in % [:care-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/care-delivery-executed
               (count (filter #(true? (get-in % [:care-produce-plan :care-delivery-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/housing-r1-dry
               (count (filter #(= :R1-dry (get-in % [:housing-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/housing-gated-refused
               (count (filter #(and (:housing-gated-live-status %)
                                    (false? (get-in % [:housing-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/housing-land-grant-executed
               (count (filter #(true? (or (get-in % [:housing-gated-live-status :land-grant-executed])
                                          (get-in % [:housing-produce-plan :land-grant-executed])
                                          (:land-grant-executed %)))
                              (concat subjects tenure-subjects)))
               :scorecard/housing-council-held
               (count (filter #(true? (get-in % [:housing-gated-live-status :council-housing-held?]))
                              (concat subjects tenure-subjects)))
               :scorecard/tooling-r1-dry
               (count (filter #(= :R1-dry (get-in % [:tooling-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/tooling-gated-refused
               (count (filter #(and (:tooling-gated-live-status %)
                                    (false? (get-in % [:tooling-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/tooling-fulfillment-executed
               (count (filter #(true? (get-in % [:tooling-produce-plan :fulfillment-executed]))
                              (concat subjects tenure-subjects)))
               :scorecard/compute-r1-dry
               (count (filter #(= :R1-dry (get-in % [:compute-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/compute-gated-refused
               (count (filter #(and (:compute-gated-live-status %)
                                    (false? (get-in % [:compute-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/compute-quota-executed
               (count (filter #(true? (get-in % [:compute-produce-plan :quota-executed]))
                              (concat subjects tenure-subjects)))
               ;; Displacement→L0 subject membranes (gated-receive/produce DESIGN; default refuse)
               :scorecard/displacement-membrane-subjects
               (count (filter :membrane-summary (concat subjects tenure-subjects)))
               ;; L4 path only (do not double-count tenure carries)
               :scorecard/displacement-held-stress-subjects
               (count (filter :held-stress subjects))
               :scorecard/displacement-held-stress-ladder-refused
               (count (filter #(true? (:held-stress-ladder-refused %)) subjects))
               ;; L5/L6 tenure subjects carrying L0 held-stress embed
               :scorecard/tenure-held-stress-subjects
               (count (filter :held-stress tenure-subjects))
               :scorecard/tenure-held-stress-ladder-refused
               (count (filter #(true? (:held-stress-ladder-refused %)) tenure-subjects))
               :scorecard/tenure-held-stress-carried
               (count (filter :l0-held-stress-carried tenure-subjects))
               ;; G7 gov rows (package-subject carry of L0/tenure held-stress)
               :scorecard/gov-held-stress-subjects
               (or (:gov-held-stress-subjects batch)
                   (reduce + 0 (map #(or (:gov-held-stress-subjects %) 0) pkgs)))
               :scorecard/gov-held-stress-ladder-refused
               (or (:gov-held-stress-ladder-refused batch)
                   (reduce + 0 (map #(or (:gov-held-stress-ladder-refused %) 0) pkgs)))
               :scorecard/tenure-gov-held-stress-subjects
               (or (:tenure-gov-held-stress-subjects batch)
                   (reduce + 0 (map #(or (:tenure-gov-held-stress-subjects %) 0) pkgs)))
               :scorecard/tenure-gov-held-stress-ladder-refused
               (or (:tenure-gov-held-stress-ladder-refused batch)
                   (reduce + 0 (map #(or (:tenure-gov-held-stress-ladder-refused %) 0) pkgs)))
               :scorecard/displacement-care-housing-full-chain-refused
               (count (filter #(true? (get-in % [:membrane-summary
                                                 :care-housing-mitsuho-hikari-full-chain-refused]))
                              (concat subjects tenure-subjects)))
               :scorecard/displacement-all-inkind-full-chain-refused
               (count (filter #(true? (get-in % [:membrane-summary
                                                 :all-inkind-produce-rails-full-chain-refused]))
                              (concat subjects tenure-subjects)))
               :scorecard/displacement-all-seven-receive-membrane-refused
               (count (filter #(true? (get-in % [:membrane-summary
                                                 :all-seven-rails-receive-membrane-refused]))
                              (concat subjects tenure-subjects)))
               :scorecard/displacement-liquidity-recv-refused
               (count (filter #(and (:liquidity-gated-receive-status %)
                                    (false? (get-in % [:liquidity-gated-receive-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/displacement-food-recv-refused
               (count (filter #(and (:food-gated-receive-status %)
                                    (false? (get-in % [:food-gated-receive-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/displacement-care-recv-refused
               (count (filter #(and (:care-gated-receive-status %)
                                    (false? (get-in % [:care-gated-receive-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/displacement-housing-recv-refused
               (count (filter #(and (:housing-gated-receive-status %)
                                    (false? (get-in % [:housing-gated-receive-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-r1-dry
               (count (filter #(= :R1-dry (get-in % [:liquidity-package :phase]))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-gated-refused
               (count (filter #(and (:liquidity-gated-live-status %)
                                    (false? (get-in % [:liquidity-gated-live-status :admissible])))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-loan-executed
               (count (filter #(true? (or (get-in % [:liquidity-gated-live-status :loan-executed])
                                          (get-in % [:liquidity-package :loan-executed])))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-member-principal
               (count (filter #(true? (get-in % [:liquidity-package :member-principal]))
                              (concat subjects tenure-subjects)))
               :scorecard/liquidity-cash-usd-micros
               (reduce + 0 (map #(or (get-in % [:liquidity-package :cash-usd-micros]) 0)
                                (concat subjects tenure-subjects)))
               :scorecard/itonami-ledger ledger
               :scorecard/cohorts
               (mapv (fn [p]
                       {:cohort-id (:cohort-id p)
                        :displacing-actor (:displacing-actor p)
                        :phase (name (:phase p))
                        :subjects (count (:subjects p))
                        :committed (or (get-in p [:couple :committed-usd-micros-yr]) 0)
                        :committed-post-ratify
                        (or (get-in p [:couple-post-ratify :committed-usd-micros-yr]) 0)
                        :headroom (or (get-in p [:couple :headroom-usd-micros-yr]) 0)
                        :g2 (boolean (get-in p [:couple :admissible]))
                        :gov-routes (or (:gov-route-counts p) {})
                        :gov-flowable (or (:gov-flowable-committed-usd-micros p) 0)
                        :gov-post-ratify (or (:gov-post-ratify-committed-usd-micros p) 0)
                        :tenure-phase (when (:tenure-phase p) (name (:tenure-phase p)))
                        :tenure-subjects (count (:tenure-subjects p))
                        :tenure-g2 (boolean (get-in p [:tenure-couple :admissible]))
                        :tenure-committed
                        (or (get-in p [:tenure-couple :committed-usd-micros-yr]) 0)
                        :tenure-gov-flowable
                        (or (:tenure-gov-flowable-committed-usd-micros p) 0)
                        :tenure-gov-post-ratify
                        (or (:tenure-gov-post-ratify-committed-usd-micros p) 0)
                        :tenure-disclosure-open (or (:tenure-disclosure-open p) 0)
                        :tenure-disclosure-held (or (:tenure-disclosure-held p) 0)
                        :cash-usd-micros 0
                        :live false
                        :score-surface []})
                     pkgs)}
         r2sum (r2-execute-summary pkgs)
         body (assoc body
                     :scorecard/r2-status-count (:r2-status-count r2sum)
                     :scorecard/r2-refused (:r2-refused r2sum)
                     :scorecard/r2-executed (:r2-executed r2sum)
                     :scorecard/r2-by-rail (:r2-by-rail r2sum)
                     :scorecard/all-r2-not-executed (:all-r2-not-executed r2sum))
         ;; Priority #2: offline all-disclosure-held stress projection (does not mutate open batch)
         stress (try
                  (when-not (= :all-held (:disclosure-stress batch))
                    (let [sb (dgov/package-batch-all-disclosure-held batch)
                          enr (filter #(= :offline-enrolled (:phase %)) (:packages sb))]
                      {:stress "all-disclosure-held"
                       :held-subjects (or (:disclosure-stress-held-subjects sb) 0)
                       :gov-flowable (long (or (:gov-flowable-committed-usd-micros sb) 0))
                       :tenure-gov-flowable
                       (long (or (:tenure-gov-flowable-committed-usd-micros sb) 0))
                       :g2-admissible-cohorts
                       (count (filter #(true? (get-in % [:couple :admissible])) enr))
                       :open-gov-flowable
                       (long (or (:gov-flowable-committed-usd-micros batch) 0))
                       :land-grant-executed 0
                       :r2-executed 0
                       :live false
                       :cash-usd-micros 0
                       :score-surface []
                       :priority-stack PRIORITY-STACK
                       :note "stress only — open path unchanged; live refuse"}))
                  (catch #?(:clj Exception :cljs :default) _ nil))
         pstack-fact (try
                       (pstack/public-facts (pstack/run-offline {}))
                       (catch #?(:clj Exception :cljs :default) ex
                         {:path "priority-stack-offline"
                          :error (or (ex-message ex) "priority-stack unavailable")
                          :ok false
                          :live false
                          :cash-usd-micros 0
                          :score-surface []
                          :priority-stack PRIORITY-STACK}))
         ss-path-fact (ss-priority-path-scorecard-fact)
         l0-seven (l0-all-seven-enroll-fact)
         l0-held (l0-held-all-seven-enroll-fact)
         l0-exit (l0-exit-reaffirm-fact)
         l0-fl (l0-falsehood-lift-fact)
         l0-cf (l0-care-first-mitsuho-fact)
         l0-ch (l0-care-first-hikari-fact)
         l0-cfh (l0-care-first-mitsuho-hikari-fact)
         l0-chs (l0-care-housing-first-fact)
         l0-mgs (l0-multi-gen-substrate-fact)
         l0-fis (l0-full-inkind-substrate-fact)
         l0-voc (l0-vocation-recovery-fact)
         l0-liq (l0-liquidity-residual-fact)
         l0-a7s (l0-all-seven-substrate-fact)
         l0-cat (l0-priority-path-catalog-fact)
         rail-c (rail-care-design-fact)
         rail-ho (rail-housing-design-fact)
         rail-m (rail-mitsuho-design-fact)
         rail-h (rail-hikari-design-fact)
         rail-t (rail-tooling-design-fact)
         rail-co (rail-compute-design-fact)
         rail-l (rail-liquidity-design-fact)
         rail-cat (rail-design-catalog-fact
                   :rails [rail-c rail-ho rail-m rail-h rail-t rail-co rail-l])
         body (cond-> (assoc body
                             :scorecard/priority-stack-offline pstack-fact
                             :scorecard/ss-priority-path ss-path-fact
                             :scorecard/rail-care-design rail-c
                             :scorecard/rail-housing-design rail-ho
                             :scorecard/rail-mitsuho-design rail-m
                             :scorecard/rail-hikari-design rail-h
                             :scorecard/rail-tooling-design rail-t
                             :scorecard/rail-compute-design rail-co
                             :scorecard/rail-liquidity-design rail-l
                             :scorecard/rail-design-catalog rail-cat
                             :scorecard/l0-all-seven-enroll l0-seven
                             :scorecard/l0-held-all-seven-enroll l0-held
                             :scorecard/l0-exit-reaffirm l0-exit
                             :scorecard/l0-falsehood-lift l0-fl
                             :scorecard/l0-care-first-mitsuho l0-cf
                             :scorecard/l0-care-first-hikari l0-ch
                             :scorecard/l0-care-first-mitsuho-hikari l0-cfh
                             :scorecard/l0-care-housing-first l0-chs
                             :scorecard/l0-multi-gen-substrate l0-mgs
                             :scorecard/l0-full-inkind-substrate l0-fis
                             :scorecard/l0-vocation-recovery l0-voc
                             :scorecard/l0-liquidity-residual l0-liq
                             :scorecard/l0-all-seven-substrate l0-a7s
                             :scorecard/l0-priority-path-catalog l0-cat)
                stress (assoc :scorecard/all-held-stress stress))]
     (pp/assert-no-public-scores!
      (dissoc body :scorecard/itonami-ledger :scorecard/cohorts :scorecard/live-legs
              :scorecard/tenure-stage-counts :scorecard/stage-counts
              :scorecard/gov-route-counts :scorecard/tenure-gov-route-counts
              :scorecard/r2-by-rail
              :scorecard/ss-priority-path
              :scorecard/rail-care-design
              :scorecard/rail-housing-design
              :scorecard/rail-mitsuho-design
              :scorecard/rail-hikari-design
              :scorecard/rail-tooling-design
              :scorecard/rail-compute-design
              :scorecard/rail-liquidity-design
              :scorecard/rail-design-catalog
              :scorecard/l0-all-seven-enroll
              :scorecard/l0-held-all-seven-enroll
              :scorecard/l0-exit-reaffirm
              :scorecard/l0-falsehood-lift
              :scorecard/l0-care-first-mitsuho
              :scorecard/l0-care-first-hikari
              :scorecard/l0-care-first-mitsuho-hikari
              :scorecard/l0-care-housing-first
              :scorecard/l0-multi-gen-substrate
              :scorecard/l0-full-inkind-substrate
              :scorecard/l0-vocation-recovery
              :scorecard/l0-liquidity-residual
              :scorecard/l0-all-seven-substrate
              :scorecard/l0-priority-path-catalog
              :scorecard/all-held-stress))
     (when stress (pp/assert-no-public-scores! stress))
     (when-let [sp (:scorecard/ss-priority-path body)]
       (pp/assert-no-public-scores!
        (dissoc sp :error
                :care-care-first-api-path :housing-care-first-api-path
                :housing-care-first-before-rails
                :care-design-rail-kind :housing-design-rail-kind
                :mitsuho-care-first-before-rails
                :hikari-care-first-before-rails
                :mitsuho-care-first-api-path :hikari-care-first-api-path
                :mitsuho-design-rail-kind :hikari-design-rail-kind
                :tooling-care-first-api-path :compute-care-first-api-path
                :liquidity-care-first-api-path
                :tooling-design-rail-kind :compute-design-rail-kind
                :liquidity-design-rail-kind)))
     (when-let [rc (:scorecard/rail-care-design body)]
       (pp/assert-no-public-scores!
        (dissoc rc :error :note :api :path :priority-stack :multi-gen-facts
                :care-first-before-rails :care-first-api-path)))
     (when-let [rho (:scorecard/rail-housing-design body)]
       (pp/assert-no-public-scores!
        (dissoc rho :error :note :api :path :priority-stack :multi-gen-facts
                :care-first-before-rails :care-first-api-path)))
     (when-let [rm (:scorecard/rail-mitsuho-design body)]
       (pp/assert-no-public-scores!
        (dissoc rm :error :note :api :path :priority-stack :multi-gen-facts
                :care-first-before-rails :care-first-api-path)))
     (when-let [rh (:scorecard/rail-hikari-design body)]
       (pp/assert-no-public-scores!
        (dissoc rh :error :note :api :path :priority-stack :multi-gen-facts
                :care-first-before-rails :care-first-api-path)))
     (when-let [rt (:scorecard/rail-tooling-design body)]
       (pp/assert-no-public-scores!
        (dissoc rt :error :note :api :path :priority-stack :multi-gen-facts
                :care-first-before-rails :care-first-api-path)))
     (when-let [rco (:scorecard/rail-compute-design body)]
       (pp/assert-no-public-scores!
        (dissoc rco :error :note :api :path :priority-stack :multi-gen-facts
                :care-first-before-rails :care-first-api-path)))
     (when-let [rl (:scorecard/rail-liquidity-design body)]
       (pp/assert-no-public-scores!
        (dissoc rl :error :note :api :path :priority-stack :multi-gen-facts
                :care-first-before-rails :care-first-api-path)))
     (when-let [rcat (:scorecard/rail-design-catalog body)]
       (pp/assert-no-public-scores!
        (dissoc rcat :error :note :rails :order :rail-ids :rail-kinds :invariants
                :priority-stack :api :path)))
     (when-let [l7 (:scorecard/l0-all-seven-enroll body)]
       (pp/assert-no-public-scores!
        (dissoc l7 :error :note :api :path :priority-stack
                :continuity-final-state :ladder-advance-phase)))
     (when-let [lh (:scorecard/l0-held-all-seven-enroll body)]
       (pp/assert-no-public-scores!
        (dissoc lh :error :note :api :path :priority-stack
                :continuity-final-state :ladder-advance-phase)))
     (when-let [ex (:scorecard/l0-exit-reaffirm body)]
       (pp/assert-no-public-scores!
        (dissoc ex :error :note :api :path :priority-stack
                :exit-ladder-phase :reaffirm-ladder-phase)))
     (when-let [fl (:scorecard/l0-falsehood-lift body)]
       (pp/assert-no-public-scores!
        (dissoc fl :error :note :api :path :priority-stack
                :falsehood-ladder-phase :lift-ladder-phase)))
     (when-let [cf (:scorecard/l0-care-first-mitsuho body)]
       (pp/assert-no-public-scores!
        (dissoc cf :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress
                :care-design :mitsuho-design
                :care-first-before-rails :care-first-api-path))
       (when-let [hs (:held-stress cf)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [ch (:scorecard/l0-care-first-hikari body)]
       (pp/assert-no-public-scores!
        (dissoc ch :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress
                :care-design :hikari-design
                :care-first-before-rails :care-first-api-path))
       (when-let [hs (:held-stress ch)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [cfh (:scorecard/l0-care-first-mitsuho-hikari body)]
       (pp/assert-no-public-scores!
        (dissoc cfh :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress
                :care-design :mitsuho-design :hikari-design
                :care-first-before-rails))
       (when-let [hs (:held-stress cfh)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [chs (:scorecard/l0-care-housing-first body)]
       (pp/assert-no-public-scores!
        (dissoc chs :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress))
       (when-let [hs (:held-stress chs)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [mgs (:scorecard/l0-multi-gen-substrate body)]
       (pp/assert-no-public-scores!
        (dissoc mgs :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress))
       (when-let [hs (:held-stress mgs)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [fis (:scorecard/l0-full-inkind-substrate body)]
       (pp/assert-no-public-scores!
        (dissoc fis :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress))
       (when-let [hs (:held-stress fis)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [voc (:scorecard/l0-vocation-recovery body)]
       (pp/assert-no-public-scores!
        (dissoc voc :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress))
       (when-let [hs (:held-stress voc)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [liq (:scorecard/l0-liquidity-residual body)]
       (pp/assert-no-public-scores!
        (dissoc liq :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress))
       (when-let [hs (:held-stress liq)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [a7s (:scorecard/l0-all-seven-substrate body)]
       (pp/assert-no-public-scores!
        (dissoc a7s :error :note :api :path :priority-stack :multi-gen-facts
                :ladder-advance-phase :held-stress))
       (when-let [hs (:held-stress a7s)]
         (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
     (when-let [cat (:scorecard/l0-priority-path-catalog body)]
       (pp/assert-no-public-scores!
        (dissoc cat :error :note :paths :invariants :priority-stack)))
     (doseq [c (:scorecard/cohorts body)] (pp/assert-no-public-scores! c))
     body)))

(defn scorecard-md
  "Markdown scorecard (facts only)."
  [body]
  (let [lines (transient
               ["# fuchi — displacement SS offline scorecard\n\n"
                (str "Priority: wellbecoming > mago(孫) > ko(子) > present. "
                     "cash≡0. live=false. No personal scores.\n\n")
                "## Summary\n\n"
                (str "- admissible cohorts: " (:scorecard/admissible-cohorts body) "\n")
                (str "- refused cohorts: " (:scorecard/refused-cohorts body) "\n")
                (str "- enrolled subjects (L4 path): " (:scorecard/enrolled-subjects body) "\n")
                (str "- stages (L4 path): " (pr-str (:scorecard/stage-counts body)) "\n")
                (str "- tenure target: " (:scorecard/tenure-target body) "\n")
                (str "- tenure admissible cohorts: " (:scorecard/tenure-admissible-cohorts body) "\n")
                (str "- tenure subjects: " (:scorecard/tenure-subjects body) "\n")
                (str "- tenure stages: " (pr-str (:scorecard/tenure-stage-counts body)) "\n")
                (str "- committed USD micros (L4): " (:scorecard/committed-usd-micros-yr body) "\n")
                (str "- headroom USD micros (L4): " (:scorecard/headroom-usd-micros-yr body) "\n")
                (str "- tenure committed USD micros (flowable-first): "
                     (:scorecard/tenure-committed-usd-micros-yr body) "\n")
                (str "- tenure post-ratify committed: "
                     (or (:scorecard/tenure-committed-post-ratify-usd-micros-yr body) 0) "\n")
                (str "- booked ledger entries (L4): " (:scorecard/booked-entries body) "\n")
                (str "- tenure booked entries: " (:scorecard/tenure-booked-entries body) "\n")
                (str "- all live legs refused: " (:scorecard/all-live-refused body) "\n")
                (str "- gov routes (L4): " (pr-str (:scorecard/gov-route-counts body)) "\n")
                (str "- gov flowable committed L4 (housing held): "
                     (:scorecard/gov-flowable-committed-usd-micros body) "\n")
                (str "- gov post-ratify committed L4 (grant false): "
                     (:scorecard/gov-post-ratify-committed-usd-micros body) "\n")
                (str "- couple post-ratify committed L4: "
                     (or (:scorecard/committed-post-ratify-usd-micros-yr body) 0) "\n")
                (str "- tenure gov routes: "
                     (pr-str (or (:scorecard/tenure-gov-route-counts body) {})) "\n")
                (str "- tenure gov flowable (housing held): "
                     (or (:scorecard/tenure-gov-flowable-committed-usd-micros body) 0) "\n")
                (str "- tenure gov post-ratify (grant false): "
                     (or (:scorecard/tenure-gov-post-ratify-committed-usd-micros body) 0) "\n")
                (str "- L4 disclosure open/held: "
                     (or (:scorecard/l4-disclosure-open body) 0) "/"
                     (or (:scorecard/l4-disclosure-held body) 0) "\n")
                (str "- tenure disclosure open/held: "
                     (or (:scorecard/tenure-disclosure-open body) 0) "/"
                     (or (:scorecard/tenure-disclosure-held body) 0) "\n")
                (str "- mitsuho food R1-dry / gated-refused / produce-executed: "
                     (or (:scorecard/mitsuho-r1-dry body) 0) "/"
                     (or (:scorecard/mitsuho-gated-refused body) 0) "/"
                     (or (:scorecard/mitsuho-produce-executed body) 0) "\n")
                (str "- hikari energy R1-dry / gated-refused / generate-executed: "
                     (or (:scorecard/hikari-r1-dry body) 0) "/"
                     (or (:scorecard/hikari-gated-refused body) 0) "/"
                     (or (:scorecard/hikari-generate-executed body) 0) "\n")
                (str "- care-iyashi R1-dry / gated-refused / care-delivery-executed: "
                     (or (:scorecard/care-r1-dry body) 0) "/"
                     (or (:scorecard/care-gated-refused body) 0) "/"
                     (or (:scorecard/care-delivery-executed body) 0) "\n")
                (str "- displacement L0 membranes subjects / care+housing full-chain-refused / all-inkind full-chain-refused / all-seven receive-membrane: "
                     (or (:scorecard/displacement-membrane-subjects body) 0) "/"
                     (or (:scorecard/displacement-care-housing-full-chain-refused body) 0) "/"
                     (or (:scorecard/displacement-all-inkind-full-chain-refused body) 0) "/"
                     (or (:scorecard/displacement-all-seven-receive-membrane-refused body) 0) "\n")
                (str "- displacement L0 held-stress subjects / ladder-refused: "
                     (or (:scorecard/displacement-held-stress-subjects body) 0) "/"
                     (or (:scorecard/displacement-held-stress-ladder-refused body) 0) "\n")
                (str "- tenure held-stress subjects / ladder-refused / carried-from-L0: "
                     (or (:scorecard/tenure-held-stress-subjects body) 0) "/"
                     (or (:scorecard/tenure-held-stress-ladder-refused body) 0) "/"
                     (or (:scorecard/tenure-held-stress-carried body) 0) "\n")
                (str "- gov held-stress subjects / ladder-refused (L4 rows): "
                     (or (:scorecard/gov-held-stress-subjects body) 0) "/"
                     (or (:scorecard/gov-held-stress-ladder-refused body) 0) "\n")
                (str "- tenure-gov held-stress subjects / ladder-refused: "
                     (or (:scorecard/tenure-gov-held-stress-subjects body) 0) "/"
                     (or (:scorecard/tenure-gov-held-stress-ladder-refused body) 0) "\n")
                (str "- displacement L0 liquidity residual receive-refused (member-principal): "
                     (or (:scorecard/displacement-liquidity-recv-refused body) 0) "\n")
                (str "- displacement L0 food/care/housing gated-recv-refused: "
                     (or (:scorecard/displacement-food-recv-refused body) 0) "/"
                     (or (:scorecard/displacement-care-recv-refused body) 0) "/"
                     (or (:scorecard/displacement-housing-recv-refused body) 0) "\n")
                (str "- housing-commons R1-dry / gated-refused / land-grant-executed: "
                     (or (:scorecard/housing-r1-dry body) 0) "/"
                     (or (:scorecard/housing-gated-refused body) 0) "/"
                     (or (:scorecard/housing-land-grant-executed body) 0) "\n")
                (str "- housing council-held (awaiting Lv7): "
                     (or (:scorecard/housing-council-held body) 0) "\n")
                (str "- tooling-okaimono R1-dry / gated-refused / fulfillment-executed: "
                     (or (:scorecard/tooling-r1-dry body) 0) "/"
                     (or (:scorecard/tooling-gated-refused body) 0) "/"
                     (or (:scorecard/tooling-fulfillment-executed body) 0) "\n")
                (str "- compute-murakumo R1-dry / gated-refused / quota-executed: "
                     (or (:scorecard/compute-r1-dry body) 0) "/"
                     (or (:scorecard/compute-gated-refused body) 0) "/"
                     (or (:scorecard/compute-quota-executed body) 0) "\n")
                (str "- liquidity-warifu R1-dry / gated-refused / loan-executed: "
                     (or (:scorecard/liquidity-r1-dry body) 0) "/"
                     (or (:scorecard/liquidity-gated-refused body) 0) "/"
                     (or (:scorecard/liquidity-loan-executed body) 0) "\n")
                (str "- liquidity member-principal / cash-usd-micros: "
                     (or (:scorecard/liquidity-member-principal body) 0) "/"
                     (or (:scorecard/liquidity-cash-usd-micros body) 0) "\n")
                (str "- R2 execute membrane statuses / refused / executed: "
                     (or (:scorecard/r2-status-count body) 0) "/"
                     (or (:scorecard/r2-refused body) 0) "/"
                     (or (:scorecard/r2-executed body) 0) "\n")
                (str "- all-r2-not-executed: "
                     (boolean (:scorecard/all-r2-not-executed body true)) "\n")
                (str "- r2-by-rail: " (pr-str (or (:scorecard/r2-by-rail body) {})) "\n")])]
    (when-let [l7 (:scorecard/l0-all-seven-enroll body)]
      (when (and (map? l7) (not (:error l7)))
        (conj! lines "\n## L0 enroll all-seven rails (priority 1+2+3 smoke)\n\n")
        (conj! lines (str "- api: " (or (:api l7) "enroll-with-all-seven-rails") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state l7) "n/a") "/"
                         (boolean (:disclosure-held l7)) "/"
                         (boolean (:entitlements-may-flow l7 true)) "\n"))
        (conj! lines (str "- care/housing/mitsuho/hikari full-chain-refused: "
                         (boolean (:care-full-chain-refused l7)) "/"
                         (boolean (:housing-full-chain-refused l7)) "/"
                         (boolean (:mitsuho-full-chain-refused l7)) "/"
                         (boolean (:hikari-full-chain-refused l7)) "\n"))
        (conj! lines (str "- tooling/compute full-chain-refused: "
                         (boolean (:tooling-full-chain-refused l7)) "/"
                         (boolean (:compute-full-chain-refused l7)) "\n"))
        (conj! lines (str "- all-inkind / liquidity-receive / all-seven-membrane: "
                         (boolean (:all-inkind-produce-rails-full-chain-refused l7)) "/"
                         (boolean (:liquidity-receive-full-chain-refused l7)) "/"
                         (boolean (:all-seven-rails-receive-membrane-refused l7)) "\n"))
        (conj! lines (str "- liquidity member-principal / loan-executed / cash-usd-micros: "
                         (boolean (:liquidity-member-principal l7 true)) "/"
                         (boolean (:liquidity-loan-executed l7)) "/"
                         (or (:liquidity-cash-usd-micros l7) 0) "\n"))
        (conj! lines (str "- land-grant-executed / live / cash: "
                         (boolean (:land-grant-executed l7)) "/"
                         (boolean (:live l7)) "/"
                         (or (:cash-usd-micros l7) 0) "\n"))
        (conj! lines (str "- continuity stress final/held-steps: "
                         (or (:continuity-final-state l7) "n/a") "/"
                         (or (:continuity-held-steps l7) 0) "\n"))
        (conj! lines (str "- ladder advance phase/refused: "
                         (or (:ladder-advance-phase l7) "n/a") "/"
                         (boolean (:ladder-advance-refused l7)) "\n"))))
    (when-let [lh (:scorecard/l0-held-all-seven-enroll body)]
      (when (and (map? lh) (not (:error lh)))
        (conj! lines "\n## L0 held all-seven (disclosure stale stress)\n\n")
        (conj! lines (str "- api: " (or (:api lh) "enroll-with-all-seven+stale") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state lh) "n/a") "/"
                         (boolean (:disclosure-held lh)) "/"
                         (boolean (:entitlements-may-flow lh)) "\n"))
        (conj! lines (str "- all-inkind / liq-recv / all-seven-membrane: "
                         (boolean (:all-inkind-produce-rails-full-chain-refused lh)) "/"
                         (boolean (:liquidity-receive-full-chain-refused lh)) "/"
                         (boolean (:all-seven-rails-receive-membrane-refused lh)) "\n"))
        (conj! lines (str "- ladder advance phase/refused: "
                         (or (:ladder-advance-phase lh) "n/a") "/"
                         (boolean (:ladder-advance-refused lh true)) "\n"))
        (conj! lines (str "- loan / land-grant / live / cash: "
                         (boolean (:liquidity-loan-executed lh)) "/"
                         (boolean (:land-grant-executed lh)) "/"
                         (boolean (:live lh)) "/"
                         (or (:cash-usd-micros lh) 0) "\n"))))
    (when-let [ex (:scorecard/l0-exit-reaffirm body)]
      (when (and (map? ex) (not (:error ex)))
        (conj! lines "\n## L0 exit→re-affirm stress (disclosure SM)\n\n")
        (conj! lines (str "- api: " (or (:api ex) "exit-suspend→re-affirm") "\n"))
        (conj! lines (str "- exit state/suspended/may-flow: "
                         (or (:exit-state ex) "n/a") "/"
                         (boolean (:exit-suspended? ex)) "/"
                         (boolean (:exit-entitlements-may-flow ex)) "\n"))
        (conj! lines (str "- exit ladder phase/refused: "
                         (or (:exit-ladder-phase ex) "n/a") "/"
                         (boolean (:exit-ladder-refused ex true)) "\n"))
        (conj! lines (str "- re-affirm state/suspended/may-flow: "
                         (or (:reaffirm-state ex) "n/a") "/"
                         (boolean (:reaffirm-exit-suspended? ex)) "/"
                         (boolean (:reaffirm-entitlements-may-flow ex true)) "\n"))
        (conj! lines (str "- re-affirm ladder phase/refused: "
                         (or (:reaffirm-ladder-phase ex) "n/a") "/"
                         (boolean (:reaffirm-ladder-refused ex)) "\n"))
        (conj! lines (str "- live / cash-usd-micros: "
                         (boolean (:live ex)) "/"
                         (or (:cash-usd-micros ex) 0) "\n"))))
    (when-let [fl (:scorecard/l0-falsehood-lift body)]
      (when (and (map? fl) (not (:error fl)))
        (conj! lines "\n## L0 falsehood→lift-hold stress\n\n")
        (conj! lines (str "- api: " (or (:api fl) "falsehood→lift-hold") "\n"))
        (conj! lines (str "- falsehood held/may-flow/ladder-refused: "
                         (boolean (:falsehood-held? fl true)) "/"
                         (boolean (:falsehood-entitlements-may-flow fl)) "/"
                         (boolean (:falsehood-ladder-refused fl true)) "\n"))
        (conj! lines (str "- lift state/may-flow/ladder: "
                         (or (:lift-state fl) "n/a") "/"
                         (boolean (:lift-entitlements-may-flow fl true)) "/"
                         (or (:lift-ladder-phase fl) "n/a")
                         "/refused=" (boolean (:lift-ladder-refused fl)) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live fl)) "/"
                         (or (:cash-usd-micros fl) 0) "\n"))))
    (when-let [cf (:scorecard/l0-care-first-mitsuho body)]
      (when (and (map? cf) (not (:error cf)))
        (conj! lines "\n## L0 care-first + mitsuho (priority 1+2+3 孫/子)\n\n")
        (conj! lines (str "- api: " (or (:api cf) "enroll+care+mitsuho+ladder") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state cf) "n/a") "/"
                         (boolean (:disclosure-held cf)) "/"
                         (boolean (:entitlements-may-flow cf true)) "\n"))
        (conj! lines (str "- care/mitsuho full-chain/both-refused: "
                         (boolean (:care-full-chain-refused cf)) "/"
                         (boolean (:mitsuho-full-chain-refused cf)) "/"
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
        (conj! lines (str "- ladder phase/refused: "
                         (or (:ladder-advance-phase cf) "n/a") "/"
                         (boolean (:ladder-advance-refused cf)) "\n"))
        (when-let [hs (:held-stress cf)]
          (conj! lines (str "- held-stress held/both-refused/ladder-refused: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-mitsuho-both-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live cf)) "/"
                         (or (:cash-usd-micros cf) 0) "\n"))))
    (when-let [ch (:scorecard/l0-care-first-hikari body)]
      (when (and (map? ch) (not (:error ch)))
        (conj! lines "\n## L0 care-first + hikari (priority 1+2+3 孫/子 + energy)\n\n")
        (conj! lines (str "- api: " (or (:api ch) "enroll+care+hikari+ladder") "\n"))
        (conj! lines (str "- care/hikari full-chain/both-refused: "
                         (boolean (:care-full-chain-refused ch)) "/"
                         (boolean (:hikari-full-chain-refused ch)) "/"
                         (boolean (:care-hikari-both-refused ch true)) "\n"))
        (conj! lines (str "- care-first-api-path / before-rails: "
                         (or (:care-first-api-path ch) "care-first-hikari-path") " / "
                         (str/join "," (or (:care-first-before-rails ch) ["care" "housing"])) "\n"))
        (conj! lines (str "- hikari-design rail-kind / live-produce / generate-executed: "
                         (or (get-in ch [:hikari-design :rail-kind]) "energy-hikari") "/"
                         (boolean (:hikari-live-produce ch)) "/"
                         (boolean (:hikari-generate-executed ch)) "\n"))
        (conj! lines (str "- ladder phase/refused: "
                         (or (:ladder-advance-phase ch) "n/a") "/"
                         (boolean (:ladder-advance-refused ch)) "\n"))
        (when-let [hs (:held-stress ch)]
          (conj! lines (str "- held-stress held/both-refused/ladder-refused: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-hikari-both-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live ch)) "/"
                         (or (:cash-usd-micros ch) 0) "\n"))))
    (when-let [cfh (:scorecard/l0-care-first-mitsuho-hikari body)]
      (when (and (map? cfh) (not (:error cfh)))
        (conj! lines "\n## L0 care-first + mitsuho + hikari (priority 1+2+3 dual rail)\n\n")
        (conj! lines (str "- api: " (or (:api cfh) "enroll+care+mitsuho+hikari+ladder") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state cfh) "n/a") "/"
                         (boolean (:disclosure-held cfh)) "/"
                         (boolean (:entitlements-may-flow cfh true)) "\n"))
        (conj! lines (str "- care/mitsuho/hikari full-chain / all-refused: "
                         (boolean (:care-full-chain-refused cfh)) "/"
                         (boolean (:mitsuho-full-chain-refused cfh)) "/"
                         (boolean (:hikari-full-chain-refused cfh)) "/"
                         (boolean (:care-mitsuho-hikari-all-refused cfh true)) "\n"))
        (conj! lines (str "- mitsuho+hikari both-refused: "
                         (boolean (:mitsuho-hikari-both-refused cfh true)) "\n"))
        (conj! lines (str "- mitsuho/hikari design live-produce: "
                         (boolean (:mitsuho-live-produce cfh)) "/"
                         (boolean (:hikari-live-produce cfh)) "\n"))
        (conj! lines (str "- ladder phase/refused: "
                         (or (:ladder-advance-phase cfh) "n/a") "/"
                         (boolean (:ladder-advance-refused cfh)) "\n"))
        (when-let [hs (:held-stress cfh)]
          (conj! lines (str "- held-stress held/all-refused/ladder-refused: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-mitsuho-hikari-all-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live cfh)) "/"
                         (or (:cash-usd-micros cfh) 0) "\n"))))
    (when-let [chs (:scorecard/l0-care-housing-first body)]
      (when (and (map? chs) (not (:error chs)))
        (conj! lines "\n## L0 care+housing multi-gen substrate (孫/子)\n\n")
        (conj! lines (str "- api: " (or (:api chs) "enroll+care+housing+ladder") "\n"))
        (conj! lines (str "- care/housing full-chain/both-refused: "
                         (boolean (:care-full-chain-refused chs)) "/"
                         (boolean (:housing-full-chain-refused chs)) "/"
                         (boolean (:care-housing-both-refused chs true)) "\n"))
        (conj! lines (str "- land-grant-executed / ladder phase/refused: "
                         (boolean (:land-grant-executed chs)) "/"
                         (or (:ladder-advance-phase chs) "n/a") "/"
                         (boolean (:ladder-advance-refused chs)) "\n"))
        (when-let [hs (:held-stress chs)]
          (conj! lines (str "- held-stress held/both-refused/ladder-refused/land-grant: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-housing-both-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "/"
                           (boolean (:land-grant-executed hs)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live chs)) "/"
                         (or (:cash-usd-micros chs) 0) "\n"))))
    (when-let [mgs (:scorecard/l0-multi-gen-substrate body)]
      (when (and (map? mgs) (not (:error mgs)))
        (conj! lines "\n## L0 multi-gen substrate + mitsuho+hikari (L4 priority)\n\n")
        (conj! lines (str "- api: " (or (:api mgs) "enroll+care+housing+mitsuho+hikari+ladder") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state mgs) "n/a") "/"
                         (boolean (:disclosure-held mgs)) "/"
                         (boolean (:entitlements-may-flow mgs true)) "\n"))
        (conj! lines (str "- care/housing/mitsuho/hikari full-chain / all-refused: "
                         (boolean (:care-full-chain-refused mgs)) "/"
                         (boolean (:housing-full-chain-refused mgs)) "/"
                         (boolean (:mitsuho-full-chain-refused mgs)) "/"
                         (boolean (:hikari-full-chain-refused mgs)) "/"
                         (boolean (:care-housing-mitsuho-hikari-all-refused mgs true)) "\n"))
        (conj! lines (str "- care+housing both / mitsuho+hikari both: "
                         (boolean (:care-housing-both-refused mgs true)) "/"
                         (boolean (:mitsuho-hikari-both-refused mgs true)) "\n"))
        (conj! lines (str "- land-grant-executed / ladder phase/refused: "
                         (boolean (:land-grant-executed mgs)) "/"
                         (or (:ladder-advance-phase mgs) "n/a") "/"
                         (boolean (:ladder-advance-refused mgs)) "\n"))
        (when-let [hs (:held-stress mgs)]
          (conj! lines (str "- held-stress held/all-refused/ladder-refused/land-grant: "
                           (boolean (:disclosure-held hs true)) "/"
                           (boolean (:care-housing-mitsuho-hikari-all-refused hs true)) "/"
                           (boolean (:ladder-advance-refused hs true)) "/"
                           (boolean (:land-grant-executed hs)) "\n")))
        (conj! lines (str "- live / cash: "
                         (boolean (:live mgs)) "/"
                         (or (:cash-usd-micros mgs) 0) "\n"))))
    (when-let [fis (:scorecard/l0-full-inkind-substrate body)]
      (when (and (map? fis) (not (:error fis)))
        (conj! lines "\n## L0 full in-kind substrate (multi-gen + vocation / itonami)\n\n")
        (conj! lines (str "- api: " (or (:api fis) "enroll+six-inkind+ladder") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state fis) "n/a") "/"
                         (boolean (:disclosure-held fis)) "/"
                         (boolean (:entitlements-may-flow fis true)) "\n"))
        (conj! lines (str "- six in-kind all-refused: "
                         (boolean (:all-inkind-produce-rails-full-chain-refused fis true)) "\n"))
        (conj! lines (str "- care+housing / mitsuho+hikari / tooling+compute both-refused: "
                         (boolean (:care-housing-both-refused fis true)) "/"
                         (boolean (:mitsuho-hikari-both-refused fis true)) "/"
                         (boolean (:tooling-compute-both-refused fis true)) "\n"))
        (conj! lines (str "- land-grant / fulfillment / quota executed: "
                         (boolean (:land-grant-executed fis)) "/"
                         (boolean (:fulfillment-executed fis)) "/"
                         (boolean (:quota-executed fis)) "\n"))
        (conj! lines (str "- ladder phase/refused: "
                         (or (:ladder-advance-phase fis) "n/a") "/"
                         (boolean (:ladder-advance-refused fis)) "\n"))
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
    (when-let [voc (:scorecard/l0-vocation-recovery body)]
      (when (and (map? voc) (not (:error voc)))
        (conj! lines "\n## L0 vocation recovery (tooling+compute / itonami job-loss)\n\n")
        (conj! lines (str "- api: " (or (:api voc) "enroll+tooling+compute+ladder") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state voc) "n/a") "/"
                         (boolean (:disclosure-held voc)) "/"
                         (boolean (:entitlements-may-flow voc true)) "\n"))
        (conj! lines (str "- tooling/compute full-chain / both-refused: "
                         (boolean (:tooling-full-chain-refused voc)) "/"
                         (boolean (:compute-full-chain-refused voc)) "/"
                         (boolean (:tooling-compute-both-refused voc true)) "\n"))
        (conj! lines (str "- fulfillment / quota executed: "
                         (boolean (:fulfillment-executed voc)) "/"
                         (boolean (:quota-executed voc)) "\n"))
        (conj! lines (str "- ladder phase/refused: "
                         (or (:ladder-advance-phase voc) "n/a") "/"
                         (boolean (:ladder-advance-refused voc)) "\n"))
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
    (when-let [liq (:scorecard/l0-liquidity-residual body)]
      (when (and (map? liq) (not (:error liq)))
        (conj! lines "\n## L0 liquidity residual (warifu member-principal)\n\n")
        (conj! lines (str "- api: " (or (:api liq) "enroll+liquidity+ladder") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state liq) "n/a") "/"
                         (boolean (:disclosure-held liq)) "/"
                         (boolean (:entitlements-may-flow liq true)) "\n"))
        (conj! lines (str "- liquidity receive full-chain-refused: "
                         (boolean (:liquidity-receive-full-chain-refused liq true)) "\n"))
        (conj! lines (str "- member-principal / loan-executed / cash-usd-micros: "
                         (boolean (:liquidity-member-principal liq true)) "/"
                         (boolean (:liquidity-loan-executed liq)) "/"
                         (or (:liquidity-cash-usd-micros liq) 0) "\n"))
        (conj! lines (str "- ladder phase/refused: "
                         (or (:ladder-advance-phase liq) "n/a") "/"
                         (boolean (:ladder-advance-refused liq)) "\n"))
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
    (when-let [a7s (:scorecard/l0-all-seven-substrate body)]
      (when (and (map? a7s) (not (:error a7s)))
        (conj! lines "\n## L0 all-seven substrate (capstone multi-gen + vocation + residual)\n\n")
        (conj! lines (str "- api: " (or (:api a7s) "enroll-with-all-seven-rails+ladder") "\n"))
        (conj! lines (str "- disclosure open/held/may-flow: "
                         (or (:disclosure-state a7s) "n/a") "/"
                         (boolean (:disclosure-held a7s)) "/"
                         (boolean (:entitlements-may-flow a7s true)) "\n"))
        (conj! lines (str "- all-inkind / liq-recv / all-seven-membrane: "
                         (boolean (:all-inkind-produce-rails-full-chain-refused a7s true)) "/"
                         (boolean (:liquidity-receive-full-chain-refused a7s true)) "/"
                         (boolean (:all-seven-rails-receive-membrane-refused a7s true)) "\n"))
        (conj! lines (str "- member-principal / loan / land-grant / fulfillment / quota: "
                         (boolean (:liquidity-member-principal a7s true)) "/"
                         (boolean (:liquidity-loan-executed a7s)) "/"
                         (boolean (:land-grant-executed a7s)) "/"
                         (boolean (:fulfillment-executed a7s)) "/"
                         (boolean (:quota-executed a7s)) "\n"))
        (conj! lines (str "- ladder phase/refused: "
                         (or (:ladder-advance-phase a7s) "n/a") "/"
                         (boolean (:ladder-advance-refused a7s)) "\n"))
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
    (when-let [cat (:scorecard/l0-priority-path-catalog body)]
      (when (and (map? cat) (not (:error cat)))
        (conj! lines "\n## L0 offline priority path catalog (discovery)\n\n")
        (conj! lines (str "- catalog-id: " (or (:catalog-id cat) "fuchi.l0-offline-priority-paths") "\n"))
        (conj! lines (str "- path-count: " (or (:path-count cat) 0) "\n"))
        (conj! lines (str "- held-stress-embed-count: "
                         (or (:held-stress-embed-count cat) 0) "\n"))
        (conj! lines (str "- path-ids: "
                         (str/join "," (map :id (or (:paths cat) []))) "\n"))
        (conj! lines (str "- invariants loan-never/land-grant-never/held-stress-embed-all/cash: "
                         (boolean (get-in cat [:invariants :loan-never] true)) "/"
                         (boolean (get-in cat [:invariants :land-grant-never] true)) "/"
                         (boolean (get-in cat [:invariants :held-stress-embed-all] true)) "/"
                         (or (:cash-usd-micros cat) 0) "\n"))
        (conj! lines (str "- live: " (boolean (:live cat)) "\n"))))
    (when-let [rc (:scorecard/rail-care-design body)]
      (when (and (map? rc) (not (:error rc)))
        (conj! lines "\n## rail-care-iyashi DESIGN (priority 3 multi-gen #1)\n\n")
        (conj! lines (str "- rail-kind / provider: "
                         (or (:rail-kind rc) "care-iyashi") " / "
                         (or (:provider-did rc) "—") "\n"))
        (conj! lines (str "- care-first-order-rank / api-path: "
                         (or (:care-first-order-rank rc) 1) " / "
                         (or (:care-first-api-path rc) "care-housing-first-path") "\n"))
        (conj! lines (str "- multi-gen-first / care-delivery-executed: "
                         (boolean (:multi-gen-first rc true)) "/"
                         (boolean (:care-delivery-executed rc)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rc) [])) "\n"))
        (conj! lines (str "- live / cash / score-surface: "
                         (boolean (:live rc)) "/"
                         (or (:cash-usd-micros rc) 0) "/"
                         (pr-str (or (:score-surface rc) [])) "\n"))))
    (when-let [rho (:scorecard/rail-housing-design body)]
      (when (and (map? rho) (not (:error rho)))
        (conj! lines "\n## rail-housing-commons DESIGN (priority 3 multi-gen #2)\n\n")
        (conj! lines (str "- rail-kind / provider: "
                         (or (:rail-kind rho) "housing-commons") " / "
                         (or (:provider-did rho) "—") "\n"))
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
        (conj! lines (str "- live / cash / score-surface: "
                         (boolean (:live rho)) "/"
                         (or (:cash-usd-micros rho) 0) "/"
                         (pr-str (or (:score-surface rho) [])) "\n"))))
    (when-let [rm (:scorecard/rail-mitsuho-design body)]
      (when (and (map? rm) (not (:error rm)))
        (conj! lines "\n## rail-mitsuho DESIGN (priority 3 food R1→gated)\n\n")
        (conj! lines (str "- rail-kind / provider: "
                         (or (:rail-kind rm) "food-mitsuho") " / "
                         (or (:provider-did rm) "—") "\n"))
        (conj! lines (str "- care-first-before-rails: "
                         (str/join "," (or (:care-first-before-rails rm) [])) "\n"))
        (conj! lines (str "- care-first-api-path: "
                         (or (:care-first-api-path rm) "care-first-mitsuho-path") "\n"))
        (conj! lines (str "- live-produce / produce-executed: "
                         (boolean (:live-produce rm)) "/"
                         (boolean (:produce-executed rm)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rm) [])) "\n"))
        (conj! lines (str "- live / cash / score-surface: "
                         (boolean (:live rm)) "/"
                         (or (:cash-usd-micros rm) 0) "/"
                         (pr-str (or (:score-surface rm) [])) "\n"))))
    (when-let [rh (:scorecard/rail-hikari-design body)]
      (when (and (map? rh) (not (:error rh)))
        (conj! lines "\n## rail-hikari DESIGN (priority 3 energy R1→gated)\n\n")
        (conj! lines (str "- rail-kind / provider: "
                         (or (:rail-kind rh) "energy-hikari") " / "
                         (or (:provider-did rh) "—") "\n"))
        (conj! lines (str "- care-first-before-rails: "
                         (str/join "," (or (:care-first-before-rails rh) [])) "\n"))
        (conj! lines (str "- care-first-api-path: "
                         (or (:care-first-api-path rh) "care-first-hikari-path") "\n"))
        (conj! lines (str "- live-produce / generate-executed: "
                         (boolean (:live-produce rh)) "/"
                         (boolean (:generate-executed rh)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rh) [])) "\n"))
        (conj! lines (str "- live / cash / score-surface: "
                         (boolean (:live rh)) "/"
                         (or (:cash-usd-micros rh) 0) "/"
                         (pr-str (or (:score-surface rh) [])) "\n"))))
    (when-let [rt (:scorecard/rail-tooling-design body)]
      (when (and (map? rt) (not (:error rt)))
        (conj! lines "\n## rail-tooling-okaimono DESIGN (priority 3 vocation)\n\n")
        (conj! lines (str "- rail-kind / provider: "
                         (or (:rail-kind rt) "tooling-okaimono") " / "
                         (or (:provider-did rt) "—") "\n"))
        (conj! lines (str "- care-first-api-path / vocation-recovery: "
                         (or (:care-first-api-path rt) "vocation-recovery-path") " / "
                         (boolean (:vocation-recovery rt true)) "\n"))
        (conj! lines (str "- fulfillment-executed / live-produce: "
                         (boolean (:fulfillment-executed rt)) "/"
                         (boolean (:live-produce rt)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rt) [])) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live rt)) "/"
                         (or (:cash-usd-micros rt) 0) "\n"))))
    (when-let [rco (:scorecard/rail-compute-design body)]
      (when (and (map? rco) (not (:error rco)))
        (conj! lines "\n## rail-compute-murakumo DESIGN (priority 3 vocation)\n\n")
        (conj! lines (str "- rail-kind / provider: "
                         (or (:rail-kind rco) "compute-murakumo") " / "
                         (or (:provider-did rco) "—") "\n"))
        (conj! lines (str "- care-first-api-path / vocation-recovery: "
                         (or (:care-first-api-path rco) "vocation-recovery-path") " / "
                         (boolean (:vocation-recovery rco true)) "\n"))
        (conj! lines (str "- quota-executed / live-produce: "
                         (boolean (:quota-executed rco)) "/"
                         (boolean (:live-produce rco)) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rco) [])) "\n"))
        (conj! lines (str "- live / cash: "
                         (boolean (:live rco)) "/"
                         (or (:cash-usd-micros rco) 0) "\n"))))
    (when-let [rl (:scorecard/rail-liquidity-design body)]
      (when (and (map? rl) (not (:error rl)))
        (conj! lines "\n## rail-liquidity-warifu DESIGN (priority 3 residual)\n\n")
        (conj! lines (str "- rail-kind / provider: "
                         (or (:rail-kind rl) "liquidity-warifu") " / "
                         (or (:provider-did rl) "—") "\n"))
        (conj! lines (str "- care-first-api-path / residual-rail: "
                         (or (:care-first-api-path rl) "liquidity-residual-path") " / "
                         (boolean (:residual-rail rl true)) "\n"))
        (conj! lines (str "- member-principal / loan-executed / cash: "
                         (boolean (:member-principal rl true)) "/"
                         (boolean (:loan-executed rl)) "/"
                         (or (:cash-usd-micros rl) 0) "\n"))
        (conj! lines (str "- multi-gen-facts: "
                         (str/join "," (or (:multi-gen-facts rl) [])) "\n"))
        (conj! lines (str "- live: " (boolean (:live rl)) "\n"))))
    (when-let [rcat (:scorecard/rail-design-catalog body)]
      (when (and (map? rcat) (not (:error rcat)))
        (conj! lines "\n## rail DESIGN catalog (all-seven discovery)\n\n")
        (conj! lines (str "- catalog-id: " (or (:catalog-id rcat) "fuchi.rail-design-catalog") "\n"))
        (conj! lines (str "- rail-count / ok-count: "
                         (or (:rail-count rcat) 0) "/"
                         (or (:ok-count rcat) 0) "\n"))
        (conj! lines (str "- rail-kinds: "
                         (str/join "," (or (:rail-kinds rcat) [])) "\n"))
        (conj! lines (str "- order: "
                         (str/join "→" (or (:order rcat) RAIL-DESIGN-ORDER)) "\n"))
        (conj! lines (str "- live-produce-never / all-cash-zero / all-live-false: "
                         (boolean (:live-produce-never rcat true)) "/"
                         (boolean (:all-cash-zero rcat true)) "/"
                         (boolean (:all-live-false rcat true)) "\n"))
        (conj! lines (str "- invariants loan-never/land-grant-never/all-seven-design: "
                         (boolean (get-in rcat [:invariants :loan-never] true)) "/"
                         (boolean (get-in rcat [:invariants :land-grant-never] true)) "/"
                         (boolean (get-in rcat [:invariants :all-seven-design] true)) "\n"))))
    (when-let [sp (:scorecard/ss-priority-path body)]
      (when (and (map? sp) (not (:error sp)))
        (conj! lines "\n## SS priority path (L0 + disclosure + all rails gated)\n\n")
        (conj! lines (str "- L0 stage/published: " (:l0-stage sp) "/"
                         (boolean (:l0-published sp)) "\n"))
        (conj! lines (str "- L0 enroll disclosure open/held/may-flow: "
                         (or (:l0-disclosure-state sp) "n/a") "/"
                         (boolean (:l0-disclosure-held sp)) "/"
                         (boolean (:l0-entitlements-may-flow sp true))
                         " path=" (or (:l0-path sp) "l0-enroll-offline") "\n"))
        (conj! lines (str "- ladder offline: " (or (:ladder-from sp) "L0") "→"
                         (or (:ladder-to sp) "—")
                         " target=" (or (:ladder-target sp) "—")
                         " steps=" (or (:ladder-steps sp) 0)
                         " rails-hint-first=" (or (:ladder-rails-hint-first sp) "—")
                         " published=" (boolean (:ladder-published sp)) "\n"))
        (conj! lines (str "- stage_sustenance: stage="
                         (or (:stage-sustenance-stage sp) "—")
                         " rails-first/second="
                         (or (:stage-rails-first sp) "—") "/"
                         (or (:stage-rails-second sp) "—")
                         " care-h/housing-mo="
                         (or (:stage-care-hours-floor-yr sp) 0) "/"
                         (or (:stage-housing-months-floor-yr sp) 0)
                         " land-grant=" (boolean (:stage-land-grant-executed sp))
                         " r2-all-refused=" (boolean (:stage-r2-all-refused sp))
                         " gated-all-refused=" (boolean (:stage-all-gated-refused sp))
                         " gated-count=" (or (:stage-gated-count sp) 0) "\n"))
        (conj! lines (str "- stage care/mitsuho/hikari gated-admissible: "
                         (boolean (:stage-care-gated-admissible sp)) "/"
                         (boolean (:stage-mitsuho-gated-admissible sp)) "/"
                         (boolean (:stage-hikari-gated-admissible sp)) "\n"))
        (conj! lines (str "- care/housing DESIGN live-produce / care-first-api (孫/子 first): "
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
        (conj! lines (str "- mitsuho/hikari DESIGN live-produce / care-first-api: "
                         (boolean (:mitsuho-live-produce sp)) "/"
                         (boolean (:hikari-live-produce sp)) " · "
                         (or (:mitsuho-care-first-api-path sp) "care-first-mitsuho-path") "/"
                         (or (:hikari-care-first-api-path sp) "care-first-hikari-path") "\n"))
        (conj! lines (str "- mitsuho/hikari design-rail-kind: "
                         (or (:mitsuho-design-rail-kind sp) "food-mitsuho") "/"
                         (or (:hikari-design-rail-kind sp) "energy-hikari") "\n"))
        (conj! lines (str "- tooling/compute/liquidity DESIGN live-produce / care-first-api: "
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
        (conj! lines (str "- all-seven design embed-count / live-produce-never: "
                         (or (:all-seven-design-embed-count sp) 7) "/"
                         (boolean (:all-seven-design-live-produce-never sp true)) "\n"))
        (conj! lines (str "- disclosure state / entitlements-may-flow: "
                         (:disclosure-state sp) "/"
                         (boolean (:entitlements-may-flow? sp)) "\n"))
        (conj! lines (str "- held-stress held / food-r1 / ladder-refused: "
                         (boolean (:held-stress-held? sp)) "/"
                         (or (:held-stress-food-phase sp) "—") "/"
                         (boolean (:held-stress-ladder-refused sp)) "\n"))
        (conj! lines (str "- rails-gated-count / admissible / all-rails-gated-refused: "
                         (or (:rails-gated-count sp) 0) "/"
                         (or (:rails-gated-admissible-count sp) 0) "/"
                         (boolean (:all-rails-gated-refused sp)) "\n"))
        (conj! lines (str "- mitsuho/hikari/care gated-admissible: "
                         (boolean (:mitsuho-gated-admissible sp)) "/"
                         (boolean (:hikari-gated-admissible sp)) "/"
                         (boolean (:care-gated-admissible sp)) "\n"))
        (conj! lines (str "- mitsuho/hikari gated-receive admissible/both-refused: "
                         (boolean (:mitsuho-gated-receive-admissible sp)) "/"
                         (boolean (:hikari-gated-receive-admissible sp)) "/"
                         (boolean (:mitsuho-hikari-receive-both-refused sp)) "\n"))
        (conj! lines (str "- care-iyashi gated-receive (孫/子) admissible/all-three-refused: "
                         (boolean (:care-gated-receive-admissible sp)) "/"
                         (boolean (:care-mitsuho-hikari-receive-all-refused sp)) "\n"))
        (conj! lines (str "- mitsuho/hikari gated-produce admissible/both/full-chain-refused: "
                         (boolean (:mitsuho-gated-produce-admissible sp)) "/"
                         (boolean (:hikari-gated-produce-admissible sp)) "/"
                         (boolean (:mitsuho-hikari-produce-both-refused sp)) "/"
                         (boolean (:mitsuho-hikari-full-chain-refused sp)) "\n"))
        (conj! lines (str "- care gated-produce (孫/子) admissible/produce-all/full-chain: "
                         (boolean (:care-gated-produce-admissible sp)) "/"
                         (boolean (:care-mitsuho-hikari-produce-all-refused sp)) "/"
                         (boolean (:care-mitsuho-hikari-full-chain-refused sp)) "\n"))
        (conj! lines (str "- housing gated-receive/produce (孫/子) admissible/full-chain: "
                         (boolean (:housing-gated-receive-admissible sp)) "/"
                         (boolean (:housing-gated-produce-admissible sp)) "/"
                         (boolean (:housing-full-chain-refused sp)) "\n"))
        (conj! lines (str "- care+housing+food+energy full-chain-refused: "
                         (boolean (:care-housing-mitsuho-hikari-full-chain-refused sp)) "\n"))
        (conj! lines (str "- tooling/compute gated-receive/produce admissible/full-chain: "
                         (boolean (:tooling-gated-receive-admissible sp)) "/"
                         (boolean (:tooling-gated-produce-admissible sp)) "/"
                         (boolean (:tooling-full-chain-refused sp)) " · "
                         (boolean (:compute-gated-receive-admissible sp)) "/"
                         (boolean (:compute-gated-produce-admissible sp)) "/"
                         (boolean (:compute-full-chain-refused sp)) "\n"))
        (conj! lines (str "- tooling+compute full-chain / all-inkind-produce-rails full-chain: "
                         (boolean (:tooling-compute-full-chain-refused sp)) "/"
                         (boolean (:all-inkind-produce-rails-full-chain-refused sp)) "\n"))
        (conj! lines (str "- liquidity gated-receive admissible/receive-full-chain: "
                         (boolean (:liquidity-gated-receive-admissible sp)) "/"
                         (boolean (:liquidity-receive-full-chain-refused sp)) "\n"))
        (conj! lines (str "- all-seven-rails receive-membrane refused: "
                         (boolean (:all-seven-rails-receive-membrane-refused sp)) "\n"))
        (conj! lines (str "- housing land-grant / liquidity loan / cash: "
                         (boolean (:housing-land-grant-executed sp)) "/"
                         (boolean (:liquidity-loan-executed sp)) "/"
                         (or (:liquidity-cash-usd-micros sp) 0) "\n"))
        (conj! lines (str "- ss R2 statuses / executed / all-not-executed: "
                         (or (:r2-status-count sp) 0) "/"
                         (or (:r2-executed-count sp) 0) "/"
                         (boolean (:all-r2-not-executed sp true)) "\n"))
        (conj! lines (str "- live: " (boolean (:live sp))
                         " cash: " (or (:cash-usd-micros sp) 0) "\n"))))
    (when-let [st (:scorecard/all-held-stress body)]
      (conj! lines "\n## All-disclosure-held stress (priority #2, offline)\n\n")
      (conj! lines (str "- stress: " (:stress st) "\n"))
      (conj! lines (str "- held subjects: " (:held-subjects st) "\n"))
      (conj! lines (str "- open-path gov flowable: " (:open-gov-flowable st) "\n"))
      (conj! lines (str "- all-held gov flowable: " (:gov-flowable st) "\n"))
      (conj! lines (str "- all-held tenure gov flowable: " (:tenure-gov-flowable st) "\n"))
      (conj! lines (str "- all-held G2 admissible cohorts: " (:g2-admissible-cohorts st) "\n"))
      (conj! lines (str "- land-grant-executed: " (:land-grant-executed st) "\n"))
      (conj! lines (str "- live: " (:live st) " cash: " (:cash-usd-micros st) "\n")))
    (when-let [ps (:scorecard/priority-stack-offline body)]
      (conj! lines "\n## Priority stack offline SSoT (1)L0 (2)disclosure (3)care-housing→mitsuho+hikari→all-seven\n\n")
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
      (conj! lines (str "- care-first-before-rails food/energy: "
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
        (conj! lines (str "- error: " (:error ps) "\n"))))
    (conj! lines "\n## Cohorts\n\n")
    (conj! lines
           (str "| actor | cohort | phase | n | L4-flow | L4-post | ten-flow | ten-post "
                "| land-grant | headroom | tenure | tenure-n |\n"))
    (conj! lines "|---|---|---|---|---|---|---|---|---|---|---|---|\n")
    (doseq [c (:scorecard/cohorts body)]
      (conj! lines
             (str "| " (:displacing-actor c) " | " (:cohort-id c) " | "
                  (:phase c) " | " (:subjects c) " | " (:committed c) " | "
                  (or (:committed-post-ratify c)
                      (:gov-post-ratify c) 0) " | "
                  (or (:tenure-gov-flowable c) 0) " | "
                  (or (:tenure-gov-post-ratify c) 0) " | "
                  0 " | "
                  (:headroom c) " | "
                  (or (:tenure-phase c) "—") " | "
                  (:tenure-subjects c) " |\n")))
    (conj! lines "\n## Live legs (default refuse)\n\n")
    (conj! lines "| leg | admissible | reason |\n|---|---|---|\n")
    (doseq [l (:scorecard/live-legs body)]
      (conj! lines
             (str "| " (:leg l) " | " (:admissible l) " | "
                  (or (:reason l) "—") " |\n")))
    (when-let [led (:scorecard/itonami-ledger body)]
      (conj! lines "\n## itonami surplus ledger (offline)\n\n")
      (conj! lines (str "- events: " (:events led) "\n"))
      (conj! lines (str "- funded-admissible: " (:funded-admissible led) "\n"))
      (conj! lines (str "- refused: " (:refused led) "\n"))
      (conj! lines (str "- cash-to-workers: " (or (:cash-to-workers-usd-micros led) 0) "\n")))
    (conj! lines "\n_No personal scores, ranks, or percentiles. No live disbursement._\n")
    (apply str (persistent! lines))))
(defn- join-out [name]
  #?(:clj (str (java.io.File. (java.io.File. (actor-dir) "out") name))
     :cljs (let [path (js/require "node:path")]
             (.join path (actor-dir) "out" name))))

(defn- ensure-out!
  []
  (let [d #?(:clj (java.io.File. (actor-dir) "out")
             :cljs (let [path (js/require "node:path")]
                     (.join path (actor-dir) "out")))]
    #?(:clj (.mkdirs d)
       :cljs (let [fs (js/require "node:fs")]
               (when-not (.existsSync fs d)
                 (.mkdirSync fs d #js {:recursive true}))))
    d))

(defn- write-text! [p content]
  #?(:clj (spit (str p) content)
     :cljs (.writeFileSync (js/require "node:fs") (str p) (str content) "utf8")))

(defn write-scorecard!
  "Write out/displacement-scorecard.{md,edn}. Portable under bb and nbb."
  ([]
   (let [body (build)
         _ (ensure-out!)
         md (join-out "displacement-scorecard.md")
         edn-p (join-out "displacement-scorecard.edn")]
     (write-text! md (scorecard-md body))
     (write-text! edn-p (pr-str body))
     {:md md
      :edn edn-p
      :live false
      :cash-usd-micros 0
      :score-surface []
      :all-live-refused (:scorecard/all-live-refused body)
      :tenure-subjects (:scorecard/tenure-subjects body)
      :priority-stack PRIORITY-STACK})))
