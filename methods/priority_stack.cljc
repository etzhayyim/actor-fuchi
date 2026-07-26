(ns fuchi.methods.priority-stack
  "priority_stack.cljc — offline SSoT for robotics/itonami × etzhayyim SS priorities.

  Priority order (do not reorder):
    (1) L0 enroll offline scaffold
    (2) disclosure hold + continuity SM (held-stress)
    (3) mitsuho + hikari R1 → gated-live DESIGN (care/housing before food/energy; 孫/子)

  Invariants: wellbecoming > 孫 > 子; public-person facts only; no scores; cash≡0;
  live default refuse. Portable .cljc (bb + nbb; ADR-2607173000)."
  (:require [clojure.string :as str]
            [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.disclosure-continuity :as disc]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.edn :as edn]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(def PRIORITY-ORDER
  [(1 "L0 enroll offline scaffold")
   (2 "disclosure hold + continuity SM + held-stress")
   (3 "care-housing → mitsuho+hikari DESIGN → all-seven substrate (孫/子 first)")])

(def DESIGN-EDN "priority-stack-design.edn")
(def L0-PATHS-DESIGN-EDN "l0-offline-priority-paths-design.edn")

(defn load-design
  "Load data/priority-stack-design.edn (portable nbb+bb)."
  []
  (edn/load-data DESIGN-EDN))

(defn design-edn-invariants
  "Facts-only checks against priority-stack-design EDN: cash≡0, live false, (1)(2)(3) present."
  []
  (let [d (load-design)
        cash (or (get d ":design/cash-usd-micros") (get d :design/cash-usd-micros) 0)
        live (or (get d ":design/live") (get d :design/live) false)
        scores (or (get d ":design/score-surface") (get d :design/score-surface) [])
        order (or (get d ":design/priority-order") (get d :design/priority-order) [])
        ns- (mapv #(or (get % ":n") (get % :n)) order)
        out {:design-id (or (get d ":design/id") (get d :design/id))
             :cash-usd-micros cash
             :live live
             :score-surface scores
             :priority-order-ns ns-
             :order-count (count order)
             :module (or (get d ":design/module") (get d :design/module))
             :api (or (get d ":design/api") (get d :design/api))
             :priority-stack PRIORITY-STACK}]
    (when-not (zero? (long cash))
      (throw (ex-info "priority-stack design cash≡0" out)))
    (when (true? live)
      (throw (ex-info "priority-stack design live must be false" out)))
    (when-not (= [1 2 3] (mapv long (filter some? ns-)))
      (throw (ex-info "priority-stack design must declare order n=1,2,3" out)))
    (pp/assert-no-public-scores!
     (dissoc out :priority-order-ns :priority-stack :module :api :design-id))
    out))

(defn l0-paths-design-invariants
  "Facts-only checks against data/l0-offline-priority-paths-design.edn:
   cash≡0, live false, all catalog paths declare held-stress-embed, order 1→2→3."
  []
  (let [d (edn/load-data L0-PATHS-DESIGN-EDN)
        inv (or (get d ":design/invariants") (get d :design/invariants) {})
        cash (or (get inv ":cash-usd-micros") (get inv :cash-usd-micros) 0)
        live (or (get inv ":live") (get inv :live) false)
        scores (or (get inv ":score-surface") (get inv :score-surface) [])
        paths (or (get d ":design/paths") (get d :design/paths) [])
        held? (fn [p] (or (get p ":held-stress-embed") (get p :held-stress-embed)))
        all-held (every? held? paths)
        order (or (get d ":design/priority-order") (get d :design/priority-order) [])
        order-ns (mapv #(or (when (vector? %) (first %))
                            (get % ":n") (get % :n)
                            (when (sequential? %) (first %)))
                       order)
        out {:design-id (or (get d ":design/id") (get d :design/id))
             :cash-usd-micros cash
             :live live
             :score-surface scores
             :path-count (count paths)
             :all-paths-held-stress-embed all-held
             :priority-order-ns (mapv long (filter some? order-ns))
             :priority-stack PRIORITY-STACK}]
    (when-not (zero? (long cash))
      (throw (ex-info "l0-paths design cash≡0" out)))
    (when (true? live)
      (throw (ex-info "l0-paths design live must be false" out)))
    (when-not (= [] (or scores []))
      (throw (ex-info "l0-paths design score-surface empty" out)))
    (when-not (= 9 (count paths))
      (throw (ex-info "l0-paths design expects 9 PRIORITY-PATH-CATALOG paths" out)))
    (when-not all-held
      (throw (ex-info "l0-paths every path must :held-stress-embed true" out)))
    (when-not (= [1 2 3] (:priority-order-ns out))
      (throw (ex-info "l0-paths design order n=1,2,3" out)))
    (pp/assert-no-public-scores!
     (dissoc out :priority-order-ns :priority-stack :design-id))
    out))

(def FRESH
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted :related-party-edges []
   :rider-s2-self-report :none})

(def STALE
  {:wage-labor-band :stale :state-benefits? false
   :wellbecoming-attest-fact :stale :related-party-edges []
   :rider-s2-self-report :none})

(defn- zero-cash? [m]
  (zero? (long (or (:cash-usd-micros m) 0))))

(defn- not-live? [m]
  (not (true? (:live m))))

(defn- assert-slice! [label m]
  (when-not (zero-cash? m)
    (throw (ex-info (str label " cash≡0") m)))
  (when-not (not-live? m)
    (throw (ex-info (str label " live refuse") m)))
  (pp/assert-no-public-scores!
   (dissoc m :note :priority-stack :multi-gen-facts :care-first-before-rails
           :care-first-api-path :held-stress :history :series :machine
           :person :vow :entitlement :public-person :disclosure-gate
           :disclosure-hold :disclosure-continuity :intent :package
           :design :design-edn :care-design :mitsuho-design :hikari-design
           :rail :care-first-path :gated-status :l0 :disclosure :mitsuho :hikari
           :care-housing :all-seven :path-body :api-path
           :l0-paths-design :priority-order :path :enrolled))
  true)

(defn run-l0
  "Priority (1): L0 enroll offline. Never live mint."
  [opts]
  (let [opts (merge {:subject-did "did:web:etzhayyim.com:member:priority-stack"
                     :vow-text "悔い改め・バプテスマ・得度 — permanent commitment for descendant wellbecoming"
                     :member-signature "sig-offline-priority-stack-representative"
                     :covenant "vowed"
                     :disclosure FRESH
                     :food-imputed-usd-micros-yr 2000000000
                     :care-imputed-usd-micros-yr 1000000000}
                    opts)
        e (l0/enroll opts)
        out {:priority 1
             :slice "l0-enroll-offline"
             :stage (or (get-in e [:public-person :stage])
                        (get-in e [:entitlement :stage])
                        "L0")
             :public-person? (boolean (get-in e [:public-person :public-person?]))
             :published (boolean (get-in e [:vow :published]))
             :token-stub (some? (get-in e [:vow :token-id]))
             :disclosure-state (when-let [s (:disclosure-state e)]
                                 (if (keyword? s) (name s) (str s)))
             :entitlements-may-flow? (boolean (:entitlements-may-flow? e))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :enrolled e}]
    (assert-slice! "L0" out)
    (when-not (= "L0" (:stage out))
      (throw (ex-info "L0 stage expected" out)))
    (when (true? (:published out))
      (throw (ex-info "L0 must not publish" out)))
    out))

(defn run-disclosure
  "Priority (2): hold SM + continuity tick-series (fresh→stale→fresh) + held-stress."
  [enrolled]
  (let [did (or (get-in enrolled [:enrolled :person :did])
                (get-in enrolled [:enrolled :vow :subject-did])
                "did:web:etzhayyim.com:member:priority-stack")
        person {:did did :covenant "vowed"
                :rails [{:kind "food" :active? true}]
                :floor-usd-micros-yr 2000000000
                :disclosure FRESH :exit-suspended? false}
        open-hm (dh/initial person)
        held-hm (dh/initial (assoc person :disclosure STALE))
        series (disc/tick-series person [FRESH STALE FRESH])
        hist (:history series)
        mid (second hist)
        cont (l0/continuity-stress (:enrolled enrolled))
        out {:priority 2
             :slice "disclosure-hold-continuity"
             :open-may-flow (boolean (disc/entitlements-may-flow? open-hm))
             :stale-held (boolean (:entitlements-held? held-hm))
             :stale-may-flow (boolean (disc/entitlements-may-flow? held-hm))
             :tick-history-count (count hist)
             :mid-held? (boolean (:held? mid))
             :final-state (when-let [s (:final-state series)]
                            (if (keyword? s) (name s) (str s)))
             :final-may-flow (boolean (disc/entitlements-may-flow? (:machine series)))
             :continuity-held-steps (or (:held-steps cont) 0)
             :continuity-live (boolean (:live cont))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK}]
    (assert-slice! "disclosure" out)
    (when-not (true? (:open-may-flow out))
      (throw (ex-info "open disclosure must allow flow" out)))
    (when (true? (:stale-may-flow out))
      (throw (ex-info "stale disclosure must block flow" out)))
    (when-not (true? (:stale-held out))
      (throw (ex-info "stale initial must hold entitlements" out)))
    (when-not (= 3 (:tick-history-count out))
      (throw (ex-info "tick-series history length 3" out)))
    (when-not (true? (:mid-held? out))
      (throw (ex-info "mid tick must held?" out)))
    (when-not (= "open" (:final-state out))
      (throw (ex-info "final tick must reopen" out)))
    (when-not (true? (:final-may-flow out))
      (throw (ex-info "final machine may-flow" out)))
    (when-not (pos? (:continuity-held-steps out))
      (throw (ex-info "continuity-stress held-steps" out)))
    out))

(defn run-mitsuho-design
  "Priority (3): mitsuho R1 dry → gated default refuse + design EDN care-first.
   Optional care-first-mitsuho-path for 孫/子 ordering."
  [opts]
  (let [did (or (:subject-did opts)
                "did:web:etzhayyim.com:member:priority-stack")
        person {:did did :covenant "vowed"
                :rails [{:kind "food" :active? true}]
                :floor-usd-micros-yr 2000000000
                :disclosure FRESH :exit-suspended? false}
        pkg (mitsuho/r1-dry-package
             {:alloc-id (or (:alloc-id opts) "priority-stack-mitsuho")
              :subject-did did
              :imputed-usd-micros-yr (or (:food-imputed-usd-micros-yr opts) 2000000000)
              :person person})
        st (mitsuho/gated-live-status pkg)
        d (mitsuho/design-public-facts)
        inv (mitsuho/design-edn-invariants)
        cf (l0/care-first-mitsuho-path
            (merge {:subject-did did
                    :vow-text (or (:vow-text opts)
                                  "悔い改め・バプテスマ・得度 — permanent commitment for descendant wellbecoming")
                    :member-signature (or (:member-signature opts)
                                          "sig-offline-priority-stack-representative")
                    :covenant (or (:covenant opts) "vowed")
                    :disclosure FRESH
                    :food-imputed-usd-micros-yr 2000000000
                    :care-imputed-usd-micros-yr 1000000000}
                   (select-keys opts [:subject-did :food-imputed-usd-micros-yr
                                      :care-imputed-usd-micros-yr :member-signature
                                      :vow-text :covenant])))
        out {:priority 3
             :slice "mitsuho-r1-gated-design"
             :r1-phase (when-let [p (:phase pkg)] (if (keyword? p) (name p) (str p)))
             :gated-phase (when-let [p (:phase st)] (if (keyword? p) (name p) (str p)))
             :gated-admissible (boolean (:admissible st))
             :produce-executed (boolean (:produce-executed st false))
             :care-first-api-path (:care-first-api-path d)
             :care-first-before-rails (:care-first-before-rails d)
             :design-live-produce (boolean (:live-produce d))
             :design-edn-cash (long (:cash-usd-micros inv 0))
             :design-edn-live (boolean (:design-live inv))
             :care-first-path-live (boolean (:live cf))
             :held-stress-ladder-refused (boolean (:held-stress-ladder-refused cf))
             :held-stress-both-refused (boolean (:held-stress-both-refused cf))
             :mitsuho-live-produce (boolean (:mitsuho-live-produce cf false))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :package pkg
             :gated-status st
             :design d
             :design-edn inv
             :care-first-path cf}]
    (assert-slice! "mitsuho" out)
    (when-not (= "R1-dry" (:r1-phase out))
      (throw (ex-info "R1 dry expected" out)))
    (when-not (= "refused" (:gated-phase out))
      (throw (ex-info "gated default refuse expected" out)))
    (when (true? (:produce-executed out))
      (throw (ex-info "produce never" out)))
    (when-not (= "care-first-mitsuho-path" (:care-first-api-path out))
      (throw (ex-info "care-first api path" out)))
    (when-not (= ["care" "housing"] (:care-first-before-rails out))
      (throw (ex-info "food after care/housing 孫/子" out)))
    (when-not (zero? (:design-edn-cash out))
      (throw (ex-info "design EDN cash" out)))
    (when (true? (:design-edn-live out))
      (throw (ex-info "design EDN live" out)))
    (when-not (true? (:held-stress-ladder-refused out))
      (throw (ex-info "care-first held-stress ladder refuse embed" out)))
    out))

(defn run-hikari-design
  "Priority (3) energy twin: hikari R1 dry → gated default refuse + design EDN care-first.
   care-first-hikari-path for 孫/子 ordering (energy after care/housing)."
  [opts]
  (let [did (or (:subject-did opts)
                "did:web:etzhayyim.com:member:priority-stack")
        person {:did did :covenant "vowed"
                :rails [{:kind "energy" :active? true}]
                :floor-usd-micros-yr 1500000000
                :disclosure FRESH :exit-suspended? false}
        pkg (hikari/r1-dry-package
             {:alloc-id (or (:alloc-id opts) "priority-stack-hikari")
              :subject-did did
              :imputed-usd-micros-yr (or (:energy-imputed-usd-micros-yr opts) 1500000000)
              :person person})
        st (hikari/gated-live-status pkg)
        d (hikari/design-public-facts)
        inv (hikari/design-edn-invariants)
        cf (l0/care-first-hikari-path
            (merge {:subject-did did
                    :vow-text (or (:vow-text opts)
                                  "悔い改め・バプテスマ・得度 — permanent commitment for descendant wellbecoming")
                    :member-signature (or (:member-signature opts)
                                          "sig-offline-priority-stack-representative")
                    :covenant (or (:covenant opts) "vowed")
                    :disclosure FRESH
                    :energy-imputed-usd-micros-yr 1500000000
                    :care-imputed-usd-micros-yr 1000000000}
                   (select-keys opts [:subject-did :energy-imputed-usd-micros-yr
                                      :care-imputed-usd-micros-yr :member-signature
                                      :vow-text :covenant])))
        out {:priority 3
             :slice "hikari-r1-gated-design"
             :rail "energy-hikari"
             :r1-phase (when-let [p (:phase pkg)] (if (keyword? p) (name p) (str p)))
             :gated-phase (when-let [p (:phase st)] (if (keyword? p) (name p) (str p)))
             :gated-admissible (boolean (:admissible st))
             :produce-executed (boolean (or (:produce-executed st false)
                                            (:generate-executed st false)))
             :care-first-api-path (:care-first-api-path d)
             :care-first-before-rails (:care-first-before-rails d)
             :design-live-produce (boolean (:live-produce d))
             :design-edn-cash (long (:cash-usd-micros inv 0))
             :design-edn-live (boolean (:design-live inv))
             :care-first-path-live (boolean (:live cf))
             :held-stress-ladder-refused (boolean (:held-stress-ladder-refused cf))
             :hikari-live-produce (boolean (:hikari-live-produce cf false))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :package pkg
             :gated-status st
             :design d
             :design-edn inv
             :care-first-path cf}]
    (assert-slice! "hikari" out)
    (when-not (= "R1-dry" (:r1-phase out))
      (throw (ex-info "hikari R1 dry expected" out)))
    (when-not (= "refused" (:gated-phase out))
      (throw (ex-info "hikari gated default refuse expected" out)))
    (when (true? (:produce-executed out))
      (throw (ex-info "hikari produce/generate never" out)))
    (when-not (= "care-first-hikari-path" (:care-first-api-path out))
      (throw (ex-info "hikari care-first api path" out)))
    (when-not (= ["care" "housing"] (:care-first-before-rails out))
      (throw (ex-info "energy after care/housing 孫/子" out)))
    (when-not (zero? (:design-edn-cash out))
      (throw (ex-info "hikari design EDN cash" out)))
    (when (true? (:design-edn-live out))
      (throw (ex-info "hikari design EDN live" out)))
    (when-not (true? (:held-stress-ladder-refused out))
      (throw (ex-info "hikari care-first held-stress ladder refuse embed" out)))
    out))

(defn run-care-housing-substrate
  "Priority (3) multi-gen foundation: care+housing before food/energy (孫/子).
   care-housing-first-path: both full-chains refuse offline; land-grant never; held-stress."
  [opts]
  (let [did (or (:subject-did opts)
                "did:web:etzhayyim.com:member:priority-stack")
        ch (l0/care-housing-first-path
            (merge {:subject-did did
                    :vow-text (or (:vow-text opts)
                                  "悔い改め・バプテスマ・得度 — permanent commitment for descendant wellbecoming")
                    :member-signature (or (:member-signature opts)
                                          "sig-offline-priority-stack-representative")
                    :covenant (or (:covenant opts) "vowed")
                    :disclosure FRESH
                    :care-imputed-usd-micros-yr 1000000000
                    :housing-imputed-usd-micros-yr 12000000000}
                   (select-keys opts [:subject-did :care-imputed-usd-micros-yr
                                      :housing-imputed-usd-micros-yr :member-signature
                                      :vow-text :covenant])))
        out {:priority 3
             :slice "care-housing-multi-gen-substrate"
             :api-path "care-housing-first-path"
             :care-housing-both-refused (boolean (:care-housing-both-refused ch))
             :care-full-chain-refused (boolean (:care-full-chain-refused ch))
             :housing-full-chain-refused (boolean (:housing-full-chain-refused ch))
             :land-grant-executed (boolean (:land-grant-executed ch false))
             :held-stress-ladder-refused (boolean (:held-stress-ladder-refused ch))
             :held-stress-both-refused (boolean (:held-stress-both-refused ch))
             :entitlements-may-flow (boolean (:entitlements-may-flow ch))
             :disclosure-state (:disclosure-state ch)
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :path-body ch}]
    (assert-slice! "care-housing" out)
    (when-not (true? (:care-housing-both-refused out))
      (throw (ex-info "care+housing both refuse expected offline" out)))
    (when (true? (:land-grant-executed out))
      (throw (ex-info "land-grant never" out)))
    (when-not (true? (:held-stress-ladder-refused out))
      (throw (ex-info "care-housing held-stress ladder refuse" out)))
    (when (true? (:live out))
      (throw (ex-info "care-housing live refuse" out)))
    out))

(defn run-all-seven-substrate
  "Priority (3) capstone: six in-kind + liquidity residual (all-seven-substrate-path).
   All membranes default refuse; loan/land-grant/fulfillment/quota never; cash≡0; held-stress."
  [opts]
  (let [did (or (:subject-did opts)
                "did:web:etzhayyim.com:member:priority-stack")
        a7 (l0/all-seven-substrate-path
            (merge {:subject-did did
                    :vow-text (or (:vow-text opts)
                                  "悔い改め・バプテスマ・得度 — permanent commitment for descendant wellbecoming")
                    :member-signature (or (:member-signature opts)
                                          "sig-offline-priority-stack-representative")
                    :covenant (or (:covenant opts) "vowed")
                    :disclosure FRESH
                    :care-imputed-usd-micros-yr 1000000000
                    :housing-imputed-usd-micros-yr 12000000000
                    :food-imputed-usd-micros-yr 2000000000
                    :energy-imputed-usd-micros-yr 1500000000}
                   (select-keys opts [:subject-did :care-imputed-usd-micros-yr
                                      :housing-imputed-usd-micros-yr
                                      :food-imputed-usd-micros-yr
                                      :energy-imputed-usd-micros-yr
                                      :member-signature :vow-text :covenant])))
        out {:priority 3
             :slice "all-seven-substrate-capstone"
             :api-path "all-seven-substrate-path"
             :all-inkind-full-chain-refused
             (boolean (:all-inkind-produce-rails-full-chain-refused a7))
             :all-seven-membrane-refused
             (boolean (:all-seven-rails-receive-membrane-refused a7))
             :liquidity-receive-refused
             (boolean (:liquidity-receive-full-chain-refused a7))
             :liquidity-member-principal (boolean (:liquidity-member-principal a7 true))
             :liquidity-loan-executed (boolean (:liquidity-loan-executed a7 false))
             :liquidity-cash-usd-micros (long (or (:liquidity-cash-usd-micros a7) 0))
             :land-grant-executed (boolean (:land-grant-executed a7 false))
             :fulfillment-executed (boolean (:fulfillment-executed a7 false))
             :quota-executed (boolean (:quota-executed a7 false))
             :held-stress-ladder-refused (boolean (:held-stress-ladder-refused a7))
             :held-stress-membrane-refused (boolean (:held-stress-membrane-refused a7))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :path-body a7}]
    (assert-slice! "all-seven" out)
    (when-not (true? (:all-inkind-full-chain-refused out))
      (throw (ex-info "all-seven in-kind full-chain refuse" out)))
    (when-not (true? (:all-seven-membrane-refused out))
      (throw (ex-info "all-seven membrane refuse" out)))
    (when-not (true? (:liquidity-receive-refused out))
      (throw (ex-info "liquidity residual receive refuse" out)))
    (when-not (true? (:liquidity-member-principal out))
      (throw (ex-info "liquidity must be member-principal" out)))
    (when (true? (:liquidity-loan-executed out))
      (throw (ex-info "loan never" out)))
    (when-not (zero? (:liquidity-cash-usd-micros out))
      (throw (ex-info "liquidity cash≡0" out)))
    (when (true? (:land-grant-executed out))
      (throw (ex-info "land-grant never" out)))
    (when (true? (:fulfillment-executed out))
      (throw (ex-info "fulfillment never" out)))
    (when (true? (:quota-executed out))
      (throw (ex-info "quota never" out)))
    (when-not (true? (:held-stress-ladder-refused out))
      (throw (ex-info "all-seven held-stress ladder refuse" out)))
    (when (true? (:live out))
      (throw (ex-info "all-seven live refuse" out)))
    out))

(defn run-offline
  "Run priorities (1)→(2)→(3) offline. Returns facts-only summary.
   (3) = care-housing multi-gen → mitsuho+hikari R1→gated DESIGN → all-seven capstone.
   Throws on invariant breach. Never live. cash≡0.
   Validates priority-stack-design.edn + l0-offline-priority-paths-design.edn."
  ([]
   (run-offline {}))
  ([opts]
   (let [design-inv (try (design-edn-invariants)
                         (catch #?(:clj Exception :cljs :default) e
                           (throw (ex-info (str "priority-stack design: "
                                               (or (ex-message e) e))
                                          {:cause e}))))
         paths-inv (try (l0-paths-design-invariants)
                        (catch #?(:clj Exception :cljs :default) e
                          (throw (ex-info (str "l0-paths design: "
                                              (or (ex-message e) e))
                                         {:cause e}))))
         l0-out (run-l0 opts)
         d2 (run-disclosure l0-out)
         d3ch (run-care-housing-substrate opts)
         d3m (run-mitsuho-design opts)
         d3h (run-hikari-design opts)
         d3a7 (run-all-seven-substrate opts)
         summary {:path "priority-stack-offline"
                  :priority-order PRIORITY-ORDER
                  :priority-stack PRIORITY-STACK
                  :design design-inv
                  :l0-paths-design paths-inv
                  :l0 l0-out
                  :disclosure d2
                  :care-housing d3ch
                  :mitsuho d3m
                  :hikari d3h
                  :all-seven d3a7
                  :ok true
                  :live false
                  :cash-usd-micros 0
                  :score-surface []
                  :loan-never true
                  :land-grant-never true
                  :public-person-facts-only true
                  :default-refuse true
                  :note "offline (1)L0 (2)disclosure (3)care-housing→mitsuho+hikari→all-seven"}]
     (assert-slice! "priority-stack" summary)
     (pp/assert-no-public-scores!
      (select-keys summary [:live :cash-usd-micros :score-surface :ok
                            :loan-never :land-grant-never
                            :public-person-facts-only :default-refuse]))
     summary)))

(defn public-facts
  "Strip nested bodies for scorecard/public surfaces (facts only)."
  [summary]
  (let [out {:path (:path summary)
             :priority-order PRIORITY-ORDER
             :priority-stack PRIORITY-STACK
             :design-id (get-in summary [:design :design-id])
             :design-order-count (get-in summary [:design :order-count])
             :l0-paths-design-id (get-in summary [:l0-paths-design :design-id])
             :l0-paths-count (get-in summary [:l0-paths-design :path-count])
             :l0-paths-all-held-stress (get-in summary [:l0-paths-design :all-paths-held-stress-embed])
             :l0-stage (get-in summary [:l0 :stage])
             :l0-public-person? (get-in summary [:l0 :public-person?])
             :l0-published (get-in summary [:l0 :published])
             :disclosure-open-may-flow (get-in summary [:disclosure :open-may-flow])
             :disclosure-stale-held (get-in summary [:disclosure :stale-held])
             :disclosure-tick-final (get-in summary [:disclosure :final-state])
             :disclosure-continuity-held-steps (get-in summary [:disclosure :continuity-held-steps])
             :care-housing-api-path (get-in summary [:care-housing :api-path])
             :care-housing-both-refused (get-in summary [:care-housing :care-housing-both-refused])
             :care-housing-land-grant-executed (get-in summary [:care-housing :land-grant-executed])
             :care-housing-held-stress-ladder-refused
             (get-in summary [:care-housing :held-stress-ladder-refused])
             :mitsuho-r1-phase (get-in summary [:mitsuho :r1-phase])
             :mitsuho-gated-phase (get-in summary [:mitsuho :gated-phase])
             :mitsuho-produce-executed (get-in summary [:mitsuho :produce-executed])
             :mitsuho-care-first-api-path (get-in summary [:mitsuho :care-first-api-path])
             :mitsuho-care-first-before-rails (get-in summary [:mitsuho :care-first-before-rails])
             :mitsuho-held-stress-ladder-refused (get-in summary [:mitsuho :held-stress-ladder-refused])
             :hikari-r1-phase (get-in summary [:hikari :r1-phase])
             :hikari-gated-phase (get-in summary [:hikari :gated-phase])
             :hikari-produce-executed (get-in summary [:hikari :produce-executed])
             :hikari-care-first-api-path (get-in summary [:hikari :care-first-api-path])
             :hikari-care-first-before-rails (get-in summary [:hikari :care-first-before-rails])
             :hikari-held-stress-ladder-refused (get-in summary [:hikari :held-stress-ladder-refused])
             :all-seven-api-path (get-in summary [:all-seven :api-path])
             :all-seven-inkind-refused (get-in summary [:all-seven :all-inkind-full-chain-refused])
             :all-seven-membrane-refused (get-in summary [:all-seven :all-seven-membrane-refused])
             :all-seven-liquidity-refused (get-in summary [:all-seven :liquidity-receive-refused])
             :all-seven-loan-executed (get-in summary [:all-seven :liquidity-loan-executed])
             :all-seven-land-grant-executed (get-in summary [:all-seven :land-grant-executed])
             :all-seven-held-stress-ladder-refused
             (get-in summary [:all-seven :held-stress-ladder-refused])
             :ok (boolean (:ok summary))
             :live false
             :cash-usd-micros 0
             :score-surface []
             :loan-never true
             :land-grant-never true
             :public-person-facts-only true
             :default-refuse true}]
    (pp/assert-no-public-scores!
     (dissoc out :priority-order :mitsuho-care-first-before-rails
             :hikari-care-first-before-rails :priority-stack))
    out))

(defn- fact-get
  "Lookup public-facts key as keyword, bare keyword, or fuchi.edn \":kw\" string."
  [m k]
  (let [nk (name k)
        sk (str ":" nk)]
    (or (get m k)
        (get m (keyword nk))
        (get m sk)
        (get m nk))))

(defn load-public-facts-file
  "Load priority-stack-offline.edn (keyword or \":…\" string keys). Portable nbb+bb.
   Strips leading ;; comment lines. Uses fuchi.methods.edn (string-key shape) then
   normalizes lookups via fact-get in assert-public-facts!."
  [path]
  (let [raw #?(:clj (slurp (str path))
               :cljs (.readFileSync (js/require "node:fs") (str path) "utf8"))
        body (->> (str/split-lines raw)
                  (remove #(or (str/blank? %)
                               (str/starts-with? (str/triml %) ";")))
                  (str/join "\n"))]
    (edn/parse-edn body)))

(defn assert-public-facts!
  "Validate a public-facts / priority-stack-offline.edn map. Throws on breach.
   Accepts keyword keys or fuchi.edn \":…\" string keys.
   Shared by smoke, pages package tests, readiness_check, and write_all post-check."
  [ps]
  (when-not (map? ps)
    (throw (ex-info "priority-stack public-facts must be a map" {:got ps})))
  (when (fact-get ps :error)
    (throw (ex-info "priority-stack public-facts has :error" ps)))
  (when-not (true? (fact-get ps :ok))
    (throw (ex-info "priority-stack :ok must be true" ps)))
  (when-not (= "L0" (fact-get ps :l0-stage))
    (throw (ex-info "priority-stack L0 stage" ps)))
  (when (true? (fact-get ps :l0-published))
    (throw (ex-info "priority-stack L0 must not publish" ps)))
  (when-not (true? (fact-get ps :disclosure-open-may-flow))
    (throw (ex-info "priority-stack disclosure open may-flow" ps)))
  (when-not (true? (fact-get ps :disclosure-stale-held))
    (throw (ex-info "priority-stack disclosure stale held" ps)))
  (when-not (= "open" (fact-get ps :disclosure-tick-final))
    (throw (ex-info "priority-stack disclosure tick final open" ps)))
  (when-not (= "care-housing-first-path" (fact-get ps :care-housing-api-path))
    (throw (ex-info "priority-stack care-housing api" ps)))
  (when-not (true? (fact-get ps :care-housing-both-refused))
    (throw (ex-info "priority-stack care-housing both refuse" ps)))
  (when (true? (fact-get ps :care-housing-land-grant-executed))
    (throw (ex-info "priority-stack land-grant never" ps)))
  (when-not (true? (fact-get ps :care-housing-held-stress-ladder-refused))
    (throw (ex-info "priority-stack care-housing held-stress ladder" ps)))
  (when-not (= "all-seven-substrate-path" (fact-get ps :all-seven-api-path))
    (throw (ex-info "priority-stack all-seven api" ps)))
  (when-not (true? (fact-get ps :all-seven-inkind-refused))
    (throw (ex-info "priority-stack all-seven inkind refuse" ps)))
  (when-not (true? (fact-get ps :all-seven-membrane-refused))
    (throw (ex-info "priority-stack all-seven membrane refuse" ps)))
  (when-not (true? (fact-get ps :all-seven-liquidity-refused))
    (throw (ex-info "priority-stack all-seven liquidity refuse" ps)))
  (when (true? (fact-get ps :all-seven-loan-executed))
    (throw (ex-info "priority-stack all-seven loan never" ps)))
  (when (true? (fact-get ps :all-seven-land-grant-executed))
    (throw (ex-info "priority-stack all-seven land-grant never" ps)))
  (when-not (true? (fact-get ps :all-seven-held-stress-ladder-refused))
    (throw (ex-info "priority-stack all-seven held-stress ladder" ps)))
  (when-not (= "R1-dry" (fact-get ps :mitsuho-r1-phase))
    (throw (ex-info "priority-stack mitsuho R1-dry" ps)))
  (when-not (= "refused" (fact-get ps :mitsuho-gated-phase))
    (throw (ex-info "priority-stack mitsuho gated refuse" ps)))
  (when (true? (fact-get ps :mitsuho-produce-executed))
    (throw (ex-info "priority-stack produce never" ps)))
  (when-not (= "care-first-mitsuho-path" (fact-get ps :mitsuho-care-first-api-path))
    (throw (ex-info "priority-stack care-first api" ps)))
  (let [before (fact-get ps :mitsuho-care-first-before-rails)
        before-v (mapv str before)]
    (when-not (= ["care" "housing"] before-v)
      (throw (ex-info "priority-stack care-first before rails" ps))))
  (when-not (true? (fact-get ps :mitsuho-held-stress-ladder-refused))
    (throw (ex-info "priority-stack held-stress ladder refuse" ps)))
  (when-not (= "R1-dry" (fact-get ps :hikari-r1-phase))
    (throw (ex-info "priority-stack hikari R1-dry" ps)))
  (when-not (= "refused" (fact-get ps :hikari-gated-phase))
    (throw (ex-info "priority-stack hikari gated refuse" ps)))
  (when (true? (fact-get ps :hikari-produce-executed))
    (throw (ex-info "priority-stack hikari produce never" ps)))
  (when-not (= "care-first-hikari-path" (fact-get ps :hikari-care-first-api-path))
    (throw (ex-info "priority-stack hikari care-first api" ps)))
  (let [before (fact-get ps :hikari-care-first-before-rails)
        before-v (mapv str before)]
    (when-not (= ["care" "housing"] before-v)
      (throw (ex-info "priority-stack hikari care-first before rails" ps))))
  (when-not (true? (fact-get ps :hikari-held-stress-ladder-refused))
    (throw (ex-info "priority-stack hikari held-stress ladder refuse" ps)))
  (when-not (= "fuchi.priority-stack-offline" (fact-get ps :design-id))
    (throw (ex-info "priority-stack design-id" ps)))
  (when-not (= 3 (long (or (fact-get ps :design-order-count) 0)))
    (throw (ex-info "priority-stack design-order-count" ps)))
  (when-not (= "fuchi.l0-offline-priority-paths" (fact-get ps :l0-paths-design-id))
    (throw (ex-info "priority-stack l0-paths design-id" ps)))
  (when-not (= 9 (long (or (fact-get ps :l0-paths-count) 0)))
    (throw (ex-info "priority-stack l0-paths-count=9" ps)))
  (when-not (true? (fact-get ps :l0-paths-all-held-stress))
    (throw (ex-info "priority-stack l0-paths all held-stress" ps)))
  (when (true? (fact-get ps :live))
    (throw (ex-info "priority-stack live refuse" ps)))
  (when-not (zero? (long (or (fact-get ps :cash-usd-micros) 0)))
    (throw (ex-info "priority-stack cash≡0" ps)))
  (when-not (= [] (or (fact-get ps :score-surface) []))
    (throw (ex-info "priority-stack score-surface empty" ps)))
  (pp/assert-no-public-scores!
   (dissoc ps :priority-order ":priority-order"
           :mitsuho-care-first-before-rails ":mitsuho-care-first-before-rails"
           :hikari-care-first-before-rails ":hikari-care-first-before-rails"
           :priority-stack ":priority-stack"
           :error ":error" :note ":note" :path ":path"))
  true)
