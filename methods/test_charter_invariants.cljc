(ns fuchi.methods.test-charter-invariants
  "Structural charter-invariant tests for 扶持 (fuchi).
  Portable under nbb and bb (ADR-2607173000). Parses ontology + lexicons + code.

  G1 instrument allowlist · G2 cash≡0 · G3 in-kind rails · G4 covenant-gated ·
  G5 owns-payoff false · G7 non-adjudicating · G9 no-server-key.

  PROVENANCE: test_g10_every_live_leg_refused_by_default deferred (R2 Autonomous)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.edn :as edn]
            [fuchi.methods.allocate :as allocate]
            [fuchi.methods.book :as book]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.provision :as prov]
            [fuchi.methods.live-gate :as live-gate]))

#?(:cljs
   (def ^:private path (js/require "node:path")))
#?(:cljs
   (def ^:private fs (js/require "node:fs")))

(defn- actor-root []
  (or (edn/actor-dir) "."))

(defn- path-exists? [p]
  #?(:clj (.exists (java.io.File. (str p)))
     :cljs (.existsSync fs (str p))))

(defn- resolve-join [root & parts]
  #?(:clj (str (reduce (fn [f p] (java.io.File. f (str p)))
                       (java.io.File. (str root))
                       parts))
     :cljs (apply (.-join path) (cons (str root) parts))))

(defn- repo-root
  "etzhayyim/root holding 00-contracts/schemas."
  []
  (or #?(:clj (System/getenv "FUCHI_REPO_ROOT")
         :cljs (.-FUCHI_REPO_ROOT (.-env js/process)))
      (let [cands [(resolve-join (actor-root) ".." "root")
                   (resolve-join (actor-root) ".." ".." "etzhayyim" "root")]]
        (some (fn [r]
                (when (path-exists? (resolve-join r "00-contracts" "schemas"))
                  r))
              cands))))

(defn- schema-path []
  (when-let [root (repo-root)]
    (resolve-join root "00-contracts" "schemas"
                  "maintainer-sustenance-ontology.kotoba.edn")))

(defn- lex-path [fname]
  (resolve-join (actor-root) "lex" fname))

(def INVESTMENT-TOKENS
  [":equity" ":debt" ":convertible" ":revenue-share"
   ":profit-claim" ":carry" ":dividend" ":exit" ":loan" ":interest"])

(defn- onto []
  (let [sp (schema-path)]
    (when-not sp
      (throw (ex-info "ontology schema not found; set FUCHI_REPO_ROOT to etzhayyim/root" {})))
    (edn/load-edn sp)))

(defn- lex [fname]
  (edn/load-edn (lex-path fname)))

(defn- attr [o ident]
  (or (first (filter #(= (get % ":db/ident") ident) (get o ":schema")))
      (throw (ex-info (str "attribute " ident " missing from schema") {:ident ident}))))

;; ── G1: no investment vehicle ───────────────────────────────────────────────
(deftest test-instrument-vocab-is-sustenance-only
  (let [instruments (set (get (onto) ":ontology/instruments"))]
    (is (= instruments #{":in-kind-grant" ":sustenance" ":tooling-access" ":compute-access"}))))

(deftest test-no-investment-token-anywhere-in-ontology
  (let [instr (attr (onto) ":alloc/instrument")
        allowed (set (get instr ":db/allowed"))]
    (doseq [tok INVESTMENT-TOKENS]
      (is (not (contains? allowed tok)) (str tok " must not be allocatable (G1)")))))

(deftest test-code-allowlist-matches-schema
  (let [o (set (map #(if (str/starts-with? % ":") (subs % 1) %)
                    (get (onto) ":ontology/instruments")))]
    (is (= (set allocate/ALLOWED-INSTRUMENTS) o))))

;; ── G2: cash≡0 ──────────────────────────────────────────────────────────────
(deftest test-envelope-cash-allowed-zero-only
  (is (= (get (attr (onto) ":envelope/cash-usd-micros") ":db/allowed") [0])))

(deftest test-alloc-cash-allowed-zero-only
  (is (= (get (attr (onto) ":alloc/cash-usd-micros") ":db/allowed") [0])))

;; ── G3: in-kind rails only ──────────────────────────────────────────────────
(deftest test-rail-vocab-has-no-cash-disbursement
  (let [rails (set (get (onto) ":ontology/rails"))]
    (is (not (contains? rails ":cash-disbursement")))
    (is (and (contains? rails ":housing-commons") (contains? rails ":liquidity-warifu")))))

(deftest test-rail-kind-allowed-matches-vocab
  (let [allowed (set (get (attr (onto) ":rail/kind") ":db/allowed"))]
    (is (= allowed (set (get (onto) ":ontology/rails"))))))

;; ── G4: covenant-gated ──────────────────────────────────────────────────────
(deftest test-covenant-vocab-excludes-anon-and-server
  (let [covs (set (get (onto) ":ontology/covenants"))]
    (is (= covs #{":outreach" ":vowed"}))
    (is (and (not (contains? covs ":anon")) (not (contains? covs ":server"))))))

;; ── G5: payoff attribution = etzhayyim ──────────────────────────────────────
(deftest test-owns-payoff-allowed-false-only
  (is (= (get (attr (onto) ":maintainer/owns-payoff") ":db/allowed") [false])))

;; ── G7: non-adjudicating route ──────────────────────────────────────────────
(deftest test-gov-routes-present
  (is (= (set (get (onto) ":ontology/gov-routes"))
         #{":auto" ":sbt-vote" ":council-lv7" ":refused"})))

(deftest test-no-decision-attribute-exists
  (let [idents (set (map #(get % ":db/ident") (get (onto) ":schema")))]
    (doseq [forbidden [":gov/decision" ":alloc/decision" ":triage/decision"]]
      (is (not (contains? idents forbidden))
          (str forbidden " must not exist (G7 non-adjudicating)")))))

;; ── G9: no-server-key ───────────────────────────────────────────────────────
(deftest test-server-held-key-allowed-false-only
  (is (= (get (attr (onto) ":alloc/server-held-key") ":db/allowed") [false])))

;; ── lexicon ↔ ontology cross-checks ─────────────────────────────────────────
(deftest test-alloc-lexicon-instrument-enum-matches-ontology
  (let [props (get-in (lex "allocationIntent.edn") [":defs" ":main" ":record" ":properties"])
        enum (set (get-in props [":instrument" ":enum"]))
        o (set (map #(if (str/starts-with? % ":") (subs % 1) %)
                    (get (onto) ":ontology/instruments")))]
    (is (= enum o))))

(deftest test-alloc-lexicon-cash-const-zero
  (let [props (get-in (lex "allocationIntent.edn") [":defs" ":main" ":record" ":properties"])]
    (is (= (get-in props [":cashUsdMicros" ":const"]) 0))
    (is (false? (get-in props [":serverHeldKey" ":const"])))))

(deftest test-covenant-lexicon-owns-payoff-const-false
  (let [props (get-in (lex "maintainerCovenant.edn") [":defs" ":main" ":record" ":properties"])]
    (is (false? (get-in props [":ownsPayoff" ":const"])))
    (is (false? (get-in props [":serverHeldKey" ":const"])))))

(deftest test-rail-lexicon-enum-matches-ontology
  (let [enum (set (get-in (lex "routingPlan.edn")
                          [":defs" ":main" ":record" ":properties" ":kind" ":enum"]))
        o (set (map #(if (str/starts-with? % ":") (subs % 1) %)
                    (get (onto) ":ontology/rails")))]
    (is (= enum o))))

;; ── R1(a) provisioning intent invariants ────────────────────────────────────
(deftest test-prov-cash-allowed-zero-only
  (is (= (get (attr (onto) ":prov/cash-usd-micros") ":db/allowed") [0])))

(deftest test-prov-published-allowed-false-only
  (is (= (get (attr (onto) ":prov/published") ":db/allowed") [false])))

(deftest test-prov-server-held-key-false-only
  (is (= (get (attr (onto) ":prov/server-held-key") ":db/allowed") [false])))

(deftest test-prov-lexicon-consts
  (let [props (get-in (lex "provisioningIntent.edn") [":defs" ":main" ":record" ":properties"])]
    (is (= (get-in props [":cashUsdMicros" ":const"]) 0))
    (is (false? (get-in props [":serverHeldKey" ":const"])))
    (is (false? (get-in props [":published" ":const"])))))

;; ── R1(b) 1 SBT = 1 vote invariants ─────────────────────────────────────────
(deftest test-ballot-weight-allowed-one-only
  (is (= (get (attr (onto) ":ballot/weight") ":db/allowed") [1])))

(deftest test-ballot-server-held-key-false-only
  (is (= (get (attr (onto) ":ballot/server-held-key") ":db/allowed") [false])))

(deftest test-ballot-choices-vocab
  (is (= (set (get (onto) ":ontology/ballot-choices")) #{":yes" ":no" ":abstain"})))

(deftest test-ballot-lexicon-weight-const-one
  (let [props (get-in (lex "voteBallot.edn") [":defs" ":main" ":record" ":properties"])]
    (is (= (get-in props [":weight" ":const"]) 1))
    (is (false? (get-in props [":serverHeldKey" ":const"])))))

;; ── R1(c) toritate booking invariants ───────────────────────────────────────
(deftest test-book-cash-allowed-zero-only
  (is (= (get (attr (onto) ":book/cash-usd-micros") ":db/allowed") [0])))

(deftest test-book-categories-have-no-payroll
  (let [cats (set (get (onto) ":ontology/book-categories"))]
    (doseq [forbidden [":payroll" ":salary" ":wage" ":bonus" ":commission"]]
      (is (not (contains? cats forbidden))))))

(deftest test-book-category-matches-toritate-enum
  (let [allowed (set (get (attr (onto) ":book/category") ":db/allowed"))]
    (is (= allowed (set (get (onto) ":ontology/book-categories"))))))

(deftest test-book-code-categories-match-schema
  (let [o (set (map #(if (str/starts-with? % ":") (subs % 1) %)
                    (get (onto) ":ontology/book-categories")))]
    (is (= (set book/TORITATE-CATEGORIES) o))))

(deftest test-booking-lexicon-cash-const-zero
  (let [props (get-in (lex "sustenanceBooking.edn") [":defs" ":main" ":record" ":properties"])]
    (is (= (get-in props [":cashUsdMicros" ":const"]) 0))))

(deftest test-flow-classes-vocab
  (is (= (set (get (onto) ":ontology/flow-classes"))
         #{":publicfund-to-fuchi" ":fuchi-to-provider" ":provider-to-maintainer"})))

;; ── R1(d) Displacement-Dividend coupling invariants ─────────────────────────
(deftest test-tithe-bps-is-ten-percent
  (is (= (get (onto) ":ontology/tithe-bps") 1000)))

(deftest test-code-tithe-bps-matches-ontology
  (is (= couple/TITHE-BPS (get (onto) ":ontology/tithe-bps"))))

(deftest test-earmark-funded-attr-exists
  (is (= (get (attr (onto) ":earmark/funded") ":db/valueType") ":db.type/boolean")))

(deftest test-couple-admissible-attr-exists
  (is (= (get (attr (onto) ":couple/admissible") ":db/valueType") ":db.type/boolean")))

(deftest test-tithe-split-is-exact-for-all-inputs
  (doseq [s [1 7 9999 10001 60000000000]]
    (let [em (couple/earmark-from-surplus
              (couple/make-displacement-event
               {:displacing-actor "a" :cohort-id "c" :displaced-count 1
                :surplus-usd-micros-yr s :funded true}))]
      (is (= (+ (:tithe-usd-micros em) (:earmark-usd-micros-yr em)) s)))))

(deftest test-g2-refuses-unfunded
  (let [e (couple/make-displacement-event
           {:displacing-actor "sanae" :cohort-id "c" :displaced-count 1
            :surplus-usd-micros-yr 10 :funded false})
        g (couple/coupling-gate e (couple/earmark-from-surplus e) 1)]
    (is (and (false? (get g "admissible")) (str/includes? (get g "reason") "G2")))))

;; ── G10 (R1-live) ───────────────────────────────────────────────────────────
(deftest test-g10-couple-is-invariant-adjacent-lv7
  (is (= (second (get live-gate/LEG-POLICY "couple")) 7))
  (is (every? #(= (second (get live-gate/LEG-POLICY %)) 6) ["provision" "vote" "book"])))

(deftest test-g10-live-mode-holds-cash-zero-and-no-server-key
  (let [leg "provision"
        g (live-gate/make-live-gate {:leg leg :operator-did "op" :council-level 6
                                     :member-signature "sig:m"})
        intents (prov/provision [{"kind" "food-mitsuho" "imputed_usd_micros_yr" 1000000}] "a")
        out (prov/dispatch-live intents g :env {(first (get live-gate/LEG-POLICY leg)) "1"})]
    (is (every? #(and (= (get-in % [:intent :cash-usd-micros]) 0)
                      (false? (get-in % [:intent :server-held-key]))) out))))
