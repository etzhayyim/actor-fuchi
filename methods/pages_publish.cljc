(ns fuchi.methods.pages-publish
  "pages_publish.cljc — offline static site package for public SS facts (Pages-ready).

  Writes public/index.html (+ optional assets) from public_surface_report.
  Does NOT deploy, does NOT call Cloudflare API, live=false.
  Portable .cljc; file I/O at #?(:clj | :cljs/nbb) edge (ADR-2607173000)."
  (:require [fuchi.methods.public-surface-report :as rep]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.displacement-scorecard :as dsc]
            [fuchi.methods.pipeline-audit-ledger :as audit]
            [fuchi.methods.priority-stack :as pstack]
            [fuchi.methods.edn :as edn]
            #?(:clj [clojure.java.io :as io])))

(def PRIORITY-STACK pp/PRIORITY-STACK)

#?(:cljs
   (do
     (def ^:private fs (js/require "node:fs"))
     (def ^:private path (js/require "node:path"))))

(defn- actor-dir
  "Resolve FUCHI_ACTOR_DIR or repo root from this methods file."
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

(defn- read-edn-file [file-path]
  (edn/load-edn file-path))

(defn- audit-summary-safe
  "summary is now portable (bb + nbb); empty map on any I/O failure."
  []
  (try (audit/summary)
       (catch #?(:clj Exception :cljs :default) _
         {:runs 0 :live false :cash-usd-micros 0 :score-surface []
          :priority-stack PRIORITY-STACK})))

(def ^:private PUBLIC-README
  (str "# fuchi public surface (static)\n\n"
       "Generated offline. cash≡0. live=false. No personal scores.\n"
       "Priority: wellbecoming > mago > ko > present.\n\n"
       "## Offline priority path (robotics/itonami SS scaffold)\n\n"
       "1. **L0 enroll disclosure** — draft vow → triple CID stubs → L0 entitlement"
       " → public-person facts (`l0_enroll`).\n"
       "2. **Disclosure hold + continuity** — open/held/exit-suspended SM;"
       " stale/falsehood → hold entitlements (public-person may remain).\n"
       "3. **mitsuho (care-first) R1→gated DESIGN** — food after care/housing (孫/子);"
       " default refuse; then all-seven membranes"
       " (care/housing/food/energy/tooling/compute + liquidity residual);"
       " R2 execute refuse; land-grant never; loan never; live-produce never.\n\n"
       "SSoT API: `fuchi.methods.priority-stack/run-offline` ·"
       " design: `data/priority-stack-design.edn` ·"
       " machine facts: `priority-stack-offline.edn` ·"
       " smoke: `nbb -cp . methods/priority_stack_smoke.cljs`.\n\n"
       "Care-first paths (孫/子 before present): L0 care-first+mitsuho,"
       " L0 care-first+hikari, L0 care-first+mitsuho+hikari dual rail,"
       " L0 care+housing both-refused multi-gen substrate,"
       " L0 multi-gen substrate + mitsuho+hikari (L4 four-rail),"
       " L0 full-inkind six-rails (multi-gen + vocation / itonami),"
       " L0 vocation recovery (tooling+compute only),"
       " L0 liquidity residual (warifu member-principal; loan never),"
       " L0 all-seven substrate (capstone six in-kind + residual),"
       " L0 offline priority path catalog (discovery index),"
       " rail DESIGN catalog (all-seven single-rail discovery;"
       " care→housing→food→energy→tooling→compute→liquidity).\n"
       "All nine ladder paths embed priority-(2) held-stress"
       " (stale disclosure → ladder refuse; cash≡0; live=false).\n\n"
       "Also covered offline:\n"
       "- displacement→L0 enroll + L0→L4 multi-gen + L6 tenure scorecard\n"
       "- displacement L0 held-stress embed (stale → ladder refuse; carried into tenure)\n"
       "- SS offline path (ladder L4 → stage floors care/housing first → all rails)\n"
       "- SS priority path embeds all-seven design-public-facts"
       " (care→…→liquidity; live-produce never)\n"
       "- L0 all-seven membrane / L0 all-seven continuity/ladder\n"
       "- L0 held all-seven membrane (disclosure stale stress)\n"
       "- L0 exit→re-affirm / L0 falsehood→lift stresses\n"
       "- all-inkind-produce-rails + all-seven-rails receive-membrane refuse aggregates\n"
       "- all-seven rail design-public-facts + rail-design-catalog"
       " (live-produce never; cash≡0)\n"
       "- displacement L0 membranes + held-stress subjects/ladder-refused\n"
       "- gov + tenure-gov held-stress subjects/ladder-refused (G7 carry)\n"
       "- Audit summary: public/audit-summary.edn (pipeline runs append-only)\n\n"
       "Deploy: point Cloudflare Pages (or any static host) at this directory.\n"
       "Actual wrangler/API is **operator out-of-band**; scaffold never deploys.\n"
       "Operator runbook: see deploy-runbook.edn when pages_deploy package is written.\n"
       "Do not enable live sustenance disbursement from this package.\n"))

(defn write-pages!
  "Generate Pages-ready static files under public/ (and out/ mirror).
   Includes L4+L6 scorecard + audit summary. Never deploys.
   Runnable under bb (:clj) and nbb (:cljs)."
  []
  (let [actor (actor-dir)
        seed-path (join-path actor "data" "seed-sustenance-graph.kotoba.edn")
        seed (read-edn-file seed-path)
        html (rep/report-html seed :include-l0-demo true :include-itonami true)
        edn-body (rep/report-edn seed :include-l0-demo true :include-itonami true
                                 :include-scorecard true :include-ss-priority-path true)
        scard (get edn-body :report/displacement-scorecard {})
        audit-sum (audit-summary-safe)
        pub (join-path actor "public")
        outd (join-path actor "out")]
    (ensure-dir! pub)
    (ensure-dir! outd)
    (doseq [f (:report/public-persons edn-body)]
      (pp/assert-no-public-scores! f))
    (write-text! (join-path pub "index.html") html)
    (write-text! (join-path pub "facts.edn") (pr-str edn-body))
    (when (seq scard)
      (write-text! (join-path pub "scorecard.md") (dsc/scorecard-md scard))
      (write-text! (join-path pub "scorecard.edn") (pr-str scard))
      (write-text! (join-path outd "displacement-scorecard.md") (dsc/scorecard-md scard))
      (write-text! (join-path outd "displacement-scorecard.edn") (pr-str scard)))
    ;; Machine-readable priority stack SSoT (1)L0 (2)disclosure (3)mitsuho — facts only
    (let [ps (or (get edn-body :report/priority-stack-offline)
                 (get scard :scorecard/priority-stack-offline)
                 (try (pstack/public-facts (pstack/run-offline {}))
                      (catch #?(:clj Exception :cljs :default) _
                        {:path "priority-stack-offline"
                         :ok false
                         :error "priority-stack unavailable"
                         :live false
                         :cash-usd-micros 0
                         :score-surface []
                         :priority-stack PRIORITY-STACK}))]
      (pp/assert-no-public-scores!
       (dissoc ps :priority-order :mitsuho-care-first-before-rails :priority-stack :error))
      (write-text! (join-path pub "priority-stack-offline.edn") (pr-str ps))
      (write-text! (join-path outd "priority-stack-offline.edn") (pr-str ps)))
    (write-text! (join-path pub "audit-summary.edn") (pr-str audit-sum))
    (write-text! (join-path pub "_headers")
                 (str "/*\n  X-Frame-Options: DENY\n  X-Content-Type-Options: nosniff\n"
                      "  Referrer-Policy: no-referrer\n"
                      "  Content-Security-Policy: default-src 'self'; style-src 'unsafe-inline'\n"))
    (write-text! (join-path pub "robots.txt") "User-agent: *\nAllow: /\n")
    (write-text! (join-path pub "README.md") PUBLIC-README)
    (write-text! (join-path outd "public-surface.html") html)
    (when-let [md (try (rep/report-md seed :include-l0-demo true :include-itonami true)
                       (catch #?(:clj Exception :cljs :default) _ nil))]
      (write-text! (join-path outd "public-surface.md") md))
    {:index (join-path pub "index.html")
     :facts (join-path pub "facts.edn")
     :priority-stack-offline (join-path pub "priority-stack-offline.edn")
     :scorecard (when (seq scard) (join-path pub "scorecard.md"))
     :audit-summary (join-path pub "audit-summary.edn")
     :live false
     :cash-usd-micros 0
     :score-surface []
     :priority-stack PRIORITY-STACK
     :all-live-refused (boolean (:scorecard/all-live-refused scard))
     :audit-runs (or (:runs audit-sum) 0)
     :deployed false
     :ss-all-seven-design-embed
     (boolean (get-in scard [:scorecard/ss-priority-path
                             :all-seven-design-live-produce-never] true))}))
