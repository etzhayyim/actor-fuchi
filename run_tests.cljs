#!/usr/bin/env nbb
;; fuchi 扶持 — test suite host (ADR-2607173000: nbb only, no bash / no bb spawn).
;;
;; Usage (from actor-fuchi root):
;;   nbb -cp . run_tests.cljs
;;
;; Pure nbb suite: requires + clojure.test/run-tests in-process (no bb spawn).
;; Prefer nbb -cp . methods/priority_stack_smoke.cljs for pure (1)(2)(3) smoke.
(require '[clojure.test :as t])

(def fs (js/require "node:fs"))
(def path (js/require "node:path"))

(def ACTOR-DIR
  (or (.-FUCHI_ACTOR_DIR (.-env js/process))
      (.resolve path (.dirname path *file*))))

(defn- exists? [p] (.existsSync fs (str p)))

(defn- ensure-fuchi-symlink!
  "ns is fuchi.methods.* — provide fuchi/methods via self-symlink (idempotent)."
  []
  (let [link (.join path ACTOR-DIR "fuchi")]
    (when-not (exists? link)
      (.symlinkSync fs "." "fuchi" "dir")
      (println "created self-symlink fuchi/ → ."))))

(defn- resolve-repo-root!
  "etzhayyim/root holds 00-contracts/schemas (maintainer-sustenance-ontology)."
  []
  (let [cands [(.resolve path ACTOR-DIR ".." "root" "00-contracts" "schemas")
               (.resolve path ACTOR-DIR ".." ".." "etzhayyim" "root" "00-contracts" "schemas")]]
    (or (some (fn [schemas]
                (when (exists? schemas)
                  (.resolve path schemas ".." "..")))
              cands)
        nil)))

(def SUITE
  '[fuchi.cells.test-state-machine
    fuchi.methods.test-provision
    fuchi.methods.test-book
    fuchi.methods.test-allocate
    fuchi.methods.test-analyze
    fuchi.methods.test-charter-invariants
    fuchi.methods.test-route
    fuchi.methods.test-lexicons
    fuchi.methods.test-couple
    fuchi.methods.test-consistency
    fuchi.methods.test-vote
    fuchi.methods.test-live-gate
    fuchi.methods.test-public-person
    fuchi.methods.test-l0-enroll
    fuchi.methods.test-disclosure-hold
    fuchi.methods.test-priority-stack
    fuchi.methods.test-liberation-ladder
    ;; readiness structure covered inside test_priority_stack
    fuchi.methods.test-stage-sustenance
    fuchi.methods.test-disclosure-continuity
    fuchi.methods.test-displacement-book
    fuchi.methods.test-displacement-couple
    fuchi.methods.test-displacement-scorecard
    fuchi.methods.test-displacement-tenure
    fuchi.methods.test-displacement-pipeline
    fuchi.methods.test-displacement-gov
    fuchi.methods.test-pipeline-audit-ledger
    fuchi.methods.test-rail-mitsuho
    fuchi.methods.test-rail-hikari
    fuchi.methods.test-public-surface-report
    fuchi.methods.test-displacement-surface
    fuchi.methods.test-itonami-bridge
    fuchi.methods.test-itonami-surplus-ledger
    fuchi.methods.test-displacement-l0-path
    fuchi.methods.test-r2-execute
    fuchi.methods.test-pages-deploy
    fuchi.methods.test-mitsuho-receive
    fuchi.methods.test-hikari-receive
    fuchi.methods.test-mitsuho-produce-plan
    fuchi.methods.test-hikari-produce-plan
    fuchi.methods.test-care-iyashi-receive
    fuchi.methods.test-care-iyashi-produce-plan
    fuchi.methods.test-rail-housing-commons
    fuchi.methods.test-rail-tooling-okaimono
    fuchi.methods.test-rail-compute-murakumo
    fuchi.methods.test-rail-liquidity-warifu
    fuchi.methods.test-tooling-okaimono-receive
    fuchi.methods.test-tooling-okaimono-produce-plan
    fuchi.methods.test-compute-murakumo-receive
    fuchi.methods.test-compute-murakumo-produce-plan
    fuchi.methods.test-housing-commons-receive
    fuchi.methods.test-housing-commons-produce-plan
    fuchi.methods.test-liquidity-warifu-receive
    fuchi.methods.test-ss-offline-path
    fuchi.methods.test-rail-care-iyashi
    fuchi.methods.test-pages-publish])

(defn- require-ns! [sym]
  (try
    (require sym)
    {:ns sym :ok true}
    (catch :default e
      (println "WARN: require failed" (str sym) "—" (or (.-message e) e))
      {:ns sym :ok false :error e})))

(defn -main []
  (js/process.chdir ACTOR-DIR)
  (ensure-fuchi-symlink!)
  (let [repo-root (resolve-repo-root!)]
    (aset (.-env js/process) "FUCHI_ACTOR_DIR" ACTOR-DIR)
    (when repo-root
      (aset (.-env js/process) "FUCHI_REPO_ROOT" repo-root))
    ;; nbb resolves ns from cwd + fuchi/ symlink; -cp . if launched as `nbb -cp . run_tests.cljs`
    (println "→ nbb suite (in-process)" ACTOR-DIR)
    (let [req-results (mapv require-ns! SUITE)
          ok-ns (into [] (comp (filter :ok) (map :ns)) req-results)
          failed-req (into [] (remove :ok) req-results)]
      (when (seq failed-req)
        (println "require failures:" (count failed-req) "/" (count SUITE)))
      (if (empty? ok-ns)
        (do
          (println "ERROR: no test namespaces loaded under nbb")
          (.exit js/process 1))
        (let [r (apply t/run-tests ok-ns)
              fails (+ (or (:fail r) 0) (or (:error r) 0))
              req-fail (count failed-req)
              code (if (and (zero? fails) (zero? req-fail)) 0 1)]
          (when (pos? req-fail)
            (println "treated" req-fail "require failure(s) as suite failure"))
          (.exit js/process code))))))

(-main)
