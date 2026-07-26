#!/usr/bin/env nbb
;; _land_ss_gated_wip.cljs — one-shot land when terminal is available.
;; DELETE after successful push (WIP land helper, not permanent product code).
;;
;; Usage (from com-etzhayyim-fuchi root):
;;   nbb methods/_land_ss_gated_wip.cljs
;;
;; ADR-2607173000: nbb host only (no bash land script).
;; cash≡0 · live refuse · no force-push · no live deploy.
(require '[clojure.string :as str])

(def fs (js/require "node:fs"))
(def path (js/require "node:path"))
(def cp (js/require "node:child_process"))

(def DIR
  (or (.-FUCHI_ACTOR_DIR (.-env js/process))
      (.resolve path (.dirname path *file*) "..")))

(def ROOT
  (or (.-FLEET_ROOT (.-env js/process))
      (.resolve path DIR ".." ".." "..")))

(def COMMIT-MSG
  (str "feat(ss): priority_stack SSoT care-housing→mitsuho+hikari→all-seven\n"
       "\n"
       "Priority offline stack for robotics/itonami × etzhayyim covenantal SS:\n"
       "(1) L0 enroll offline scaffold\n"
       "(2) disclosure hold + continuity SM + held-stress\n"
       "(3) care-housing multi-gen → mitsuho+hikari R1→gated DESIGN\n"
       "    → all-seven substrate capstone (孫/子 first)\n"
       "ss_offline_path embeds priority-stack-offline public-facts\n"
       "nbb.edn + dual classpath (methods/ and src/fuchi/)\n"
       "cash≡0; live refuse; loan/land-grant never; no personal scores\n"))

(defn- exists? [p] (.existsSync fs (str p)))

(defn- run
  "node:child_process spawnSync; returns {:exit :out :err}."
  ([args] (run args nil))
  ([args opts]
   (let [base {:encoding "utf8" :stdio "inherit" :cwd DIR}
         o (clj->js (merge base (or opts {})))
         r (.spawnSync cp (first args) (to-array (rest args)) o)
         status (.-status r)
         signal (.-signal r)]
     {:exit (if (some? status) status 1)
      :out (or (.-stdout r) "")
      :err (or (.-stderr r) "")
      :signal signal})))

(defn- run!
  "Like run but throw on non-zero exit."
  [args & {:as opts}]
  (let [r (run args opts)]
    (when-not (zero? (:exit r))
      (throw (js/Error. (str "command failed exit=" (:exit r)
                             " args=" (pr-str args)
                             (when (:signal r) (str " signal=" (:signal r)))))))
    r))

(defn- run-capture
  "spawnSync capturing stdout (stdio pipe)."
  [args & {:as opts}]
  (let [r (run args (merge {:stdio "pipe"} opts))]
    (when-not (zero? (:exit r))
      (throw (js/Error. (str "command failed exit=" (:exit r)
                             " args=" (pr-str args)
                             " err=" (:err r)))))
    (str/trim (str (:out r)))))

(defn- ensure-fuchi-symlink!
  "nbb -cp . and run_tests need fuchi/ → . for ns fuchi.methods.*"
  []
  (let [link (.join path DIR "fuchi")]
    (when-not (exists? link)
      (.symlinkSync fs "." "fuchi" "dir")
      (println "created self-symlink fuchi/ → ."))))

(defn- step-status!
  []
  (println "OK" DIR)
  (run! ["git" "status" "-sb"])
  (run! ["git" "log" "-2" "--oneline"]))

(defn- step-rg-smoke!
  "Optional: confirm receive membranes still expose gated-receive-status."
  []
  (let [files ["methods/tooling_okaimono_receive.cljc"
               "methods/compute_murakumo_receive.cljc"
               "methods/liquidity_warifu_receive.cljc"]
        has-rg? (try
                  (zero? (:exit (run ["rg" "--version"] {:stdio "pipe"})))
                  (catch :default _ false))]
    (if has-rg?
      (run! (into ["rg" "-n" "gated-receive-status"] files))
      (println "skip rg smoke (rg not found)"))))

(defn- step-priority-stack-smoke!
  "nbb pure smoke for priorities (1) L0 (2) disclosure (3) mitsuho R1→gated.
   Also runs readiness_check (design + static + readiness layer) first when present."
  []
  (let [ready (.join path DIR "methods/readiness_check.cljs")
        smoke (.join path DIR "methods/priority_stack_smoke.cljs")]
    (when (exists? ready)
      (println "→ nbb -cp . methods/readiness_check.cljs")
      (run! ["nbb" "-cp" "." ready]))
    (when-not (exists? smoke)
      (println "skip priority_stack_smoke (missing)")
      (throw (js/Error. (str "missing " smoke))))
    (println "→ nbb -cp . methods/priority_stack_smoke.cljs")
    (run! ["nbb" "-cp" "." smoke])))

(defn- step-tests!
  []
  (let [suite (.join path DIR "run_tests.cljs")]
    (when-not (exists? suite)
      (throw (js/Error. (str "missing " suite))))
    (println "→ nbb -cp . run_tests.cljs")
    (run! ["nbb" "-cp" "." suite])))

(defn- step-regen-package!
  "Prefer methods/write_all.cljs (nbb entry) → write-all! → deploy package → pages."
  []
  (ensure-fuchi-symlink!)
  (let [write-all-js (.join path DIR "methods/write_all.cljs")
        form-all "(require (quote fuchi.methods.displacement-pipeline)) (fuchi.methods.displacement-pipeline/write-all!)"
        form-deploy "(require (quote fuchi.methods.pages-deploy)) (fuchi.methods.pages-deploy/write-deploy-package!)"
        form-pages "(require (quote fuchi.methods.pages-publish)) (fuchi.methods.pages-publish/write-pages!)"]
    (println "→ nbb -cp . methods/write_all.cljs")
    (let [r0 (if (exists? write-all-js)
               (run ["nbb" "-cp" "." write-all-js])
               {:exit 1})]
      (if (zero? (:exit r0))
        (println "write_all.cljs ok (scorecard+audit+surplus+public)")
        (do
          (println "write_all.cljs exit=" (:exit r0) "→ nbb -e write-all!")
          (let [r (run ["nbb" "-cp" "." "-e" form-all])]
            (if (zero? (:exit r))
              (println "write-all! ok")
              (do
                (println "write-all! failed exit=" (:exit r) "→ try write-deploy-package!")
                (let [r2 (run ["nbb" "-cp" "." "-e" form-deploy])]
                  (if (zero? (:exit r2))
                    (println "deploy package ok")
                    (do
                      (println "write-deploy-package! failed exit=" (:exit r2) "→ write-pages!")
                      (let [r3 (run ["nbb" "-cp" "." "-e" form-pages])]
                        (if (zero? (:exit r3))
                          (println "pages package ok")
                          (println "WARN: package regen failed (continuing land) exit=" (:exit r3)))))))))))))))

(defn- step-commit!
  []
  (println "→ git add + commit")
  (run! ["git" "add" "methods/" "public/" "data/" "out/" "README.md" "MATURITY.md" "CLAUDE.md"
         "run_tests.cljs" "data/priority-stack-design.edn"
         "public/priority-stack-offline.edn" "out/priority-stack-offline.edn"
         "methods/readiness_check.cljs"])
  (when (exists? (.join path DIR "data/rail-design-catalog.md"))
    (run ["git" "add" "data/rail-design-catalog.md"]))
  ;; stage land helper only if present (deleted after push)
  (when (exists? (.join path DIR "methods/_land_ss_gated_wip.cljs"))
    (run ["git" "add" "methods/_land_ss_gated_wip.cljs"]))
  ;; retire obsolete extension paths if still present
  (doseq [obs ["methods/_land_ss_gated_wip.sh" "run_tests.sh" "methods/publish.bb"
               "methods/init.clj"]]
    (let [p (.join path DIR obs)]
      (when (exists? p)
        (try (.unlinkSync fs p) (catch :default _))
        (run ["git" "rm" "-f" "--ignore-unmatch" obs]))))
  (run! ["git" "status" "-sb"])
  (let [r (run ["git" "commit" "-m" COMMIT-MSG])]
    (if (zero? (:exit r))
      (println "committed")
      (println "git commit exit=" (:exit r) "(maybe nothing to commit)"))))

(defn- step-sync-push!
  []
  (println "→ fetch/merge/push main (ff-only, no force)")
  (let [remotes (try (str/split-lines (run-capture ["git" "remote"]))
                     (catch :default _ []))
        remote (cond
                 (some #{"etzhayyim"} remotes) "etzhayyim"
                 (some #{"origin"} remotes) "origin"
                 :else (first remotes))]
    (when-not remote
      (throw (js/Error. "no git remote configured")))
    (println "remote:" remote)
    (run! ["git" "fetch" remote "main"])
    (run! ["git" "merge" "--ff-only" (str remote "/main")])
    (run! ["git" "push" remote "main"])
    (let [sha (run-capture ["git" "rev-parse" "HEAD"])]
      (println "FUCHI_SHA=" sha)
      {:remote remote :sha sha})))

(defn- step-cleanup-self!
  "Remove land helper after successful push (nbb cljs only; no .sh)."
  []
  (let [f "methods/_land_ss_gated_wip.cljs"
        p (.join path DIR f)]
    (when (exists? p)
      (.unlinkSync fs p)
      (println "removed" f))))

(defn- step-west-pin!
  [sha]
  (let [gen (.join path ROOT "scripts/gen-west-manifest.cljs")]
    (if (exists? gen)
      (do
        (println "→ west pin gen --entry com-etzhayyim-fuchi")
        (run! ["nbb" gen "--entry" "com-etzhayyim-fuchi"] {:cwd ROOT})
        (let [chk (run ["nbb" gen "--check"] {:cwd ROOT})]
          (when-not (zero? (:exit chk))
            (println "WARN: gen-west-manifest --check exit=" (:exit chk))))
        (println "If gen wrote west.yml, commit on root main with:")
        (println "  chore(west): pin com-etzhayyim-fuchi → itonami offline held-stress E2E"))
      (do
        (println "Manual west pin: set com-etzhayyim-fuchi revision to" sha)
        (println "Message: chore(west): pin com-etzhayyim-fuchi → itonami offline held-stress E2E")))))

(defn -main []
  (js/process.chdir DIR)
  (aset (.-env js/process) "FUCHI_ACTOR_DIR" DIR)
  (ensure-fuchi-symlink!)
  (step-status!)
  (step-rg-smoke!)
  (step-priority-stack-smoke!)
  (step-tests!)
  (step-regen-package!)
  (step-commit!)
  (let [{:keys [sha]} (step-sync-push!)]
    (step-cleanup-self!)
    (step-west-pin! sha)
    (println "land complete" sha)))

(-main)
