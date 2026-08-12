#!/usr/bin/env nbb
;; write_all.cljs — offline package regen entry (nbb only; ADR-2607173000).
;;
;; Runs displacement_pipeline/write-all!:
;;   scorecard + audit ledger + itonami surplus ledger + public/ deploy package
;; Then verifies public/priority-stack-offline.edn (1)(2)(3) SSoT facts.
;; Never deploys. cash≡0. live refuse. no personal scores.
;;
;; Usage (from actor-fuchi root):
;;   nbb -cp . methods/write_all.cljs
(def fs (js/require "node:fs"))
(def path (js/require "node:path"))

(def DIR
  (or (.-FUCHI_ACTOR_DIR (.-env js/process))
      (.resolve path (.dirname path *file*) "..")))

(defn- ensure-fuchi-symlink!
  []
  (let [link (.join path DIR "fuchi")]
    (when-not (.existsSync fs link)
      (.symlinkSync fs "." "fuchi" "dir")
      (println "created self-symlink fuchi/ → ."))))

(defn- fail! [msg data]
  (println "FAIL" msg (pr-str data))
  (.exit js/process 1))

(defn -main []
  (js/process.chdir DIR)
  (aset (.-env js/process) "FUCHI_ACTOR_DIR" DIR)
  (ensure-fuchi-symlink!)
  (println "→ write-all! offline package" DIR)
  (require 'fuchi.methods.displacement-pipeline)
  (require 'fuchi.methods.priority-stack)
  (let [write-all! (ns-resolve (find-ns 'fuchi.methods.displacement-pipeline) 'write-all!)
        assert-ps! (ns-resolve (find-ns 'fuchi.methods.priority-stack) 'assert-public-facts!)
        load-ps (ns-resolve (find-ns 'fuchi.methods.priority-stack) 'load-public-facts-file)
        out (write-all!)
        ps-path (.join path DIR "public" "priority-stack-offline.edn")
        out-ps (.join path DIR "out" "priority-stack-offline.edn")]
    (println "paths" (pr-str (:paths out)))
    (when-let [err (get-in out [:itonami-surplus-ledger :error])]
      (println "WARN surplus ledger:" err))
    (let [pub (or (:public-package out) (:public-pkg out) (:pages out))]
      (when-let [err (:error pub)]
        (println "WARN public package:" err))
      (when (true? (:live out))
        (fail! "live must be false" out))
      (when-not (zero? (long (or (:cash-usd-micros out) 0)))
        (fail! "cash≡0" out))
      (when-not (.existsSync fs ps-path)
        (fail! "missing public/priority-stack-offline.edn" ps-path))
      (when-not (.existsSync fs out-ps)
        (fail! "missing out/priority-stack-offline.edn" out-ps))
      (try
        (let [ps (load-ps ps-path)]
          (assert-ps! ps)
          (println "priority-stack-offline.edn OK"
                   "design-id=" (or (get ps :design-id) (get ps ":design-id"))
                   "L0=" (or (get ps :l0-stage) (get ps ":l0-stage"))
                   "mitsuho-gated=" (or (get ps :mitsuho-gated-phase)
                                       (get ps ":mitsuho-gated-phase"))))
        (catch :default e
          (fail! "priority-stack-offline.edn invalid" (or (.-message e) e))))
      (println "write-all! OK"
               "all-live-refused=" (get-in out [:scorecard :scorecard/all-live-refused])
               "deployed=" (boolean (:deployed pub)))
      (.exit js/process 0))))

(-main)
