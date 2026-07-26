#!/usr/bin/env nbb
;; readiness_check.cljs — offline readiness smoke for priority stack SSoT.
;;
;; Does NOT run full pipeline or deploy. Verifies:
;;   (1) data/priority-stack-design.edn invariants
;;   (2) priority_stack/run-offline → public-facts
;;   (3) public/ + out/ priority-stack-offline.edn (if present)
;;   (4) data/itonami-offline-ss-readiness.edn layer :priority-stack-ssot
;;
;; Usage (from com-etzhayyim-fuchi root):
;;   nbb -cp . methods/readiness_check.cljs
;;
;; cash≡0 · live refuse · no scores · ADR-2607173000 nbb only.
(require '[clojure.string :as str])

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

(defn- ok! [step]
  (println "OK" step))

(defn -main []
  (js/process.chdir DIR)
  (aset (.-env js/process) "FUCHI_ACTOR_DIR" DIR)
  (ensure-fuchi-symlink!)
  (require 'fuchi.methods.priority-stack)
  (require 'fuchi.methods.edn)
  (let [ps-ns (find-ns 'fuchi.methods.priority-stack)
        design-inv (ns-resolve ps-ns 'design-edn-invariants)
        paths-inv (ns-resolve ps-ns 'l0-paths-design-invariants)
        run-offline (ns-resolve ps-ns 'run-offline)
        public-facts (ns-resolve ps-ns 'public-facts)
        assert-ps! (ns-resolve ps-ns 'assert-public-facts!)
        load-ps (ns-resolve ps-ns 'load-public-facts-file)
        load-data (ns-resolve (find-ns 'fuchi.methods.edn) 'load-data)]
    ;; (0) design EDN + L0 path catalog
    (try
      (let [inv (design-inv)]
        (when-not (= "fuchi.priority-stack-offline" (:design-id inv))
          (fail! "design-id" inv))
        (when-not (= [1 2 3] (:priority-order-ns inv))
          (fail! "design order" inv))
        (ok! "priority-stack-design.edn invariants"))
      (catch :default e
        (fail! "design-edn-invariants" (or (.-message e) e))))
    (try
      (let [pinv (paths-inv)]
        (when-not (= 9 (:path-count pinv))
          (fail! "l0-paths count" pinv))
        (when-not (true? (:all-paths-held-stress-embed pinv))
          (fail! "l0-paths held-stress" pinv))
        (ok! "l0-offline-priority-paths-design.edn invariants"))
      (catch :default e
        (fail! "l0-paths-design-invariants" (or (.-message e) e))))
    ;; (1)(2)(3) live offline run — care-housing → mitsuho+hikari
    (try
      (let [sum (run-offline {})
            facts (public-facts sum)]
        (assert-ps! facts)
        (when-not (true? (:care-housing-both-refused facts))
          (fail! "care-housing both refuse" facts))
        (when-not (= "care-first-hikari-path" (:hikari-care-first-api-path facts))
          (fail! "hikari care-first" facts))
        (when-not (true? (:all-seven-membrane-refused facts))
          (fail! "all-seven membrane" facts))
        (when (true? (:all-seven-loan-executed facts))
          (fail! "all-seven loan never" facts))
        (ok! "run-offline + assert-public-facts! (care-housing→mitsuho+hikari→all-seven)"))
      (catch :default e
        (fail! "run-offline" (or (.-message e) e))))
    ;; static surfaces
    (doseq [rel ["public/priority-stack-offline.edn" "out/priority-stack-offline.edn"]]
      (let [p (.join path DIR rel)]
        (if (.existsSync fs p)
          (try
            (assert-ps! (load-ps p))
            (ok! rel)
            (catch :default e
              (fail! rel (or (.-message e) e))))
          (println "WARN missing" rel "(run write_all.cljs)"))))
    ;; readiness machine map
    (try
      (let [r (load-data "itonami-offline-ss-readiness.edn")
            layers (or (get r ":readiness/layers") (get r :readiness/layers) [])
            ids (set (map str (map #(or (get % ":id") (get % :id)) layers)))
            status (or (get r ":readiness/status") (get r :readiness/status))
            has-ps? (some #(str/includes? % "priority-stack-ssot") ids)]
        (when-not has-ps?
          (fail! "readiness missing :priority-stack-ssot layer" ids))
        (when-not (str/includes? (str status) "code-complete-unlanded")
          (fail! "unexpected readiness status" status))
        (ok! (str "itonami-offline-ss-readiness.edn status=" status)))
      (catch :default e
        (fail! "readiness edn" (or (.-message e) e))))
    (println "readiness-check ALL GREEN")
    (println "priority-stack SSoT (1)(2)(3) · cash≡0 · live refuse · no scores")
    (.exit js/process 0)))

(-main)
