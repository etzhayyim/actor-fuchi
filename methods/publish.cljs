#!/usr/bin/env nbb
;; fuchi self-publish — nbb host only (ADR-2607173000; no bash / no bb entry).
;; Prefer shared kototama publish.cljs when present; else offline write-pages!
;; (never live deploy).
(def fs (js/require "node:fs"))
(def path (js/require "node:path"))
(def cp (js/require "node:child_process"))

(def here (.dirname path *file*))
(def actor-root (.resolve path here ".."))
(def runtime-cljs
  (.resolve path actor-root ".." ".." "com-junkawasaki" "kototama" "lib" "actor" "publish.cljs"))

(defn- exists? [p] (.existsSync fs p))

(defn- spawn! [cmd args]
  (let [r (.spawnSync cp cmd (clj->js args)
                      #js {:stdio "inherit"
                           :cwd actor-root
                           :env (.-env js/process)})]
    (or (.-status r) 1)))

(defn -main [& args]
  (aset (.-env js/process) "FUCHI_ACTOR_DIR" actor-root)
  (if (exists? runtime-cljs)
    (.exit js/process
           (spawn! "nbb" (into [runtime-cljs "--actor" actor-root] args)))
    (do
      (println "→ offline write-pages! (no kototama publish.cljs; never deploys)")
      (let [r (spawn! "nbb" ["-cp" actor-root "-e"
                             "(require (quote fuchi.methods.pages-publish)) (fuchi.methods.pages-publish/write-pages!)"])]
        (.exit js/process r)))))

(apply -main *command-line-args*)
