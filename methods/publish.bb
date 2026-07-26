#!/usr/bin/env nbb
;; OBSOLETE path (extension .bb). ADR-2607173000: script host is nbb only.
;; Use: nbb methods/publish.cljs
(println "ERROR: methods/publish.bb is retired.")
(println "Use: nbb methods/publish.cljs")
(.exit js/process 1)
