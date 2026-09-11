#!/usr/bin/env kbb
;; OBSOLETE path (extension .bb). ADR-2607173000: script host is nbb only.
;; Use: kbb --backend sci methods/publish.cljk
(println "ERROR: methods/publish.bb is retired.")
(println "Use: kbb --backend sci methods/publish.cljk")
(.exit js/process 1)
