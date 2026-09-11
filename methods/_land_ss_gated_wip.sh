#!/usr/bin/env kbb
;; OBSOLETE path (extension .sh). ADR-2607173000: script host is nbb only.
;; Use: kbb --backend sci methods/_land_ss_gated_wip.cljk
(println "ERROR: methods/_land_ss_gated_wip.sh is retired.")
(println "Use: kbb --backend sci methods/_land_ss_gated_wip.cljk")
(.exit js/process 1)
