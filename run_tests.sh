#!/usr/bin/env kbb
;; OBSOLETE path (extension .sh). ADR-2607173000: script host is nbb only.
;; Use: kbb --backend sci run_tests.cljs.cljk
(println "ERROR: run_tests.sh is retired.")
(println "Use: kbb --backend sci run_tests.cljs.cljk")
(.exit js/process 1)
