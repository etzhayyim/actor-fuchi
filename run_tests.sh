#!/usr/bin/env nbb
;; OBSOLETE path (extension .sh). ADR-2607173000: script host is nbb only.
;; Use: nbb run_tests.cljs.cljk
(println "ERROR: run_tests.sh is retired.")
(println "Use: nbb run_tests.cljs.cljk")
(.exit js/process 1)
