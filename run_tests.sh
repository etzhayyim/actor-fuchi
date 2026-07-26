#!/usr/bin/env nbb
;; OBSOLETE path (extension .sh). ADR-2607173000: script host is nbb only.
;; Use: nbb run_tests.cljs
(println "ERROR: run_tests.sh is retired.")
(println "Use: nbb run_tests.cljs")
(.exit js/process 1)
