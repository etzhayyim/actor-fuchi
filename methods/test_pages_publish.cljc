(ns fuchi.methods.test-pages-publish
  "write-pages! package tests — portable under bb and nbb."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.pages-publish :as pages]
            [fuchi.methods.priority-stack :as pstack]
            [fuchi.methods.public-person :as pp]))

#?(:cljs
   (def ^:private fs (js/require "node:fs")))

(defn- path-exists? [p]
  #?(:clj (.exists (java.io.File. (str p)))
     :cljs (.existsSync fs (str p))))

(defn- read-text [p]
  #?(:clj (slurp (str p))
     :cljs (.readFileSync fs (str p) "utf8")))

(deftest test-write-pages-package
  (let [paths (pages/write-pages!)]
    (is (path-exists? (:index paths)))
    (is (path-exists? (:facts paths)))
    (is (path-exists? (:priority-stack-offline paths)))
    (is (false? (:deployed paths)))
    (is (false? (:live paths)))
    (is (= 0 (:cash-usd-micros paths)))
    (is (= [] (:score-surface paths)))
    (is (true? (:all-live-refused paths)))
    (is (path-exists? (:scorecard paths)))
    (is (path-exists? (:audit-summary paths)))
    (is (>= (:audit-runs paths) 0))
    (let [html (read-text (:index paths))
          readme (read-text "public/README.md")
          audit (read-string (read-text (:audit-summary paths)))
          ps (read-string (read-text (:priority-stack-offline paths)))]
      (is (true? (pstack/assert-public-facts! ps)))
      (is (str/includes? html "public surface"))
      (is (str/includes? html "Priority stack offline SSoT"))
      (is (str/includes? html "wellbecoming"))
      (is (str/includes? html "L0→L4"))
      (is (str/includes? html "SS scorecard"))
      (is (str/includes? html "Pipeline audit summary"))
      (is (str/includes? html "gov-post-ratify="))
      (is (str/includes? readme "priority-stack"))
      (is (str/includes? readme "L0 enroll disclosure"))
      (is (str/includes? readme "all-seven-rails"))
      (is (str/includes? readme "L0 all-seven membrane"))
      (is (str/includes? readme "L0 all-seven continuity/ladder"))
      (is (str/includes? readme "L0 held all-seven membrane"))
      (is (str/includes? readme "L0 exit→re-affirm"))
      (is (str/includes? readme "L0 falsehood→lift"))
      (is (str/includes? readme "L0 care-first+mitsuho"))
      (is (str/includes? readme "L0 care-first+hikari"))
      (is (str/includes? readme "L0 care-first+mitsuho+hikari"))
      (is (str/includes? readme "L0 care+housing both-refused"))
      (is (str/includes? readme "L0 multi-gen substrate"))
      (is (str/includes? readme "L0 full-inkind"))
      (is (str/includes? readme "L0 vocation recovery"))
      (is (str/includes? readme "L0 liquidity residual"))
      (is (str/includes? readme "L0 all-seven substrate"))
      (is (str/includes? readme "priority path catalog"))
      (is (str/includes? readme "rail DESIGN catalog"))
      (is (str/includes? readme "rail-design-catalog"))
      (is (str/includes? readme "live-produce never"))
      (is (str/includes? readme "SS priority path embeds all-seven design-public-facts"))
      (is (str/includes? readme "displacement L0 membranes"))
      (is (str/includes? readme "held-stress"))
      (is (str/includes? readme "gov + tenure-gov held-stress"))
      (is (str/includes? readme "all-inkind-produce-rails"))
      (is (str/includes? readme "out-of-band"))
      (is (true? (:ss-all-seven-design-embed paths)))
      (when (pos? (or (:runs audit) 0))
        (is (contains? audit :last-run-gov-post-ratify-committed-usd-micros))
        (is (contains? audit :last-run-displacement-held-stress-subjects))
        (is (contains? audit :last-run-gov-held-stress-subjects))
        (is (contains? audit :last-run-tenure-gov-held-stress-subjects))
        (is (contains? audit :last-run-housing-land-grant-executed))
        (is (false? (boolean (:any-land-grant-executed? audit))))
        (is (zero? (or (:last-run-housing-land-grant-executed audit) 0))))
      (is (false? (boolean (or (:live audit) false))))
      (is (zero? (or (:cash-usd-micros audit) 0)))
      (is (not (re-find #"(?i)\| *rank *\|" html))))))
