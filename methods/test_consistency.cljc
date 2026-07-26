(ns fuchi.methods.test-consistency
  "SSoT drift-lock tests for 扶持 (fuchi): manifest ↔ files ↔ ontology ↔ seed.
  Portable under nbb and bb (ADR-2607173000)."
  (:require [clojure.test :refer [deftest is]]
            [clojure.string :as str]
            [fuchi.methods.edn :as edn]))

#?(:cljs
   (def ^:private fs (js/require "node:fs")))
#?(:cljs
   (def ^:private path (js/require "node:path")))

;; ── minimal JSON reader (subset sufficient for manifest.jsonld) ───────────────
(declare json-value)

(defn- skip-ws [^String s i]
  (loop [i i]
    (if (and (< i (count s)) (contains? #{\space \tab \newline \return} (nth s i)))
      (recur (inc i)) i)))

(defn- json-string [^String s i]
  (loop [i (inc i), acc ""]
    (let [c (nth s i)]
      (cond
        (= c \") [acc (inc i)]
        (= c \\)
        (let [e (nth s (inc i))]
          (case e
            \" (recur (+ i 2) (str acc \"))
            \\ (recur (+ i 2) (str acc \\))
            \/ (recur (+ i 2) (str acc \/))
            \b (recur (+ i 2) (str acc \backspace))
            \f (recur (+ i 2) (str acc \formfeed))
            \n (recur (+ i 2) (str acc \newline))
            \r (recur (+ i 2) (str acc \return))
            \t (recur (+ i 2) (str acc \tab))
            \u (let [hex (subs s (+ i 2) (+ i 6))
                     cp #?(:clj (Integer/parseInt hex 16)
                           :cljs (js/parseInt hex 16))
                     ch #?(:clj (str (char cp))
                           :cljs (.fromCharCode js/String cp))]
                 (recur (+ i 6) (str acc ch)))
            (recur (+ i 2) (str acc e))))
        :else (recur (inc i) (str acc c))))))

(defn- parse-num-token [tok]
  (if (some #{\. \e \E} tok)
    #?(:clj (Double/parseDouble tok)
       :cljs (js/parseFloat tok))
    #?(:clj (Long/parseLong tok)
       :cljs (js/parseInt tok 10))))

(defn- json-number [^String s i]
  (let [end (loop [j i]
              (if (and (< j (count s))
                       (contains? #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9 \+ \- \. \e \E} (nth s j)))
                (recur (inc j)) j))
        tok (subs s i end)]
    [(parse-num-token tok) end]))

(defn- json-array [^String s i]
  (loop [i (skip-ws s (inc i)), out []]
    (if (= (nth s i) \])
      [out (inc i)]
      (let [[v i] (json-value s i)
            i (skip-ws s i)]
        (if (= (nth s i) \,)
          (recur (skip-ws s (inc i)) (conj out v))
          [(conj out v) (inc i)])))))

(defn- json-object [^String s i]
  (loop [i (skip-ws s (inc i)), out {}]
    (if (= (nth s i) \})
      [out (inc i)]
      (let [[k i] (json-string s i)
            i (skip-ws s i)
            [v i] (json-value s (skip-ws s (inc i)))
            out (assoc out k v)
            i (skip-ws s i)]
        (if (= (nth s i) \,)
          (recur (skip-ws s (inc i)) out)
          [out (inc i)])))))

(defn- json-value [^String s i]
  (let [i (skip-ws s i), c (nth s i)]
    (cond
      (= c \{) (json-object s i)
      (= c \[) (json-array s i)
      (= c \") (json-string s i)
      (= c \t) [true (+ i 4)]
      (= c \f) [false (+ i 5)]
      (= c \n) [nil (+ i 4)]
      :else (json-number s i))))

(defn- parse-json [text] (first (json-value text 0)))

;; ── fixture locators ─────────────────────────────────────────────────────────
(defn- actor-root
  []
  (or (edn/actor-dir) "."))

(defn- path-exists? [p]
  #?(:clj (.exists (java.io.File. (str p)))
     :cljs (.existsSync fs (str p))))

(defn- read-text [p]
  #?(:clj (slurp (str p))
     :cljs (.readFileSync fs (str p) "utf8")))

(defn- list-dir-names [dir]
  #?(:clj
     (mapv #(.getName %)
           (filter #(.isDirectory %)
                   (or (.listFiles (java.io.File. (str dir))) (into-array java.io.File []))))
     :cljs
     (let [entries (seq (.readdirSync fs (str dir) #js {:withFileTypes true}))]
       (mapv #(.-name %) (filter #(.isDirectory %) entries)))))

(defn- list-edn-files [dir]
  #?(:clj
     (mapv str
           (filter #(str/ends-with? (.getName %) ".edn")
                   (or (.listFiles (java.io.File. (str dir))) (into-array java.io.File []))))
     :cljs
     (mapv #(.join path (str dir) %)
           (filter #(str/ends-with? % ".edn")
                   (seq (.readdirSync fs (str dir)))))))

(defn- actor-join*
  "Join under actor-root."
  [& parts]
  #?(:clj (str (reduce (fn [f p] (java.io.File. f (str p)))
                       (java.io.File. (actor-root))
                       parts))
     :cljs (apply (.-join path) (cons (actor-root) parts))))

(defn- resolve-join
  "Join absolute-ish root with parts."
  [root & parts]
  #?(:clj (str (reduce (fn [f p] (java.io.File. f (str p)))
                       (java.io.File. (str root))
                       parts))
     :cljs (apply (.-join path) (cons (str root) parts))))

(defn- repo-root
  "etzhayyim/root (ontology schemas) or FUCHI_REPO_ROOT."
  []
  (or #?(:clj (System/getenv "FUCHI_REPO_ROOT")
         :cljs (.-FUCHI_REPO_ROOT (.-env js/process)))
      (let [cands [(resolve-join (actor-root) ".." "root")
                   (resolve-join (actor-root) ".." ".." "etzhayyim" "root")]]
        (some (fn [r]
                (when (path-exists? (resolve-join r "00-contracts" "schemas"))
                  r))
              cands))))

(defn- schema-path []
  (when-let [root (repo-root)]
    (resolve-join root "00-contracts" "schemas"
                  "maintainer-sustenance-ontology.kotoba.edn")))

(defn- manifest-path []
  (actor-join* "manifest.jsonld"))

(defn- seed-path []
  (edn/data-file "seed-sustenance-graph.kotoba.edn"))

(defn- manifest []
  (parse-json (read-text (manifest-path))))

;; ── tests ─────────────────────────────────────────────────────────────────
(deftest test-manifest-cells-match-cell-dirs
  (let [declared (set (map #(get % "name") (get (manifest) "cells")))
        cells-dir (actor-join* "cells")
        dirs (set (remove #(str/starts-with? % "__") (list-dir-names cells-dir)))]
    (is (= declared dirs) (str "manifest " declared " != dirs " dirs))))

(deftest test-manifest-lexicons-match-lex-files
  (let [declared (set (map #(get % "id") (get (manifest) "lexiconNamespaces")))
        lex-dir (actor-join* "lex")
        files (set (map #(get (edn/load-edn %) ":id") (list-edn-files lex-dir)))]
    (is (= declared files) (str "manifest " declared " != files " files))))

(deftest test-manifest-adr-matches-ontology
  (let [sp (schema-path)]
    (is (some? sp) "schema path must resolve (set FUCHI_REPO_ROOT or etzhayyim/root)")
    (when sp
      (let [onto (edn/load-edn sp)
            adr (get onto ":ontology/adr")]
        (is (or (= adr "2606052300")
                (and (sequential? adr) (some #{"2606052300" "2607177000"} (map str adr))))
            (str "unexpected :ontology/adr " adr))
        (is (str/ends-with? (get-in (manifest) ["adr" "master"]) "2606052300"))))))

(deftest test-manifest-schema-pointer-exists
  (let [root (repo-root)
        rel (str/replace (get-in (manifest) ["references" "schema"]) #"^/" "")
        p (when root (resolve-join root rel))]
    (is (some? root) "repo root for ontology")
    (is (and p (path-exists? p)) (str p))))

(deftest test-every-seed-maintainer-has-envelope-or-is-excluded
  (let [seed (edn/load-edn (seed-path))
        env-dids (set (map #(get % ":envelope/maintainer") (get seed ":envelope/batch")))]
    (doseq [m (get seed ":maintainer/batch")]
      (is (contains? env-dids (get m ":maintainer/did")) (get m ":maintainer/did")))))

(deftest test-seed-cash-is-zero-everywhere
  (let [seed (edn/load-edn (seed-path))]
    (doseq [e (get seed ":envelope/batch")]
      (is (= (get e ":envelope/cash-usd-micros") 0)))))

(deftest test-seed-no-maintainer-owns-payoff
  (let [seed (edn/load-edn (seed-path))]
    (doseq [m (get seed ":maintainer/batch")]
      (is (false? (get m ":maintainer/owns-payoff" false))))))

(deftest test-seed-covenants-are-valid-vocab
  (let [sp (schema-path)]
    (is (some? sp))
    (when sp
      (let [seed (edn/load-edn (seed-path))
            vocab (set (get (edn/load-edn sp) ":ontology/covenants"))]
        (doseq [m (get seed ":maintainer/batch")]
          (is (contains? vocab (get m ":maintainer/covenant"))))))))

(deftest test-seed-envelope-lines-are-valid-vocab
  (let [sp (schema-path)]
    (is (some? sp))
    (when sp
      (let [seed (edn/load-edn (seed-path))
            vocab (set (get (edn/load-edn sp) ":ontology/envelope-lines"))]
        (doseq [e (get seed ":envelope/batch")]
          (is (contains? vocab (get e ":envelope/line"))))))))
