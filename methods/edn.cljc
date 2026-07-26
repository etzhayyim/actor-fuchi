(ns fuchi.methods.edn
  "Minimal EDN reader (subset: [] {} :kw \"str\" num bool nil) — 1:1 Clojure port of
  fuchi `methods/_edn.py` (ADR-2606052300; itself ported from ake/noroshi).

  Keeps keywords as their \":ns/name\" STRINGS, NOT Clojure keywords — allocate/route/
  analyze key every seed record on string keys (\":maintainer/did\", \":envelope/line\",
  \":gov/ballots\", …), so the loader must yield the same string shape Python `load_edn`
  does, byte-for-byte. Stdlib only (regex tokenizer); file I/O at the #?(:clj) edge."
  (:require [clojure.string :as str]))

;; ── tokenizer (mirror of the Python _TOK regex) ───────────────────────────
(def ^:private token-re
  #"[\s,]+|;[^\n]*|(\[|\]|\{|\}|\"(?:\\.|[^\"\\])*\"|[^\s,\[\]{}]+)")

(defn- tokens
  "Lazy seq of significant tokens (capture group 1; whitespace/comments dropped)."
  [s]
  (let [m (re-matcher token-re s)]
    ((fn step []
       (lazy-seq
        (when (.find m)
          (let [t (.group m 1)]
            (if (nil? t) (step) (cons t (step))))))))))

(defn- unescape-string [t]
  (-> (subs t 1 (dec (count t)))
      (str/replace "\\\"" "\"")
      (str/replace "\\\\" "\\")))

(defn- parse-long* [^String t]
  #?(:clj (try (Long/parseLong t) (catch Exception _ nil))
     :cljs (let [n (js/parseInt t 10)] (when (and (not (js/isNaN n)) (re-matches #"[-+]?\d+" t)) n))))

(defn- parse-double* [^String t]
  #?(:clj (try (Double/parseDouble t) (catch Exception _ nil))
     :cljs (let [n (js/parseFloat t)] (when (and (not (js/isNaN n)) (re-matches #"[-+]?(\d+\.?\d*|\.\d+)([eE][-+]?\d+)?" t)) n))))

(defn- atom* [t]
  (cond
    (str/starts-with? t "\"") (unescape-string t)
    (= t "true")  true
    (= t "false") false
    (= t "nil")   nil
    (str/starts-with? t ":") t           ;; keyword kept as ":ns/name" STRING
    :else (or (parse-long* t) (parse-double* t) t)))

(def ^:private END ::end)

(defn- next-tok! [state]
  (let [ts @state]
    (when (empty? ts)
      (throw (ex-info "fuchi.methods.edn: unexpected end of input" {})))
    (reset! state (rest ts))
    (first ts)))

(defn- parse-form [state]
  (let [t (next-tok! state)]
    (cond
      (= t "[") (loop [out []]
                  (let [x (parse-form state)]
                    (if (= x END) out (recur (conj out x)))))
      (= t "{") (loop [out (array-map)]
                  (let [k (parse-form state)]
                    (if (= k END)
                      out
                      (let [v (parse-form state)]
                        (recur (assoc out k v))))))
      (or (= t "]") (= t "}")) END
      :else (atom* t))))

(defn parse-edn
  "Parse the first top-level EDN form (keywords as \":…\" strings; maps keep insertion order)."
  [s]
  (parse-form (atom (tokens s))))

#?(:clj
   (defn load-edn
     "Read + parse an EDN file (string or java.io.File). Clojure equivalent of `_edn.load_edn`."
     [path]
     (parse-edn (slurp (str path))))
   :cljs
   (defn load-edn
     "Read + parse an EDN file under nbb/cljs (node:fs). Same string-keyword shape as clj."
     [path]
     (let [fs (js/require "node:fs")
           p (str path)]
       (parse-edn (.readFileSync fs p "utf8")))))

(defn actor-dir
  "Resolve FUCHI_ACTOR_DIR or parent of methods/ (for load-design / seed paths)."
  []
  #?(:clj
     (or (System/getenv "FUCHI_ACTOR_DIR")
         (try
           (-> *file* java.io.File. .getParentFile .getParentFile .getCanonicalPath)
           (catch Exception _ ".")))
     :cljs
     (or (.-FUCHI_ACTOR_DIR (.-env js/process))
         (try
           (let [path (js/require "node:path")]
             (.resolve path (.dirname path *file*) ".."))
           (catch :default _ ".")))))

(defn data-file
  "Absolute path to data/<name> under actor-dir."
  [name]
  #?(:clj
     (str (java.io.File. (actor-dir) (str "data/" name)))
     :cljs
     (let [path (js/require "node:path")]
       (.join path (actor-dir) "data" name))))

(defn load-data
  "load-edn of data/<name> under actor-dir."
  [name]
  (load-edn (data-file name)))
