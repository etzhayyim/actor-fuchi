(ns fuchi.methods.displacement-l0-path
  "displacement_l0_path.cljc — offline path: itonami/robotics displacement → L0 enroll.

  When a funded Public-Fund earmark exists (G2), project representative displaced
  subjects into L0 enrollment, climb offline toward L4 (explicit 孫/子 multi-gen
  care: care/housing first + food/energy/tooling/compute), attach stage-aware dry
  floors, disclosure continuity tick, offline toritate/kanae booking (write_live
  refused). Optional member-principal liquidity residual (warifu; no produce plan;
  loan never invoked). R2 execute stays refused. Unfunded surplus → refused
  (no free-riding).

  Never cash. Never scores. Never live mint/dispatch. Portable .cljc."
  (:require [fuchi.methods.l0-enroll :as l0]
            [fuchi.methods.couple :as couple]
            [fuchi.methods.itonami-bridge :as itonami]
            [fuchi.methods.public-person :as pp]
            [fuchi.methods.disclosure-hold :as dh]
            [fuchi.methods.disclosure-continuity :as disc]
            [fuchi.methods.liberation-ladder :as ladder]
            [fuchi.methods.stage-sustenance :as stage]
            [fuchi.methods.displacement-book :as dbook]
            [fuchi.methods.displacement-couple :as dcouple]
            [fuchi.methods.rail-mitsuho :as mitsuho]
            [fuchi.methods.rail-hikari :as hikari]
            [fuchi.methods.rail-care-iyashi :as care]
            [fuchi.methods.rail-housing-commons :as housing]
            [fuchi.methods.rail-tooling-okaimono :as tooling]
            [fuchi.methods.rail-compute-murakumo :as compute]
            [fuchi.methods.rail-liquidity-warifu :as liquidity]
            [fuchi.methods.mitsuho-receive :as frecv]
            [fuchi.methods.mitsuho-produce-plan :as mprod]
            [fuchi.methods.hikari-receive :as hrecv]
            [fuchi.methods.hikari-produce-plan :as hprod]
            [fuchi.methods.care-iyashi-receive :as crecv]
            [fuchi.methods.care-iyashi-produce-plan :as cprod]
            [fuchi.methods.housing-commons-receive :as housrecv]
            [fuchi.methods.housing-commons-produce-plan :as housprod]
            [fuchi.methods.tooling-okaimono-receive :as trecv]
            [fuchi.methods.tooling-okaimono-produce-plan :as tprod]
            [fuchi.methods.compute-murakumo-receive :as mrecv]
            [fuchi.methods.compute-murakumo-produce-plan :as cmpprod]
            [fuchi.methods.liquidity-warifu-receive :as wrecv]
            [fuchi.methods.edn :as edn]))

(def PRIORITY-STACK pp/PRIORITY-STACK)

(def FRESH-DISC
  {:wage-labor-band "0-10h" :state-benefits? false
   :wellbecoming-attest-fact :submitted
   :related-party-edges [] :rider-s2-self-report :none})

(def STALE-DISC
  "Stale disclosure for priority-(2) held-stress (entitlements held; ladder refuse)."
  {:wage-labor-band :stale :state-benefits? false
   :wellbecoming-attest-fact :stale
   :related-party-edges [] :rider-s2-self-report :none})

;; Per-subject advisory floors within cohort earmark (illustrative offline).
;; L3: multi-gen substrate + vocation rails for robotics displacement recovery.
(def DEFAULT-FOOD-MICROS-YR 2000000000)
(def DEFAULT-CARE-MICROS-YR 1000000000)
(def DEFAULT-ENERGY-MICROS-YR 800000000)
(def DEFAULT-HOUSING-MICROS-YR 6000000000)
(def DEFAULT-TOOLING-MICROS-YR 500000000)
(def DEFAULT-COMPUTE-MICROS-YR 400000000)
;; Residual only (member-principal warifu); NOT charged against earmark slot budget.
(def DEFAULT-LIQUIDITY-MICROS-YR 1500000000)

(defn subject-did-for
  "Stable offline DID stub for a displaced worker slot in a cohort."
  [cohort-id slot]
  (str "did:web:etzhayyim.com:displaced:" cohort-id ":w" slot))

(defn plan-cohort-slots
  "How many L0 slots to open offline (capped; not a ranking score)."
  [event & {:keys [max-slots] :or {max-slots 5}}]
  (let [n (long (:displaced-count event 0))
        earmark (:earmark-usd-micros-yr (couple/earmark-from-surplus event))
        per (+ DEFAULT-FOOD-MICROS-YR DEFAULT-CARE-MICROS-YR
               DEFAULT-ENERGY-MICROS-YR DEFAULT-HOUSING-MICROS-YR
               DEFAULT-TOOLING-MICROS-YR DEFAULT-COMPUTE-MICROS-YR)
        by-budget (if (pos? per) (quot earmark per) 0)
        slots (min max-slots n (max 0 by-budget))]
    {:cohort-id (:cohort-id event)
     :displacing-actor (:displacing-actor event)
     :displaced-count n
     :earmark-usd-micros-yr earmark
     :slots slots
     :per-subject-floor-usd-micros-yr per
     :cash-usd-micros 0
     :live false
     :score-surface []}))

(defn enroll-displaced-subject
  "Offline L0 enroll → climb to target stage (default L4 multi-gen) → stage floors + disclosure.
   R2 execute remains default refuse. Priority (1) L0 + (2) disclosure + (3) rails R1→gated.
   Default also embeds priority-(2) held-stress (stale disclosure → ladder refuse; membranes refuse).
   cash≡0; no scores; live=false. Care/housing first at L4 (wellbecoming > 孫 > 子)."
  [{:keys [subject-did cohort-id displacing-actor food-imputed-usd-micros-yr
           care-imputed-usd-micros-yr energy-imputed-usd-micros-yr
           housing-imputed-usd-micros-yr tooling-imputed-usd-micros-yr
           compute-imputed-usd-micros-yr liquidity-imputed-usd-micros-yr
           vow-text member-signature climb-steps target-stage
           disclosure include-held-stress]
    :or {food-imputed-usd-micros-yr DEFAULT-FOOD-MICROS-YR
         care-imputed-usd-micros-yr DEFAULT-CARE-MICROS-YR
         energy-imputed-usd-micros-yr DEFAULT-ENERGY-MICROS-YR
         housing-imputed-usd-micros-yr DEFAULT-HOUSING-MICROS-YR
         tooling-imputed-usd-micros-yr DEFAULT-TOOLING-MICROS-YR
         compute-imputed-usd-micros-yr DEFAULT-COMPUTE-MICROS-YR
         liquidity-imputed-usd-micros-yr DEFAULT-LIQUIDITY-MICROS-YR
         climb-steps 4
         target-stage "L4"
         disclosure FRESH-DISC
         include-held-stress true}
    :as opts}]
  ;; climb-steps 4 → L4 multi-gen-care (wellbecoming > 孫 > 子); 3 = L3 vocation
  (let [sig (or member-signature (str "sig-displaced-" subject-did))
        disc-in (or disclosure FRESH-DISC)
        enrolled (l0/enroll {:subject-did subject-did
                             :vow-text (or vow-text
                                           (str "L0 after displacement cohort " cohort-id
                                                " by " displacing-actor
                                                " — multi-gen wellbecoming + vocation"))
                             :member-signature sig
                             :covenant "outreach"
                             :disclosure disc-in})
        person0 {:did subject-did
                 :covenant "vowed"
                 :rails (cond-> [{:kind "food" :active? true}
                                 {:kind "care" :active? true}
                                 {:kind "energy" :active? true}
                                 {:kind "housing" :active? true}
                                 {:kind "tooling" :active? true}
                                 {:kind "compute" :active? true}]
                          (pos? liquidity-imputed-usd-micros-yr)
                          (conj {:kind "liquidity" :active? true}))
                 ;; In-kind floors only in stage floor; liquidity residual is member-principal.
                 :floor-usd-micros-yr (+ food-imputed-usd-micros-yr
                                         care-imputed-usd-micros-yr
                                         energy-imputed-usd-micros-yr
                                         housing-imputed-usd-micros-yr
                                         tooling-imputed-usd-micros-yr
                                         compute-imputed-usd-micros-yr)
                 :disclosure disc-in
                 :exit-suspended? false
                 :stage "L0"
                 :cohort-id cohort-id
                 :displacement-source displacing-actor
                 :multi-gen-care-facts (or (:multi-gen-facts enrolled)
                                           ["wellbecoming-over-mago-over-ko"])
                 :cash-usd-micros 0}
        ;; Reuse L0 enroll hold (priority 1+2), then re-tick on rail-expanded person
        hold0 (or (:disclosure-hold enrolled) (dh/initial person0))
        cont (disc/tick hold0 person0
                        :disclosure disc-in
                        :reason "displacement-l0-after-enroll")
        hold (:machine cont)
        person1 (:person cont)
        climb (ladder/climb-offline person1 hold :steps climb-steps :member-signature sig)
        person (:person climb)
        stage (or (:stage person) "L0")
        stage-pkg (stage/build-for-stage
                   person hold
                   :imputed-overrides {"food" food-imputed-usd-micros-yr
                                       "care" care-imputed-usd-micros-yr
                                       "energy" energy-imputed-usd-micros-yr
                                       "housing" housing-imputed-usd-micros-yr
                                       "tooling" tooling-imputed-usd-micros-yr
                                       "compute" compute-imputed-usd-micros-yr})
        pkgs (:packages stage-pkg)
        booking (dbook/book-subject subject-did stage-pkg
                                    :alloc-id (str "disp-" cohort-id "-" subject-did))
        out {:path "displacement-l0"
             :subject-did subject-did
             :cohort-id cohort-id
             :displacing-actor displacing-actor
             :priority-stack PRIORITY-STACK
             :live false
             :cash-usd-micros 0
             :score-surface []
             :l0 enrolled
             :disclosure-hold hold
             :disclosure-continuity cont
             :disclosure-state (:state hold)
             :disclosure-held? (boolean (:entitlements-held? hold))
             :entitlements-may-flow? (disc/entitlements-may-flow? hold)
             :ladder climb
             :ladder-fact (ladder/ladder-public-fact person)
             :stage stage
             :target-stage target-stage
             :public-person (pp/public-surface person :stage stage)
             :stage-sustenance stage-pkg
             :stage-public (stage/public-floor-row stage-pkg)
             :food-package (get-in pkgs ["food" :package])
             :food-produce-plan (get-in pkgs ["food" :plan])
             :care-package (get-in pkgs ["care" :package])
             :care-produce-plan (get-in pkgs ["care" :plan])
             :energy-package (get-in pkgs ["energy" :package])
             :energy-produce-plan (get-in pkgs ["energy" :plan])
             :housing-package (get-in pkgs ["housing" :package])
             :housing-produce-plan (get-in pkgs ["housing" :plan])
             :tooling-package (get-in pkgs ["tooling" :package])
             :tooling-produce-plan (get-in pkgs ["tooling" :plan])
             :compute-package (get-in pkgs ["compute" :package])
             :compute-produce-plan (get-in pkgs ["compute" :plan])
             :r2-execute-status (or (get-in pkgs ["food" :r2])
                                    (get-in pkgs ["tooling" :r2])
                                    (get-in pkgs ["care" :r2]))
             :booking booking
             :booking-public (dbook/public-book-summary booking)}
        ;; member-principal residual (not from earmark; no produce plan; loan never invoked)
        liq-pkg (when (pos? liquidity-imputed-usd-micros-yr)
                  (liquidity/r1-dry-package
                   {:alloc-id (str "disp-liq-" cohort-id "-" subject-did)
                    :subject-did subject-did
                    :imputed-usd-micros-yr liquidity-imputed-usd-micros-yr
                    :person person
                    :hold-machine hold}))
        food-st (when-let [fp (:food-package out)]
                  (mitsuho/gated-live-status fp :hold-machine hold))
        energy-st (when-let [ep (:energy-package out)]
                    (hikari/gated-live-status ep :hold-machine hold))
        care-st (when-let [cp (:care-package out)]
                  (care/gated-live-status cp :hold-machine hold))
        hous-st (when-let [hp (:housing-package out)]
                  (housing/gated-live-status hp :hold-machine hold
                                             :council-housing-held? false))
        tool-st (when-let [tp (:tooling-package out)]
                  (tooling/gated-live-status tp :hold-machine hold))
        comp-st (when-let [cp (:compute-package out)]
                  (compute/gated-live-status cp :hold-machine hold))
        liq-st (when (and liq-pkg (not= :refused (:phase liq-pkg)))
                 (liquidity/gated-live-status liq-pkg :hold-machine hold))
        ;; R1→gated-receive/produce DESIGN (default refuse; no side-effects)
        food-recv (when-let [fp (:food-package out)]
                    (when (not= :refused (:phase fp)) (frecv/gated-receive-status fp)))
        food-prod (when-let [fp (:food-package out)]
                    (when (not= :refused (:phase fp)) (mprod/gated-produce-status fp)))
        energy-recv (when-let [ep (:energy-package out)]
                      (when (not= :refused (:phase ep)) (hrecv/gated-receive-status ep)))
        energy-prod (when-let [ep (:energy-package out)]
                      (when (not= :refused (:phase ep)) (hprod/gated-produce-status ep)))
        care-recv (when-let [cp (:care-package out)]
                    (when (not= :refused (:phase cp)) (crecv/gated-receive-status cp)))
        care-prod (when-let [cp (:care-package out)]
                    (when (not= :refused (:phase cp)) (cprod/gated-produce-status cp)))
        hous-recv (when-let [hp (:housing-package out)]
                    (when (not= :refused (:phase hp)) (housrecv/gated-receive-status hp)))
        hous-prod (when-let [hp (:housing-package out)]
                    (when (not= :refused (:phase hp)) (housprod/gated-produce-status hp)))
        tool-recv (when-let [tp (:tooling-package out)]
                    (when (not= :refused (:phase tp)) (trecv/gated-receive-status tp)))
        tool-prod (when-let [tp (:tooling-package out)]
                    (when (not= :refused (:phase tp)) (tprod/gated-produce-status tp)))
        comp-recv (when-let [cp (:compute-package out)]
                    (when (not= :refused (:phase cp)) (mrecv/gated-receive-status cp)))
        comp-prod (when-let [cp (:compute-package out)]
                    (when (not= :refused (:phase cp)) (cmpprod/gated-produce-status cp)))
        liq-recv (when (and liq-pkg (not= :refused (:phase liq-pkg)))
                   (wrecv/gated-receive-status liq-pkg))
        all-inkind-refused?
        (and (some? care-st) (some? hous-st) (some? food-st) (some? energy-st)
             (some? tool-st) (some? comp-st)
             (some? care-recv) (some? hous-recv) (some? food-recv) (some? energy-recv)
             (some? tool-recv) (some? comp-recv)
             (some? care-prod) (some? hous-prod) (some? food-prod) (some? energy-prod)
             (some? tool-prod) (some? comp-prod)
             (not (true? (:admissible care-st)))
             (not (true? (:admissible hous-st)))
             (not (true? (:admissible food-st)))
             (not (true? (:admissible energy-st)))
             (not (true? (:admissible tool-st)))
             (not (true? (:admissible comp-st)))
             (not (true? (:admissible care-recv)))
             (not (true? (:admissible hous-recv)))
             (not (true? (:admissible food-recv)))
             (not (true? (:admissible energy-recv)))
             (not (true? (:admissible tool-recv)))
             (not (true? (:admissible comp-recv)))
             (not (true? (:admissible care-prod)))
             (not (true? (:admissible hous-prod)))
             (not (true? (:admissible food-prod)))
             (not (true? (:admissible energy-prod)))
             (not (true? (:admissible tool-prod)))
             (not (true? (:admissible comp-prod))))
        liq-recv-full-chain-refused?
        (and (some? liq-st) (some? liq-recv)
             (not (true? (:admissible liq-st)))
             (not (true? (:admissible liq-recv))))
        membrane-summary
        {:food-gated-receive-admissible (boolean (:admissible food-recv))
         :food-gated-produce-admissible (boolean (:admissible food-prod))
         :energy-gated-receive-admissible (boolean (:admissible energy-recv))
         :energy-gated-produce-admissible (boolean (:admissible energy-prod))
         :care-gated-receive-admissible (boolean (:admissible care-recv))
         :care-gated-produce-admissible (boolean (:admissible care-prod))
         :housing-gated-receive-admissible (boolean (:admissible hous-recv))
         :housing-gated-produce-admissible (boolean (:admissible hous-prod))
         :tooling-gated-receive-admissible (boolean (:admissible tool-recv))
         :tooling-gated-produce-admissible (boolean (:admissible tool-prod))
         :compute-gated-receive-admissible (boolean (:admissible comp-recv))
         :compute-gated-produce-admissible (boolean (:admissible comp-prod))
         :liquidity-gated-receive-admissible (boolean (:admissible liq-recv))
         :liquidity-loan-executed false
         :liquidity-member-principal (boolean (:member-principal liq-pkg true))
         :liquidity-cash-usd-micros 0
         :liquidity-receive-full-chain-refused liq-recv-full-chain-refused?
         :mitsuho-hikari-full-chain-refused
         (and (some? food-st) (some? energy-st)
              (some? food-recv) (some? energy-recv)
              (some? food-prod) (some? energy-prod)
              (not (true? (:admissible food-st)))
              (not (true? (:admissible energy-st)))
              (not (true? (:admissible food-recv)))
              (not (true? (:admissible energy-recv)))
              (not (true? (:admissible food-prod)))
              (not (true? (:admissible energy-prod))))
         :care-housing-mitsuho-hikari-full-chain-refused
         (and (some? care-st) (some? hous-st) (some? food-st) (some? energy-st)
              (some? care-recv) (some? hous-recv) (some? food-recv) (some? energy-recv)
              (some? care-prod) (some? hous-prod) (some? food-prod) (some? energy-prod)
              (not (true? (:admissible care-st)))
              (not (true? (:admissible hous-st)))
              (not (true? (:admissible food-st)))
              (not (true? (:admissible energy-st)))
              (not (true? (:admissible care-recv)))
              (not (true? (:admissible hous-recv)))
              (not (true? (:admissible food-recv)))
              (not (true? (:admissible energy-recv)))
              (not (true? (:admissible care-prod)))
              (not (true? (:admissible hous-prod)))
              (not (true? (:admissible food-prod)))
              (not (true? (:admissible energy-prod))))
         :all-inkind-produce-rails-full-chain-refused all-inkind-refused?
         :all-seven-rails-receive-membrane-refused
         (boolean (and all-inkind-refused? liq-recv-full-chain-refused?))
         :live false
         :cash-usd-micros 0
         :score-surface []
         :priority-stack PRIORITY-STACK
         :note "displacement L0 subject membranes — default refuse; no live execute; liquidity residual member-principal only"}
        out (cond-> (assoc out :membrane-summary membrane-summary)
              food-st (assoc :food-gated-live-status food-st)
              energy-st (assoc :energy-gated-live-status energy-st)
              care-st (assoc :care-gated-live-status care-st)
              hous-st (assoc :housing-gated-live-status hous-st
                             :land-grant-executed false)
              tool-st (assoc :tooling-gated-live-status tool-st)
              comp-st (assoc :compute-gated-live-status comp-st)
              liq-pkg (assoc :liquidity-package liq-pkg
                             :liquidity-loan-executed false
                             :liquidity-cash-usd-micros 0)
              liq-st (assoc :liquidity-gated-live-status liq-st)
              liq-recv (assoc :liquidity-gated-receive-status liq-recv)
              food-recv (assoc :food-gated-receive-status food-recv)
              food-prod (assoc :food-gated-produce-status food-prod)
              energy-recv (assoc :energy-gated-receive-status energy-recv)
              energy-prod (assoc :energy-gated-produce-status energy-prod)
              care-recv (assoc :care-gated-receive-status care-recv)
              care-prod (assoc :care-gated-produce-status care-prod)
              hous-recv (assoc :housing-gated-receive-status hous-recv)
              hous-prod (assoc :housing-gated-produce-status hous-prod)
              tool-recv (assoc :tooling-gated-receive-status tool-recv)
              tool-prod (assoc :tooling-gated-produce-status tool-prod)
              comp-recv (assoc :compute-gated-receive-status comp-recv)
              comp-prod (assoc :compute-gated-produce-status comp-prod))
        ;; priority-(2) held-stress: stale disclosure → ladder refuse + membranes refuse
        held (when (not (false? include-held-stress))
               (enroll-displaced-subject
                (assoc opts
                       :include-held-stress false
                       :disclosure STALE-DISC
                       :member-signature sig)))
        held-stress
        (when held
          (let [lad-phase (get-in held [:ladder :phase])
                ms (:membrane-summary held)
                ;; When held keeps subject at L0, not all rail packages exist — treat
                ;; "no rail admissible + held" as full membrane refuse for smoke facts.
                no-rail-admissible?
                (not (some true?
                           [(:food-gated-produce-admissible ms)
                            (:energy-gated-produce-admissible ms)
                            (:care-gated-produce-admissible ms)
                            (:housing-gated-produce-admissible ms)
                            (:tooling-gated-produce-admissible ms)
                            (:compute-gated-produce-admissible ms)
                            (:food-gated-receive-admissible ms)
                            (:care-gated-receive-admissible ms)]))
                held-disc? (boolean (:disclosure-held? held))
                ladder-refused?
                (boolean (or (#{:stopped :refused} lad-phase) held-disc?))
                all-inkind-refused?
                (boolean (or (true? (:all-inkind-produce-rails-full-chain-refused ms))
                             (and held-disc? no-rail-admissible?)))
                care-housing-food-energy-refused?
                (boolean (or (true? (:care-housing-mitsuho-hikari-full-chain-refused ms))
                             (and held-disc? no-rail-admissible?)))
                hs {:disclosure-state (when-let [s (:disclosure-state held)]
                                        (if (keyword? s) (name s) (str s)))
                    :disclosure-held held-disc?
                    :entitlements-may-flow (boolean (:entitlements-may-flow? held))
                    :ladder-phase (when lad-phase (name lad-phase))
                    :ladder-advance-refused ladder-refused?
                    :stage (or (:stage held) "L0")
                    :all-inkind-produce-rails-full-chain-refused all-inkind-refused?
                    :care-housing-mitsuho-hikari-full-chain-refused care-housing-food-energy-refused?
                    :tooling-compute-both-refused
                    (boolean (and (not (true? (:tooling-gated-produce-admissible ms)))
                                  (not (true? (:compute-gated-produce-admissible ms)))))
                    :land-grant-executed false
                    :fulfillment-executed false
                    :quota-executed false
                    :liquidity-loan-executed false
                    :live false
                    :cash-usd-micros 0
                    :score-surface []
                    :priority-stack PRIORITY-STACK
                    :note "priority-2 held stress on displacement→L0 (itonami) — ladder refuse; membranes refuse; cash≡0"}]
            (pp/assert-no-public-scores! (dissoc hs :note))
            hs))
        out (cond-> out
              held-stress (assoc :held-stress held-stress
                                 :held-stress-ladder-refused
                                 (boolean (:ladder-advance-refused held-stress))
                                 :held-stress-all-inkind-refused
                                 (boolean (:all-inkind-produce-rails-full-chain-refused
                                           held-stress))))]
    (pp/assert-no-public-scores! (:public-person out))
    (pp/assert-no-public-scores! (dissoc membrane-summary :note))
    (when held-stress
      (pp/assert-no-public-scores! (dissoc held-stress :note :priority-stack)))
    (doseq [st [food-st energy-st care-st hous-st tool-st comp-st liq-st
                food-recv food-prod energy-recv energy-prod
                care-recv care-prod hous-recv hous-prod
                tool-recv tool-prod comp-recv comp-prod liq-recv]]
      (when st (pp/assert-no-public-scores! st)))
    (when liq-pkg (pp/assert-no-public-scores! liq-pkg))
    (pp/assert-no-public-scores!
     (dissoc out :public-person :membrane-summary :held-stress :l0 :disclosure-hold
             :disclosure-continuity :ladder :ladder-fact :stage-sustenance :stage-public
             :food-package :food-produce-plan :care-package :care-produce-plan
             :energy-package :energy-produce-plan :housing-package :housing-produce-plan
             :tooling-package :tooling-produce-plan :compute-package :compute-produce-plan
             :liquidity-package :booking :booking-public
             :food-gated-live-status :energy-gated-live-status :care-gated-live-status
             :housing-gated-live-status :tooling-gated-live-status :compute-gated-live-status
             :liquidity-gated-live-status :liquidity-gated-receive-status
             :food-gated-receive-status :food-gated-produce-status
             :energy-gated-receive-status :energy-gated-produce-status
             :care-gated-receive-status :care-gated-produce-status
             :housing-gated-receive-status :housing-gated-produce-status
             :tooling-gated-receive-status :tooling-gated-produce-status
             :compute-gated-receive-status :compute-gated-produce-status
             :r2-execute-status :priority-stack :note))
    out))

(defn run-for-event
  "One funded displacement event → slot plan + offline L0→L4 + book + G2 re-gate.
   If booked floors exceed earmark, phase becomes :refused-over-earmark (subjects retained
   as dry plan for diagnosis, not admissible)."
  [event & {:keys [max-slots climb-steps] :or {max-slots 5 climb-steps 4}}]
  (let [ear (couple/earmark-from-surplus event)
        gate0 (couple/coupling-gate event ear 0)]
    (if-not (true? (get gate0 "admissible"))
      {:path "displacement-l0"
       :cohort-id (:cohort-id event)
       :displacing-actor (:displacing-actor event)
       :phase :refused
       :refusal-reason (get gate0 "reason")
       :g2-gate gate0
       :live false
       :cash-usd-micros 0
       :score-surface []
       :priority-stack PRIORITY-STACK
       :subjects []
       :couple nil}
      (let [slots-plan (plan-cohort-slots event :max-slots max-slots)
            subjects
            (mapv
             (fn [i]
               (enroll-displaced-subject
                {:subject-did (subject-did-for (:cohort-id event) i)
                 :cohort-id (:cohort-id event)
                 :displacing-actor (:displacing-actor event)
                 :climb-steps climb-steps}))
             (range (:slots slots-plan)))
            couple-ev (dcouple/commit-offline-plan event ear subjects)
            ok? (true? (:admissible couple-ev))
            enrolled-subjects (if ok? subjects [])
            held-n (count (filter :held-stress enrolled-subjects))
            held-ladder-n (count (filter :held-stress-ladder-refused enrolled-subjects))]
        {:path "displacement-l0"
         :cohort-id (:cohort-id event)
         :displacing-actor (:displacing-actor event)
         :phase (if ok? :offline-enrolled :refused-over-earmark)
         :refusal-reason (when-not ok? (:reason couple-ev))
         :g2-gate gate0
         :g2-committed couple-ev
         :earmark ear
         :slots-plan slots-plan
         :subjects enrolled-subjects
         :subjects-dry (when-not ok? subjects)
         :held-stress-subjects held-n
         :held-stress-ladder-refused-subjects held-ladder-n
         :held-stress-all-subjects-refused
         (boolean (and (pos? (count enrolled-subjects))
                       (= held-ladder-n (count enrolled-subjects))))
         :couple couple-ev
         :live false
         :cash-usd-micros 0
         :score-surface []
         :priority-stack PRIORITY-STACK
         :note (if ok?
                 "offline L0→L4 multi-gen floors booked within earmark — held-stress embed; no live mint/execute/commit"
                 "booked floors exceed earmark — G2 refuse over-commit")}))))

(defn run-from-itonami-seed
  "All itonami seed events → displacement packages (admissible + refused)."
  [itonami-seed & {:keys [max-slots climb-steps] :or {max-slots 5 climb-steps 4}}]
  (let [events (if (sequential? itonami-seed)
                 (mapv itonami/itonami->couple-event itonami-seed)
                 (itonami/load-itonami-batch itonami-seed))
        packages (mapv #(run-for-event % :max-slots max-slots :climb-steps climb-steps) events)
        all-subs (mapcat :subjects packages)
        held-n (count (filter :held-stress all-subs))
        held-ladder-n (count (filter :held-stress-ladder-refused all-subs))
        out {:path "displacement-l0-batch"
             :live false
             :cash-usd-micros 0
             :score-surface []
             :priority-stack PRIORITY-STACK
             :packages packages
             :enrolled-subjects (count all-subs)
             :refused-cohorts (count (filter #(#{:refused :refused-over-earmark} (:phase %)) packages))
             :admissible-cohorts (count (filter #(= :offline-enrolled (:phase %)) packages))
             :stage-counts (frequencies (map :stage all-subs))
             :held-stress-subjects held-n
             :held-stress-ladder-refused-subjects held-ladder-n
             :held-stress-all-subjects-refused
             (boolean (and (pos? (count all-subs))
                           (= held-ladder-n (count all-subs))))
             :committed-usd-micros-yr
             (reduce + 0 (map #(or (get-in % [:couple :committed-usd-micros-yr]) 0)
                              (filter #(= :offline-enrolled (:phase %)) packages)))}]
    (doseq [p packages
            s (:subjects p)]
      (pp/assert-no-public-scores! (:public-person s))
      (when-let [hs (:held-stress s)]
        (pp/assert-no-public-scores! (dissoc hs :note :priority-stack))))
    out))

(defn run-default-seed
  "Load data/itonami-displacement-events.edn and run displacement→L4 offline path.
   Portable under bb and nbb."
  [& {:keys [max-slots climb-steps] :or {max-slots 3 climb-steps 4}}]
  (let [seed (edn/load-data "itonami-displacement-events.edn")]
    (run-from-itonami-seed seed :max-slots max-slots :climb-steps climb-steps)))
