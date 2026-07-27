# 扶持 (fuchi) — maintainer sustenance allocator

The **charter-clean inverse of a business investment fund**. Where a VC fund invests capital in
founders expecting equity + ROI + an exit, **扶持** (the feudal **扶持米** — the in-kind rice
stipend that sustained a retainer so they could serve) allocates **in-kind sustenance +
commons-asset access + tooling/compute** to the real-world **maintainers (信者)** who keep
etzhayyim's actors alive (business / robotics / remote-control).

It is a **redistribution / sustenance allocator, never an investor**: no equity, no ROI, no debt,
no profit claim, no exit. **cash≡0.** The whole fund vocabulary (NAV / carry / IRR / cap-table /
exit / dividend) is *unrepresentable* — `:alloc/instrument` is `:db/allowed` only the sustenance
set, exactly as nusa's `:psychoactive`, tazuna's `:weaponizable`, and kamado's
`:fossil-virgin-crude` are unrepresentable.

Per **ADR-2606052300** + **ADR-2607177000** (public-person as-of, wellbecoming/mago/ko priority).

## Priority & public person

| Rule | Meaning |
|---|---|
| **P0–P2** | wellbecoming → 孫 → 子 (present recipient is subordinate) |
| **public-person?** | as-of: covenant ∧ active SS rails ∧ ¬exit-suspended |
| **PUBLIC** | identity, rails, imputed **facts**, disclosure status |
| **SCORE** | unrepresentable (no personal rank/leaderboard on public surface) |
| **INTERNAL** | tenure weight / priority-rank for rationing only |

Machine-readable SSoT: [`data/public-person-dynamic.edn`](data/public-person-dynamic.edn).  
Implementation: [`methods/public_person.cljc`](methods/public_person.cljc).  
Disclosure lexicon: [`lex/disclosureAttestation.edn`](lex/disclosureAttestation.edn)  
Seed packages: `:disclosure/batch` in [`data/seed-sustenance-graph.kotoba.edn`](data/seed-sustenance-graph.kotoba.edn)  
(stale package → `disclosure-gate :hold` while `public-person?` stays true).

### Offline priority path (robotics/itonami SS scaffold)

Priority order for covenantal SS offline work:

1. **L0 enroll** — draft vow → triple CID stubs → L0 entitlement → public-person facts  
2. **Disclosure hold + continuity** — stale/falsehood → hold entitlements (public-person may remain)  
3. **Rail R1 → gated DESIGN** — care/housing first (孫/子), then food/energy, tooling/compute, liquidity residual  

| Module | Role |
|---|---|
| [`methods/priority_stack.cljc`](methods/priority_stack.cljc) | SSoT offline stack (1)L0 (2)disclosure held-stress (3)mitsuho R1→gated DESIGN |
| [`data/priority-stack-design.edn`](data/priority-stack-design.edn) | machine-readable design for priorities (1)(2)(3) |
| [`public/priority-stack-offline.edn`](public/priority-stack-offline.edn) | static machine facts for (1)(2)(3) SSoT (pages package) |
| [`methods/l0_enroll.cljc`](methods/l0_enroll.cljc) | L0 enroll + disclosure SM tick; `enroll-with-all-seven-rails` (six in-kind + warifu residual) |
| [`lex/commitmentVow.edn`](lex/commitmentVow.edn) | §1.16.3a lexicon |
| [`methods/disclosure_hold.cljc`](methods/disclosure_hold.cljc) | open/held/exit-suspended SM |
| [`methods/disclosure_continuity.cljc`](methods/disclosure_continuity.cljc) | continuity tick / tick-series (stale → hold) |
| [`methods/ss_offline_path.cljc`](methods/ss_offline_path.cljc) | L0→ladder L4→stage floors→all 7 rails membranes + R2 refuse |
| [`methods/displacement_l0_path.cljc`](methods/displacement_l0_path.cljc) | funded itonami displacement → L0 + L4 multi-gen membranes + held-stress embed |
| [`methods/displacement_tenure.cljc`](methods/displacement_tenure.cljc) | L4→L6 tenure climb; carries L0 held-stress |
| [`methods/displacement_gov.cljc`](methods/displacement_gov.cljc) | G7 package-subject; preserves held-stress on gov rows + batch counters |
| [`methods/displacement_pipeline.cljc`](methods/displacement_pipeline.cljc) | E2E offline: L0/L4/L6 + G7 + scorecard + audit + public package |

**Entry helpers** (all `live=false`, cash≡0, default refuse):

- `l0/enroll` — (1)+(2) only  
- `l0/enroll-with-mitsuho` / `enroll-with-hikari` / `enroll-with-care` / `enroll-with-housing` — single-rail DESIGN  
- `l0/enroll-with-multi-gen-substrate` — care+housing+food+energy  
- `l0/enroll-with-full-inkind-rails` — six in-kind  
- `l0/enroll-with-all-seven-rails` — six in-kind + liquidity residual (`all-seven-rails-receive-membrane-refused`)  
- `l0/care-first-mitsuho-path` — care then food + ladder; embeds held-stress (stale → ladder refuse)  
- `l0/care-first-hikari-path` — care then energy + ladder; embeds held-stress  
- `l0/care-first-mitsuho-hikari-path` — care then food+energy dual rail + ladder; embeds held-stress  
- `l0/multi-gen-substrate-path` — care+housing then mitsuho+hikari (L4 four-rail) + ladder; embeds held-stress; land-grant never  
- `l0/full-inkind-substrate-path` — six in-kind (multi-gen + tooling/compute vocation) + ladder; embeds held-stress  
- `l0/vocation-recovery-path` — tooling+compute only + ladder (job-loss vocation rails); embeds held-stress (stale → ladder refuse)  
- `l0/liquidity-residual-path` — warifu member-principal residual + ladder; embeds held-stress; loan never; cash≡0  
- `l0/all-seven-substrate-path` — six in-kind + liquidity residual + ladder (capstone); embeds held-stress  

- `l0/priority-path-catalog` — discovery index of offline ladder paths (facts only; design: `data/l0-offline-priority-paths-design.edn`)
- `l0/care-housing-first-path` — care+housing multi-gen substrate + ladder; embeds held-stress; land-grant never  
- `l0/report-falsehood` / `lift-hold` / `falsehood-lift-stress` — Charter-Rider falsehood hold path  
- `l0/exit-suspend` / `re-affirm` / `exit-reaffirm-stress` — exit SM  

Live mint/pin/mail/land-grant/loan remain refuse-by-default (G10). No personal scores.

### food-mitsuho / energy-hikari single rail (R1 → gated-live design)

| Module | Role |
|---|---|
| [`methods/rail_mitsuho.cljc`](methods/rail_mitsuho.cljc) | R1 dry + gated-live **plan**; care-first after care/housing (孫/子); no produce; `design-public-facts` |
| [`data/rail-mitsuho-design.edn`](data/rail-mitsuho-design.edn) | design SSoT + care-first-order + itonami-recovery |
| [`lex/mitsuhoRailDispatch.edn`](lex/mitsuhoRailDispatch.edn) | dispatch package lexicon |
| [`methods/rail_hikari.cljc`](methods/rail_hikari.cljc) | energy sibling; care-first-hikari-path; no generate live; `design-public-facts` |
| [`data/rail-hikari-design.edn`](data/rail-hikari-design.edn) | design SSoT parity with mitsuho |
| scorecard / public surface | all 7 rails `:scorecard/rail-*-design` + `:scorecard/rail-design-catalog` discovery (live-produce never) |

Disclosure held → refuse. Live gate default refuse. cash≡0 / score empty.

### energy-hikari + public surface report

| Module | Role |
|---|---|
| [`methods/rail_hikari.cljc`](methods/rail_hikari.cljc) | energy-hikari R1 + gated-live plan |
| [`methods/public_surface_report.cljc`](methods/public_surface_report.cljc) | facts-only MD/EDN public surface (`out/public-surface.*`) |

```bash
# optional report emit (nbb host; ADR-2607173000)
nbb -cp . -e '(require (quote fuchi.methods.public-surface-report)) (fuchi.methods.public-surface-report/write-report!)'
# → out/public-surface.{md,edn,html}  (facts only; displacement earmark table included)
# analyze dry-run report:
# nbb -cp . -e '(require (quote fuchi.methods.analyze)) (fuchi.methods.analyze/-main)'
```

### Displacement surface (itonami/robotics coupling)

[`methods/displacement_surface.cljc`](methods/displacement_surface.cljc) projects
`:cohort/displacement` → public earmark facts (G2 funded cohort). No worker ranking scores.

### itonami bridge + mitsuho dry receive

| Module | Role |
|---|---|
| [`methods/itonami_bridge.cljc`](methods/itonami_bridge.cljc) | itonami displacement EDN → couple events / public facts |
| [`data/itonami-displacement-events.edn`](data/itonami-displacement-events.edn) | representative surplus events |
| [`methods/mitsuho_receive.cljc`](methods/mitsuho_receive.cljc) | food dry-ack + `gated-receive-status` (produce not invoked; default refuse) |
| [`methods/hikari_receive.cljc`](methods/hikari_receive.cljc) | energy dry-ack + `gated-receive-status` (generate not invoked; default refuse) |
| [`methods/mitsuho_produce_plan.cljc`](methods/mitsuho_produce_plan.cljc) | dry kcal floor + `gated-produce-status` (produce-executed=false) |
| [`methods/hikari_produce_plan.cljc`](methods/hikari_produce_plan.cljc) | dry kWh floor + `gated-produce-status` (generate-executed=false) |
| [`methods/care_iyashi_receive.cljc`](methods/care_iyashi_receive.cljc) | care dry-ack + `gated-receive-status` (delivery not invoked; 孫/子) |
| [`methods/care_iyashi_produce_plan.cljc`](methods/care_iyashi_produce_plan.cljc) | dry care-hours + `gated-produce-status` (delivery-executed=false) |
| [`methods/rail_housing_commons.cljc`](methods/rail_housing_commons.cljc) | housing-commons (LANDS.md) R1+gated plan |
| [`methods/rail_tooling_okaimono.cljc`](methods/rail_tooling_okaimono.cljc) | tooling-okaimono R1+gated plan (vocation recovery) |
| [`methods/tooling_okaimono_receive.cljc`](methods/tooling_okaimono_receive.cljc) | tooling dry-ack + `gated-receive-status` (fulfillment not invoked) |
| [`methods/tooling_okaimono_produce_plan.cljc`](methods/tooling_okaimono_produce_plan.cljc) | dry tool-units + `gated-produce-status` (fulfillment-executed=false) |
| [`methods/rail_compute_murakumo.cljc`](methods/rail_compute_murakumo.cljc) | compute-murakumo R1+gated plan (mesh access) |
| [`methods/compute_murakumo_receive.cljc`](methods/compute_murakumo_receive.cljc) | compute dry-ack + `gated-receive-status` (quota not invoked) |
| [`methods/compute_murakumo_produce_plan.cljc`](methods/compute_murakumo_produce_plan.cljc) | dry GPU-hours + `gated-produce-status` (quota-executed=false) |
| [`methods/housing_commons_receive.cljc`](methods/housing_commons_receive.cljc) | housing dry-ack + `gated-receive-status` (land grant not invoked; 孫/子) |
| [`methods/housing_commons_produce_plan.cljc`](methods/housing_commons_produce_plan.cljc) | dry housing-months + `gated-produce-status` (grant-executed=false) |
| [`methods/rail_liquidity_warifu.cljc`](methods/rail_liquidity_warifu.cljc) | liquidity-warifu member-principal residual (cash≡0) |
| [`methods/liquidity_warifu_receive.cljc`](methods/liquidity_warifu_receive.cljc) | warifu dry-ack + `gated-receive-status` (loan not invoked; no produce plan) |
| [`methods/ss_offline_path.cljc`](methods/ss_offline_path.cljc) | L0→ladder→disclosure→all 7 rails R1/gated-receive/produce DESIGN (default refuse) + full-chain refuse facts + R2 refuse |
| [`methods/rail_care_iyashi.cljc`](methods/rail_care_iyashi.cljc) | care-iyashi (子・孫 wellbecoming) R1+gated plan |
| [`methods/itonami_surplus_ledger.cljc`](methods/itonami_surplus_ledger.cljc) | offline surplus ledger (cash-to-workers≡0; G2) |
| [`methods/displacement_l0_path.cljc`](methods/displacement_l0_path.cljc) | funded displacement → L0 + food/care/energy + L0→L1 |
| [`methods/liberation_ladder.cljc`](methods/liberation_ladder.cljc) | offline L0–L6 stage climb (disclosure-gated; no mint) |
| [`methods/stage_sustenance.cljc`](methods/stage_sustenance.cljc) | stage rails-hint → dry floor packages (L3 vocation+) |
| [`methods/disclosure_continuity.cljc`](methods/disclosure_continuity.cljc) | continuous disclosure tick (stale → hold) |
| [`methods/displacement_book.cljc`](methods/displacement_book.cljc) | offline toritate/kanae book for displacement floors |
| [`methods/displacement_couple.cljc`](methods/displacement_couple.cljc) | G2 earmark headroom vs booked floors (commit_live refuse) |
| [`methods/displacement_scorecard.cljc`](methods/displacement_scorecard.cljc) | E2E offline scorecard (all live legs refused) |
| [`methods/displacement_tenure.cljc`](methods/displacement_tenure.cljc) | optional L4→L5/L6 tenure climb + re-book/G2 |
| [`methods/displacement_pipeline.cljc`](methods/displacement_pipeline.cljc) | single offline entry: L4 + L6 tenure + G7 + scorecard + optional public package |
| [`methods/pipeline_audit_ledger.cljc`](methods/pipeline_audit_ledger.cljc) | append-only offline pipeline audit (`.ednl`) |
| [`methods/displacement_gov.cljc`](methods/displacement_gov.cljc) | G7 route + dry sbt-vote/council packages (no finalize) |
| [`methods/r2_execute.cljc`](methods/r2_execute.cljc) | R2 execute membrane (default refuse; executed=false) |
| [`methods/pages_publish.cljc`](methods/pages_publish.cljc) | Pages-ready `public/` static package (no deploy); README documents offline priority path (L0/disclosure/all-seven/care-first) |
| [`methods/pages_deploy.cljc`](methods/pages_deploy.cljc) | Pages deploy membrane (default refuse; wrangler not invoked) |

```bash
# one-shot offline: pipeline → scorecard → audit → public/ (plan-only; never deploys)
nbb -cp . methods/write_all.cljs
# equivalent: nbb -cp . -e '(require (quote fuchi.methods.displacement-pipeline)) (fuchi.methods.displacement-pipeline/write-all!)'
# → out/displacement-scorecard.{md,edn} + out/pipeline-audit-ledger.ednl + public/*
#    deployed=false wrangler-invoked=false land-grant-executed=0 cash≡0

nbb -cp . -e '(require (quote fuchi.methods.pages-publish)) (fuchi.methods.pages-publish/write-pages!)'
# → public/index.html + facts.edn only  (point Cloudflare Pages here; OOB deploy)
```

## Why it exists

Real-world maintainers must be able to live; robotics/remote-control work cannot maintain itself.
The charter forbids investing-in-members (non-profit / donation-only / cash≡0 / payoff帰属=etzhayyim),
so 扶持 meets the real need the charter-clean way: it **maximizes in-kind substitution** and
**routes the irreducible external fiat residual to member-principal 0% liquidity** — 扶持 never
holds, lends, or pays cash.

## System-of-systems

扶持 is a horizontal control-plane standing ON TOP of existing systems:

```
Public Fund + TitheRouter + Mission-funding revenue arm   (value source)
   └ Displacement-Dividend tenure curve                    (allocation math, reused)
       └ Basic-High-Income in-kind                          (cash≡0 delivery semantics)
           └ in-kind rails:
               housing → commons-land (LANDS.md)
               food    → mitsuho 瑞穂
               energy  → hikari 光
               compute → Murakumo mesh
               tooling → okaimono 御買物
               care    → iyashi / hagukumi / kokoro
               liquidity → warifu 0% qard-ḥasan (MEMBER-PRINCIPAL only)
           └ toritate (books) + kanae (viz)
           └ 1 SBT = 1 vote / Council Lv7 (governance)
           └ kotoba Datom `as-of` (Wellbecoming trajectory, 非終末論)
```

## Lifecycle

`covenant → need assessment → allocation compute → routing dispatch → governance gate → in-kind
provisioning → Wellbecoming append`

The governance gate is a **pure function** (G7): below the ceiling and in-kind → `auto`; above →
`sbt-vote`; invariant-adjacent (e.g. a new commons-land grant) → `council-lv7`; Charter-Rider §2
hit → `refused`. 扶持 computes + routes; the vote / Council decides.

## R1 a/b/c/d (landed offline)

- **(a) provisioning-intent wiring** (`methods/provision.py`) — the in-kind rails are mapped to
  the **real producing actor DIDs**: `mitsuho` (food), `hikari` (energy), `okaimono` (tooling),
  `iyashi` (care), `commons-land` (housing, LANDS.md), `murakumo` (compute), `warifu` (liquidity).
  Each is a **dry-run** intent: `published=false` (G10), `cash=0` (G2), `serverHeldKey=false` (G9).
  The liquidity intent is `member_principal` (the member borrows via warifu 0%; 扶持 never pays).
- **(b) real 1 SBT = 1 vote + 48h timelock** (`methods/vote.py`) — ballots dedupe by DID
  (1 SBT = 1 vote), `weight≡1` (no plutocracy), a `:server` voter is unrepresentable, ballots
  outside the window don't count, and `finalize()` **raises** if the 48h timelock has not elapsed.
- **(c) toritate booking + kanae flow viz** (`methods/book.py`) — each accepted in-kind rail is
  projected into a **toritate `ledgerEntry`** using toritate's own category enum
  (`subsistence-flow`/`vocation-flow`/`care-flow`), `cashStipendUsd≡0`, no payroll/wage; the
  member-principal liquidity rail is **not booked as income**. A **kanae-renderable** internal
  sustenance-flow graph (`:flow/*`: Public Fund → 扶持 → provider → maintainer) is emitted for
  the viz layer (NOT the government `fundFlowEdge`).
- **(d) Displacement-Dividend coupling** (`methods/couple.py` + `cohortEarmark` lexicon +
  `:event/:earmark/:couple`) — the structural join to the labor-liberation mission's other half: a
  **displacing actor's surplus** → donation → **TitheRouter 10% split** (`gross = tithe + earmark`,
  exact) → a **per-cohort Public-Fund earmark** that is the imputed-value budget ceiling 扶持's
  in-kind sustenance for that cohort draws on. The **G2 coupling gate** (ADR-2606032130): a
  displacement is admissible **only against a funded cohort earmark with headroom** — *no live
  displacement without a funded cohort* (an unfunded / over-committed cohort is REFUSED). Honest:
  the surplus→donation is real USDC into the Public Fund; what the maintainer receives stays in-kind
  (cash≡0).

## R1 (live-but-gated)

Each outward leg now has a **live path that refuses by default** (`methods/live_gate.cljc`), exactly
as yadori's live RDAP fetch refuses without `YADORI_ALLOW_LIVE_RDAP=1`:

- `provision.dispatch_live` · `vote.finalize_binding` · `book.write_live` · `couple.commit_live`
  each call `live_gate.require()`, which **raises `LiveGateRefused`** unless ALL of:
  1. the operator process flag `FUCHI_ALLOW_LIVE_<LEG>=1` is set (an operator action on the box);
  2. an **operator attestation** DID is present;
  3. Council **Lv6+** has ratified (**Lv7+** for `couple` — invariant-adjacent: it binds the
     robotics displacement wave);
  4. a **member signature** is present (no-server-key — the server can never sign, ADR-2605231525).
- The gate is an **authorization membrane, never an invariant override**: cash≡0, no-server-key,
  in-kind-only rails, the 48h vote timelock, and the G2 funded-cohort gate all still hold in live
  mode (the `couple` leg stacks `LiveGateRefused` *and* the G2 `ValueError`).
- `analyze.py` prints every leg **refused** in a dry run and emits `out/live-gate-status.kotoba.edn`.

Actual live execution (real dispatch / on-chain binding vote / live toritate write / binding
displacement commit) still needs the env flag flipped **and** Council ratification — it cannot
happen on this branch.

## Layout

```
fuchi/
├── manifest.jsonld
├── data/seed-sustenance-graph.kotoba.edn   # :representative seed + displacement events
├── lex/                                     # com.etzhayyim.fuchi.* lexicons
├── methods/
│   ├── allocate.cljc / route.cljc / provision.cljc / vote.cljc / book.cljc / couple.cljc
│   ├── live_gate.cljc     # R1(live) operator+Council+member gate; every leg refuses by default
│   ├── analyze.cljc       # end-to-end dry-run scorecard
│   ├── l0_enroll.cljc / disclosure_hold.cljc / ss_offline_path.cljc
│   ├── rail_*.cljc + *_receive.cljc + *_produce_plan.cljc  # all-seven membranes
│   ├── displacement_*.cljc / pages_publish.cljc / pages_deploy.cljc
│   └── public_surface_report.cljc  # facts-only public HTML/MD/EDN (no scores)
├── public/                # Pages-ready static package (write-pages! / write-deploy-package!)
└── cells/                 # 5 coded state machines
```

## Run

```bash
nbb -cp . run_tests.cljs       # offline suite host (ADR-2607173000; no .sh / no bb)
nbb -cp . methods/readiness_check.cljs   # design + priority-stack SSoT (fast)
nbb -cp . methods/priority_stack_smoke.cljs
nbb -cp . methods/write_all.cljs   # scorecard+audit+surplus+public (never deploys)
# land when terminal works:
# nbb methods/_land_ss_gated_wip.cljs
# publish:
# nbb methods/publish.cljs
```

## Honest R0/R1

Design + offline allocation only. `:representative` seed. No live disbursement / provisioning /
land grant / binding vote (all G10 — Council Lv6+ + operator; invariant-adjacent Lv7+). The R1
a/b/c engines are built and tested **offline**; flipping them to live is a future ADR + Council
gate. 扶持 cannot eliminate a maintainer's external fiat obligations — it maximizes in-kind
coverage and routes the residual to member-principal 0% liquidity (N4). **Zero invariant amendments.**
