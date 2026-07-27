# fuchi (扶持) — maturity ledger

**Actor**: 扶持 (fuchi) — maintainer sustenance allocator (investment-fund inverse) · **ADR**:
2606052300 + **2607177000** (public-person as-of; wellbecoming/mago/ko priority; scores unrepresentable) · **DID**: `did:web:etzhayyim.com:actor:fuchi`

| Axis | R0 + R1 a/b/c/d (offline) | R1 live-but-gated (this) | live execution (Council-gated) |
|---|---|---|---|
| **covenant** | offline screen+record over seed (G4/G5/G9) | — | live intake over real 信者 roster (MEMBERS.md) |
| **assessment** | in-kind envelope, cash≡0 (G2/G3) | — | live need assessment per maintainer |
| **allocation** | tenure-weighted in-kind, G1 allowlist, cash≡0 | — | member-signed live allocation |
| **routing (a)** | **rails wired to real producing-actor DIDs** (mitsuho/hikari/okaimono/iyashi/commons-land/Murakumo/warifu); dry-run intents | `dispatch_live()` exists, **refuses by default** (gate) | live provisioning dispatch (flag + Council Lv6+ + member sig) |
| **governance (b)** | **real 1 SBT = 1 vote + 48h timelock** (dedupe, weight≡1, no-server-key, finalize-raises-early) | `finalize_binding()` exists, **refuses by default**; timelock still strict | binding vote on-chain |
| **booking (c)** | **toritate ledgerEntry projection** (cash≡0) + **kanae :flow/* graph** | `write_live()` exists, **refuses by default** | toritate writes the live ledger + kanae renders live |
| **coupling (d)** | **Displacement-Dividend earmark** (TitheRouter 10% split, exact) + **G2 gate** (no displacement w/o funded cohort) | `commit_live()` exists, **refuses by default**; needs **Lv7** + G2 funded-cohort | live surplus→donation→earmark; binding G2 gate on the robotics wave |
| **live gate** | — | **`methods/live_gate.cljc`** — single authorization membrane; per-leg `FUCHI_ALLOW_LIVE_<LEG>` flag + operator attestation + Council Lv6+/Lv7+ + member sig; default refused; never overrides cash≡0/no-server-key/G3 | env flag flipped + Council ratifies |

## R0 + R1 a/b/c/d + R1 live-but-gated evidence

- **Tests green via `nbb -cp . run_tests.cljs`** (nbb in-process / ADR-2607173000; no bash, no bb
  spawn): allocate/route/provision/vote/book/couple + live_gate + analyze +
  charter-invariants + lexicons + cells + offline SS path suites (L0 enroll, disclosure, all-seven
  rails, displacement pipeline, pages publish/deploy).
- **R1 (live-but-gated)** — `methods/live_gate.cljc` is the single membrane every outward leg crosses.
  `provision.dispatch_live` / `vote.finalize_binding` / `book.write_live` / `couple.commit_live`
  each call `live_gate.require()`, which **raises `LiveGateRefused` unless** the operator process
  flag (`FUCHI_ALLOW_LIVE_<LEG>=1`) + an operator attestation DID + Council **Lv6+** (Lv7+ for
  `couple`, invariant-adjacent) + a **member signature** (no-server-key) are ALL present. Default =
  refused (the deliverable). The gate is an authorization membrane, **never** an invariant override:
  cash≡0, no-server-key, in-kind-only rails, the 48h vote timelock, and the G2 funded-cohort gate
  all still hold in live mode (the `couple` leg stacks both refusals). `analyze.cljc` prints every leg
  refused. Actual live execution still needs the env flag flipped + Council ratification.
- **R1(a–d)** — offline engines in `methods/*.cljc` (provision/vote/book/couple) over the seed;
  G2 refuses unfunded displacement cohorts; cash≡0; land-grant/loan never from scaffold.
- **Offline robotics/itonami SS path** — L0 enroll + disclosure SM + care-first rails + all-seven
  membranes + scorecard/public/audit/deploy surfaces (default refuse; see Offline section below).
- **Charter-clean inverse proven structurally** (`test_charter_invariants.cljc`, parses the ontology +
  lexicons + code, not prose-grep): G1 the instrument set is the sustenance set and equity/debt/
  ROI/exit are absent from `:alloc/instrument :db/allowed`; G2 cash fields are `:db/allowed [0]`;
  G3 `:rail/kind` has no `:cash-disbursement`; G5 `:maintainer/owns-payoff :db/allowed [false]`;
  G7 no `:gov/decision` attribute exists; G9 `:alloc/server-held-key :db/allowed [false]`.
- **Tenure curve reused**, not reinvented — the Displacement-Dividend `ln(1+min(tenure,40))×hazard`
  (ADR-2606032130).
- **Registered** in `INFRA_ACTORS` → `did:web:etzhayyim.com:actor:fuchi` (resolvable + searchable);
  actor-profile seed added.

## Offline robotics/itonami SS path (scaffold, not live)

Priority stack offline (wellbecoming > 孫 > 子; cash≡0; scores empty; live default refuse):

1. **L0 enroll** — `methods/l0_enroll.cljc` (`enroll`, `enroll-with-all-seven-rails`)
2. **Disclosure continuity + hold** — hold SM + `apply-disclosure-tick` / `continuity-stress` /
   `exit-suspend` / `re-affirm` / `try-ladder-advance`
3. **Rails R1→gated DESIGN** — care/housing first, then food/energy (mitsuho+hikari dual rail),
   tooling/compute, liquidity residual

E2E projection: `ss_offline_path` + `displacement_l0_path` (itonami funded→L0→L4 +
held-stress embed, carried through `displacement_tenure` L6) + `displacement_pipeline`
+ scorecard
facts (`l0-all-seven-enroll`, `l0-held-all-seven-enroll`, `l0-exit-reaffirm`, care-first mitsuho/hikari,
`care-first-mitsuho-hikari` dual rail, care+housing, `multi-gen-substrate` L4 four-rail,
`full-inkind-substrate` six-rail, `vocation-recovery` tooling+compute,
`liquidity-residual` warifu member-principal, `all-seven-substrate` capstone —
**all nine ladder paths embed priority-(2) held-stress** (stale → ladder refuse),
`priority-path-catalog` discovery index with `held-stress-embed-count` =
`path-count` and invariant `held-stress-embed-all`). Public/audit/deploy surfaces
mirror those facts (audit last-run also records catalog embed-count);
`pages_publish` static README documents the same offline priority stack (deploy package may overwrite
README with last-run status lines).
**No** live mint, land-grant, loan, or wrangler deploy from scaffold.

## Honest gaps (R0)

- Offline itonami SS stack is complete in code: L0 enroll + disclosure SM + nine ladder paths
  (held-stress embed) + **all seven** single-rail R1→gated DESIGN facts (care→housing→food→
  energy→tooling→compute→liquidity) + `rail-design-catalog` discovery +
  displacement→L0→tenure→G7 gov (held-stress carried) + scorecard/audit/public/deploy.
  Design index: `data/l0-offline-priority-paths-design.edn`,
  `data/rail-design-catalog.edn`, `data/itonami-offline-ss-readiness.edn`, rail-*-design.edn.
  SS path embeds **all seven** design-public-facts (care→housing→food→energy→tooling→
  compute→liquidity; live-produce-never). Package + pipeline + report path are nbb-portable:
  package writers, pipeline, scorecard, surplus ledger, all-seven design-edn, and offline path
  tests (pipeline/scorecard/disclosure/ss/itonami/public-person/audit).
  SSoT: `methods/priority_stack.cljc` (`run-offline` = (1)L0 (2)disclosure (3)mitsuho).
  Fast check: `nbb -cp . methods/readiness_check.cljs`.
  Smoke: `nbb -cp . methods/priority_stack_smoke.cljs`. Land: `nbb methods/_land_ss_gated_wip.cljs`
  (prefers `write-all!`). Publish: `nbb methods/publish.cljs` (no `.sh` / `.bb` entrypoints).
  **Landing** may still be blocked when agent terminal spawn fails —
  use `nbb methods/_land_ss_gated_wip.cljs` when terminal works.
- No live disbursement / provisioning / land grant / binding vote — all G10 (Council Lv6+ +
  operator; invariant-adjacent Lv7+). The R1 a/b/c engines are built + tested **offline**; flipping
  them to live is the gated R1-live phase.
- The `:representative` seed is 5 illustrative maintainers, not the live roster.
- 扶持 **cannot eliminate a maintainer's external fiat obligations** — it maximizes in-kind coverage
  and routes the irreducible residual to member-principal 0% warifu liquidity (N4). Full
  fiat-denominated income is a Charter Lv7+ matter and is out of scope.
- No UI — a maintainer-sustenance dashboard is R1+.

## Zero invariant amendments

fuchi **strengthens** five existing invariants and amends none: cash≡0 (ADR-2605301020),
no-server-key (ADR-2605231525), payoff帰属=etzhayyim, Charter-Rider §2(b) speculative-finance
prohibition (ADR-2605192200), and the non-profit / donation-only invariants (ADR-2605192100/192115).
