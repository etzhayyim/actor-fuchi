# fuchi — displacement SS offline scorecard

Priority: wellbecoming > mago(孫) > ko(子) > present. cash≡0. live=false. No personal scores.

## Summary

- admissible cohorts: 2
- refused cohorts: 2
- enrolled subjects (L4 path): 4
- stages (L4 path): {"L4" 4}
- tenure target: L6
- tenure admissible cohorts: 2
- tenure subjects: 4
- tenure stages: {"L6" 4}
- committed USD micros (L4): 18800000000
- headroom USD micros (L4): 143200000000
- tenure committed USD micros (flowable-first): 18800000000
- tenure post-ratify committed: 42800000000
- booked ledger entries (L4): 24
- tenure booked entries: 24
- all live legs refused: true
- gov routes (L4): {"council-lv7" 4}
- gov flowable committed L4 (housing held): 18800000000
- gov post-ratify committed L4 (grant false): 42800000000
- couple post-ratify committed L4: 42800000000
- tenure gov routes: {"council-lv7" 4}
- tenure gov flowable (housing held): 18800000000
- tenure gov post-ratify (grant false): 42800000000
- L4 disclosure open/held: 4/0
- tenure disclosure open/held: 4/0
- mitsuho food R1-dry / gated-refused / produce-executed: 8/8/0
- hikari energy R1-dry / gated-refused / generate-executed: 8/8/0
- care-iyashi R1-dry / gated-refused / care-delivery-executed: 8/8/0
- displacement L0 held-stress subjects / ladder-refused: 4/4
- tenure held-stress subjects / ladder-refused / carried-from-L0: 4/4/4
- gov held-stress subjects / ladder-refused (L4 rows): 4/4
- tenure-gov held-stress subjects / ladder-refused: 4/4
- displacement L0 membranes subjects / care+housing full-chain-refused / all-inkind full-chain-refused / all-seven receive-membrane: 8/8/8/8
- displacement L0 liquidity residual receive-refused (member-principal): 8
- displacement L0 food/care/housing gated-recv-refused: 8/8/8
- housing-commons R1-dry / gated-refused / land-grant-executed: 8/8/0
- housing council-held (awaiting Lv7): 8
- tooling-okaimono R1-dry / gated-refused / fulfillment-executed: 8/8/0
- compute-murakumo R1-dry / gated-refused / quota-executed: 8/8/0
- liquidity-warifu R1-dry / gated-refused / loan-executed: 8/8/0
- liquidity member-principal / cash-usd-micros: 8/0
- R2 execute membrane statuses / refused / executed: 52/52/0
- all-r2-not-executed: true
- r2-by-rail: {"care" 8, "housing" 8, "food" 8, "energy" 8, "tooling" 8, "compute" 8}

## Priority stack offline SSoT (1)L0 (2)disclosure (3)care-housing→mitsuho+hikari→all-seven

- ok: true (regen via `nbb -cp . methods/write_all.cljs` / scorecard build)
- (1) L0 enroll offline scaffold — stage L0, published=false, cash≡0
- (2) disclosure hold + continuity — open may-flow; stale held; tick-series reopen
- (3) care-housing-first-path — both refuse; land-grant never; held-stress ladder refuse (孫/子)
- (3) mitsuho R1-dry → gated refuse; care-first-mitsuho-path; before-rails [care housing]; held-stress
- (3) hikari R1-dry → gated refuse; care-first-hikari-path; before-rails [care housing]; held-stress
- (3) all-seven-substrate-path — inkind+membrane+liq refuse; loan/land-grant never; held-stress
- l0-paths catalog: 9 paths, all held-stress-embed
- live / cash / score-surface: false/0/[]
- design-id / order-count: fuchi.priority-stack-offline/3
- design: `data/priority-stack-design.edn` · api: `fuchi.methods.priority-stack/run-offline`

## L0 enroll all-seven rails (priority 1+2+3 smoke)

- api: enroll-with-all-seven-rails
- disclosure open/held/may-flow: open/false/true
- care/housing/mitsuho/hikari full-chain-refused: true/true/true/true
- tooling/compute full-chain-refused: true/true
- all-inkind / liquidity-receive / all-seven-membrane: true/true/true
- liquidity member-principal / loan-executed / cash-usd-micros: true/false/0
- land-grant-executed / live / cash: false/false/0
- continuity stress final/held-steps: open/0
- ladder advance phase/refused: advanced/false

## L0 held all-seven (disclosure stale stress)

- api: enroll-with-all-seven+stale
- disclosure open/held/may-flow: held/true/false
- all-inkind / liq-recv / all-seven-membrane: true/true/true
- ladder advance phase/refused: refused/true
- loan / land-grant / live / cash: false/false/false/0

## L0 exit→re-affirm stress (disclosure SM)

- api: exit-suspend→re-affirm
- exit state/suspended/may-flow: exit-suspended/true/false
- exit ladder phase/refused: refused/true
- re-affirm state/suspended/may-flow: open/false/true
- re-affirm ladder phase/refused: advanced/false
- live / cash-usd-micros: false/0

## L0 falsehood→lift-hold stress

- api: falsehood→lift-hold
- falsehood held/may-flow/ladder-refused: true/false/true
- lift state/may-flow/ladder: open/true/advanced/refused=false
- live / cash: false/0

## L0 care-first + mitsuho (priority 1+2+3 孫/子)

- api: enroll+care+mitsuho+ladder
- disclosure open/held/may-flow: open/false/true
- care/mitsuho full-chain/both-refused: true/true/true
- care-first-api-path / before-rails: care-first-mitsuho-path / care,housing
- mitsuho-design rail-kind / live-produce / produce-executed: food-mitsuho/false/false
- care-design rail-kind / care-delivery-executed: care-iyashi/false
- ladder phase/refused: advanced/false
- held-stress held/both-refused/ladder-refused: true/true/true
- live / cash: false/0

## L0 care-first + hikari (priority 1+2+3 孫/子 + energy)

- api: enroll+care+hikari+ladder
- care/hikari full-chain/both-refused: true/true/true
- care-first-api-path / before-rails: care-first-hikari-path / care,housing
- hikari-design rail-kind / live-produce / generate-executed: energy-hikari/false/false
- ladder phase/refused: advanced/false
- held-stress held/both-refused/ladder-refused: true/true/true
- live / cash: false/0

## L0 care-first + mitsuho + hikari (priority 1+2+3 dual rail)

- api: enroll+care+mitsuho+hikari+ladder
- disclosure open/held/may-flow: open/false/true
- care/mitsuho/hikari full-chain / all-refused: true/true/true/true
- mitsuho+hikari both-refused: true
- mitsuho/hikari design live-produce: false/false
- ladder phase/refused: advanced/false
- held-stress held/all-refused/ladder-refused: true/true/true
- live / cash: false/0

## L0 care+housing multi-gen substrate (孫/子)

- api: enroll+care+housing+ladder
- care/housing full-chain/both-refused: true/true/true
- land-grant-executed / ladder phase/refused: false/advanced/false
- held-stress held/both-refused/ladder-refused/land-grant: true/true/true/false
- live / cash: false/0

## L0 multi-gen substrate + mitsuho+hikari (L4 priority)

- api: enroll+care+housing+mitsuho+hikari+ladder
- disclosure open/held/may-flow: open/false/true
- care/housing/mitsuho/hikari full-chain / all-refused: true/true/true/true/true
- care+housing both / mitsuho+hikari both: true/true
- land-grant-executed / ladder phase/refused: false/advanced/false
- held-stress held/all-refused/ladder-refused/land-grant: true/true/true/false
- live / cash: false/0

## L0 full in-kind substrate (multi-gen + vocation / itonami)

- api: enroll+six-inkind+ladder
- disclosure open/held/may-flow: open/false/true
- six in-kind all-refused: true
- care+housing / mitsuho+hikari / tooling+compute both-refused: true/true/true
- land-grant / fulfillment / quota executed: false/false/false
- ladder phase/refused: advanced/false
- held-stress held/all-refused/ladder-refused/land-grant/fulfillment/quota: true/true/true/false/false/false
- live / cash: false/0

## L0 vocation recovery (tooling+compute / itonami job-loss)

- api: enroll+tooling+compute+ladder
- disclosure open/held/may-flow: open/false/true
- tooling/compute full-chain / both-refused: true/true/true
- fulfillment / quota executed: false/false
- ladder phase/refused: advanced/false
- held-stress held/both-refused/ladder-refused/fulfillment/quota: true/true/true/false/false
- live / cash: false/0

## L0 liquidity residual (warifu member-principal)

- api: enroll+liquidity+ladder
- disclosure open/held/may-flow: open/false/true
- liquidity receive full-chain-refused: true
- member-principal / loan-executed / cash-usd-micros: true/false/0
- ladder phase/refused: advanced/false
- held-stress held/receive-refused/ladder-refused/loan/cash: true/true/true/false/0
- live / cash: false/0

## L0 all-seven substrate (capstone multi-gen + vocation + residual)

- api: enroll-with-all-seven-rails+ladder
- disclosure open/held/may-flow: open/false/true
- all-inkind / liq-recv / all-seven-membrane: true/true/true
- member-principal / loan / land-grant / fulfillment / quota: true/false/false/false/false
- ladder phase/refused: advanced/false
- held-stress held/membrane/ladder-refused/loan/land-grant: true/true/true/false/false
- live / cash: false/0

## L0 offline priority path catalog (discovery)

- catalog-id: fuchi.l0-offline-priority-paths
- path-count: 9
- held-stress-embed-count: 9
- path-ids: care-first-mitsuho,care-first-hikari,care-first-mitsuho-hikari,care-housing-first,multi-gen-substrate,full-inkind-substrate,vocation-recovery,liquidity-residual,all-seven-substrate
- invariants loan-never/land-grant-never/held-stress-embed-all/cash: true/true/true/0
- live: false

## rail-care-iyashi DESIGN (priority 3 multi-gen #1)

- rail-kind / provider: care-iyashi / did:web:etzhayyim.com:actor:iyashi
- care-first-order-rank / api-path: 1 / care-housing-first-path
- multi-gen-first / care-delivery-executed: true/false
- multi-gen-facts: care-hours-support-ko-and-mago-households,not-a-happiness-score,care-is-wellbecoming-substrate
- live / cash / score-surface: false/0/[]

## rail-housing-commons DESIGN (priority 3 multi-gen #2)

- rail-kind / provider: housing-commons / commons-land
- care-first-before-rails: care
- care-first-order-rank / api-path: 2 / care-housing-first-path
- land-grant-executed / live-produce: false/false
- multi-gen-facts: housing-floor-supports-ko-and-mago-households,commons-land-not-private-equity,not-a-happiness-score,land-grant-not-invoked
- live / cash / score-surface: false/0/[]

## rail-mitsuho DESIGN (priority 3 food R1→gated)

- rail-kind / provider: food-mitsuho / did:web:etzhayyim.com:actor:mitsuho
- care-first-before-rails: care,housing
- care-first-api-path: care-first-mitsuho-path
- live-produce / produce-executed: false/false
- multi-gen-facts: staple-kcal-floor-supports-caregiver-and-child-households,food-after-care-housing-for-mago-ko-substrate,not-a-happiness-score,imputed-usd-is-accounting-fact-only
- live / cash / score-surface: false/0/[]

## rail-hikari DESIGN (priority 3 energy R1→gated)

- rail-kind / provider: energy-hikari / did:web:etzhayyim.com:actor:hikari
- care-first-before-rails: care,housing
- care-first-api-path: care-first-hikari-path
- live-produce / generate-executed: false/false
- multi-gen-facts: kwh-floor-supports-household-care-and-learning,energy-after-care-housing-for-mago-ko-substrate,no-fossil-no-nuclear-constitutional,not-a-happiness-score,imputed-usd-is-accounting-fact-only
- live / cash / score-surface: false/0/[]

## rail-tooling-okaimono DESIGN (priority 3 vocation)

- rail-kind / provider: tooling-okaimono / did:web:etzhayyim.com:actor:okaimono
- care-first-api-path / vocation-recovery: vocation-recovery-path / true
- fulfillment-executed / live-produce: false/false
- multi-gen-facts: tool-access-supports-household-vocation-recovery,not-a-happiness-score,tooling-is-wellbecoming-substrate,fulfillment-not-invoked
- live / cash: false/0

## rail-compute-murakumo DESIGN (priority 3 vocation)

- rail-kind / provider: compute-murakumo / murakumo
- care-first-api-path / vocation-recovery: vocation-recovery-path / true
- quota-executed / live-produce: false/false
- multi-gen-facts: compute-access-supports-learning-and-vocation,not-a-happiness-score,mesh-quota-not-invoked,wellbecoming-substrate
- live / cash: false/0

## rail-liquidity-warifu DESIGN (priority 3 residual)

- rail-kind / provider: liquidity-warifu / did:web:etzhayyim.com:actor:warifu
- care-first-api-path / residual-rail: liquidity-residual-path / true
- member-principal / loan-executed / cash: true/false/0
- multi-gen-facts: member-principal-residual-not-fuchi-cash,qard-hasan-zero-percent,not-a-happiness-score,loan-not-invoked
- live: false

## rail DESIGN catalog (all-seven discovery)

- catalog-id: fuchi.rail-design-catalog
- rail-count / ok-count: 7/7
- rail-kinds: care-iyashi,housing-commons,food-mitsuho,energy-hikari,tooling-okaimono,compute-murakumo,liquidity-warifu
- order: care-iyashi→housing-commons→food-mitsuho→energy-hikari→tooling-okaimono→compute-murakumo→liquidity-warifu
- live-produce-never / all-cash-zero / all-live-false: true/true/true
- invariants loan-never/land-grant-never/all-seven-design: true/true/true

## SS priority path (L0 + disclosure + all rails gated)

- L0 stage/published: L0/false
- L0 enroll disclosure open/held/may-flow: open/false/true path=l0-enroll-offline
- ladder offline: L0→L4 target=L4 steps=4 rails-hint-first=care published=false
- stage_sustenance: stage=L4 rails-first/second=care/housing care-h/housing-mo=14600/144 land-grant=false r2-all-refused=true gated-all-refused=true gated-count=6
- stage care/mitsuho/hikari gated-admissible: false/false/false
- care/housing DESIGN live-produce / care-first-api (孫/子 first): false/false · care-housing-first-path/care-housing-first-path ranks=1/2 kinds=care-iyashi/housing-commons
- mitsuho/hikari DESIGN live-produce / care-first-api: false/false · care-first-mitsuho-path/care-first-hikari-path
- mitsuho/hikari design-rail-kind: food-mitsuho/energy-hikari
- tooling/compute/liquidity DESIGN live-produce / care-first-api: false/false/false · vocation-recovery-path/vocation-recovery-path/liquidity-residual-path kinds=tooling-okaimono/compute-murakumo/liquidity-warifu
- all-seven design embed-count / live-produce-never: 7/true
- disclosure state / entitlements-may-flow: open/true
- held-stress held / food-r1 / ladder-refused: true/refused/true
- rails-gated-count / admissible / all-rails-gated-refused: 7/0/true
- mitsuho/hikari/care gated-admissible: false/false/false
- mitsuho/hikari gated-receive admissible/both-refused: false/false/true
- care-iyashi gated-receive (孫/子) admissible/all-three-refused: false/true
- mitsuho/hikari gated-produce admissible/both/full-chain-refused: false/false/true/true
- care gated-produce (孫/子) admissible/produce-all/full-chain: false/true/true
- housing gated-receive/produce (孫/子) admissible/full-chain: false/false/true
- care+housing+food+energy full-chain-refused: true
- tooling/compute gated-receive/produce admissible/full-chain: false/false/true · false/false/true
- tooling+compute full-chain / all-inkind-produce-rails full-chain: true/true
- liquidity gated-receive admissible/receive-full-chain: false/true
- all-seven-rails receive-membrane refused: true
- housing land-grant / liquidity loan / cash: false/false/0
- ss R2 statuses / executed / all-not-executed: 7/0/true
- live: false cash: 0

## All-disclosure-held stress (priority #2, offline)

- stress: all-disclosure-held
- held subjects: 8
- open-path gov flowable: 18800000000
- all-held gov flowable: 0
- all-held tenure gov flowable: 0
- all-held G2 admissible cohorts: 2
- land-grant-executed: 0
- live: false cash: 0

## Cohorts

| actor | cohort | phase | n | L4-flow | L4-post | ten-flow | ten-post | land-grant | headroom | tenure | tenure-n |
|---|---|---|---|---|---|---|---|---|---|---|---|
| sanae | cohort-sanae-2026 | offline-enrolled | 2 | 9400000000 | 21400000000 | 9400000000 | 21400000000 | 0 | 44600000000 | tenure-offline | 2 |
| hataori | cohort-hataori-2026 | refused | 0 | 0 | 0 | 0 | 0 | 0 | 0 | — | 0 |
| itonami-robotics | cohort-robotics-remote-2026 | offline-enrolled | 2 | 9400000000 | 21400000000 | 9400000000 | 21400000000 | 0 | 98600000000 | tenure-offline | 2 |
| warehouse-amr | cohort-warehouse-amr-2026 | refused | 0 | 0 | 0 | 0 | 0 | 0 | 0 | — | 0 |

## Live legs (default refuse)

| leg | admissible | reason |
|---|---|---|
| provision | false | — |
| vote | false | — |
| book | false | — |
| couple | false | — |

## itonami surplus ledger (offline)

- events: 4
- funded-admissible: 2
- refused: 2
- cash-to-workers: 0

_No personal scores, ranks, or percentiles. No live disbursement._
