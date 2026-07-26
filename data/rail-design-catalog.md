# fuchi — all-seven rail DESIGN catalog (priority 3)

Facts only. cash≡0 · live=false · live-produce never · no personal scores.
Code SSoT: `fuchi.methods.displacement-scorecard/rail-design-catalog-fact`
EDN index: `data/rail-design-catalog.edn`

## Order (multi-gen first)

care-iyashi → housing-commons → food-mitsuho → energy-hikari → tooling-okaimono → compute-murakumo → liquidity-warifu

## Rails

| # | kind | care-first API | never |
|---|------|----------------|-------|
| 1 | care-iyashi | care-housing-first-path | care-delivery live |
| 2 | housing-commons | care-housing-first-path | land-grant |
| 3 | food-mitsuho | care-first-mitsuho-path | produce |
| 4 | energy-hikari | care-first-hikari-path | generate |
| 5 | tooling-okaimono | vocation-recovery-path | fulfillment |
| 6 | compute-murakumo | vocation-recovery-path | mesh quota |
| 7 | liquidity-warifu | liquidity-residual-path | loan; fuchi cash |

## Invariants

- cash-usd-micros ≡ 0
- live ≡ false
- live-produce-never
- loan-never / land-grant-never
- public-person facts only (no ranks/scores)

## Surfaces

- `:scorecard/rail-*-design` + `:scorecard/rail-design-catalog`
- `:report/rail-*-design` + `:report/rail-design-catalog`
- pipeline audit last-run rail-design-* counters
- pages publish/deploy README discovery text
