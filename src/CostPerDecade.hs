-- | Buy-it-for-life arithmetic: cost per decade, cost per use, and the
-- year the cheap option stops being the bargain.
module CostPerDecade
  ( costPerDecade
  , costPerUse
  , purchasesOver
  , breakEvenYear
  , warrantyClass
  ) where

-- | Price spread over a decade of expected life.
costPerDecade :: Double -> Double -> Double
costPerDecade price lifeYears = price / lifeYears * 10.0

-- | Price of a single use, given uses per year. 'Nothing' when the item
-- never gets used.
costPerUse :: Double -> Double -> Double -> Maybe Double
costPerUse price lifeYears usesPerYear
  | usesPerYear <= 0 = Nothing
  | otherwise        = Just (price / (lifeYears * usesPerYear))

-- | Units owned across a horizon: ceiling of horizon over life,
-- minimum one. Replacements land when a unit dies.
purchasesOver :: Double -> Double -> Int
purchasesOver horizonYears lifeYears =
  max 1 (ceiling (horizonYears / lifeYears - 1e-9))

-- | First year the cheap stack has spent at least as much as the
-- durable one. Both buyers pay for their first unit at year 0, and
-- replacements land when a unit dies. 'Just' 0 when the durable is
-- cheaper up front, 'Nothing' when the cheap stack never catches up
-- inside the horizon.
breakEvenYear :: Double -> Double -> Double -> Double -> Double -> Maybe Int
breakEvenYear priceDurable lifeDurable priceCheap lifeCheap horizonYears
  | priceCheap >= priceDurable = Just 0
  | otherwise                  = search 1 priceDurable lifeDurable priceCheap lifeCheap
  where
    horizon = floor horizonYears :: Int
    search year dSpend dNext cSpend cNext
      | year >= horizon = Nothing
      | otherwise =
          let dHit   = fromIntegral year >= dNext
              cHit   = fromIntegral year >= cNext
              dSpend' = if dHit then dSpend + priceDurable else dSpend
              dNext'  = if dHit then dNext + lifeDurable else dNext
              cSpend' = if cHit then cSpend + priceCheap else cSpend
              cNext'  = if cHit then cNext + lifeCheap else cNext
          in if cSpend' >= dSpend'
               then Just year
               else search (year + 1) dSpend' dNext' cSpend' cNext'

-- | How much of the expected life the warranty actually covers.
warrantyClass :: Double -> Double -> String
warrantyClass warrantyYears lifeYears
  | warrantyYears <= 0 = "no warranty stated"
  | warrantyYears >= lifeYears =
      "covered for life (" ++ whole warrantyYears ++ "y warranty on "
        ++ whole lifeYears ++ "y expected life)"
  | warrantyYears / lifeYears >= 0.5 =
      "solid (" ++ whole warrantyYears ++ "y warranty covers "
        ++ pct (warrantyYears / lifeYears) ++ "% of expected life)"
  | otherwise =
      "thin (" ++ whole warrantyYears ++ "y warranty covers only "
        ++ pct (warrantyYears / lifeYears) ++ "% of expected life)"
  where
    whole v = show (floor v :: Int)
    pct r   = show (round (100 * r) :: Int)
