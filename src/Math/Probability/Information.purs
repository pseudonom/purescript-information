module Math.Probability.Information where

import Data.Newtype (class Newtype, unwrap, wrap)
import Math (log)
import Prelude

import Math.Probability (Dist, Iso, Prob, expected, just, runProb, (??))

newtype Entropy = Entropy Number

selfInformation :: Prob -> Entropy
selfInformation = entropyNum.from <<< negate <<< log2 <<< runProb

entropy :: forall x z. (Eq x) => Dist z -> (z -> Dist x) -> Entropy
entropy zs x'zs = expected entropyNum $ do
  z <- zs
  x'z <- x'zs z
  let px'z = just x'z ?? x'zs z
  pure $ selfInformation px'z

pointwiseInformation :: Prob -> Prob -> Prob -> Entropy
pointwiseInformation pxy'z px'z py'z =
  entropyNum.from $ log2 (xy / (x * y)) where
    xy = runProb pxy'z
    x = runProb px'z
    y = runProb py'z

mutualInformation ::
  forall j x y z. Eq x => Eq y => Eq j =>
  Dist z -> (z -> Dist j) -> (j -> x) -> (j -> y)  -> Entropy
mutualInformation zs xys'z jx jy = expected entropyNum $ do
  z <- zs
  xy'z <- xys'z z
  pure $ pointwiseInformation (just xy'z ?? xys'z z)
                              ((==) (jx xy'z) <<< jx ?? xys'z z)
                              ((==) (jy xy'z) <<< jy ?? xys'z z)

divergence :: forall x z. (Eq x) =>
              Dist z -> (z -> Dist x) -> (z -> Dist x) -> Entropy
divergence zs x'zs y'zs = expected entropyNum $ do
  z <- zs
  x'z <- x'zs z
  let px'z = just x'z ?? x'zs z
  let py'z = just x'z ?? y'zs z
  pure <<< entropyNum.from <<< log2 $ runProb px'z / runProb py'z

-- | Helper function for using `entropy` and `mutualInformation` with
-- | non-conditional distributions.
nonCond :: forall b c. (Dist Unit -> (Unit -> Dist b) -> c) -> Dist b -> c
nonCond f d = f (pure unit) (const d)

entropyNum :: Iso Entropy Number
entropyNum = { from: wrap, to: unwrap }
log2 :: Number -> Number
log2 = logBase 2.0
logBase :: Number -> Number -> Number
logBase b n = log n / log b

derive instance ntEnt :: Newtype Entropy _
derive newtype instance eqEnt :: Eq Entropy
derive newtype instance ordEnt :: Ord Entropy
derive newtype instance showEnt :: Show Entropy
