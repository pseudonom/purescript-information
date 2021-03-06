# Module Documentation

## Module Math.Probability.Information

#### `Entropy`

``` purescript
newtype Entropy
  = Entropy Number
```


#### `selfInformation`

``` purescript
selfInformation :: Prob -> Entropy
```


#### `entropy`

``` purescript
entropy :: forall x z. (Eq x) => Dist z -> (z -> Dist x) -> Entropy
```


#### `pointwiseInformation`

``` purescript
pointwiseInformation :: Prob -> Prob -> Prob -> Entropy
```


#### `mutualInformation`

``` purescript
mutualInformation :: forall j x y z. (Eq x, Eq y, Eq j) => Dist z -> (z -> Dist j) -> (j -> x) -> (j -> y) -> Entropy
```


#### `divergence`

``` purescript
divergence :: forall x z. (Eq x) => Dist z -> (z -> Dist x) -> (z -> Dist x) -> Entropy
```


#### `nonCond`

``` purescript
nonCond :: forall a b c. (Dist Unit -> (Unit -> Dist b) -> c) -> Dist b -> c
```

Helper function for using `entropy` and `mutualInformation` with
non-conditional distributions.

#### `eqEnt`

``` purescript
instance eqEnt :: Eq Entropy
```


#### `ordEnt`

``` purescript
instance ordEnt :: Ord Entropy
```


#### `showEnt`

``` purescript
instance showEnt :: Show Entropy
```