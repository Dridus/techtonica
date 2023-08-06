# What are this?

It's a calculator/planner toolbox similar to e.g.
[Satisfactory Calculator](https://satisfactory-calculator.com/en/planners/production) but
without a web app and GUI and for Techtonica.

# Why?

What, you don't play video games this way?

It was fun is the actual answer. Techtonica isn't (wasn't?) popular enough to have a complete
wiki or calculation assistance, so why not way overdo it?

# How do?

1. `direnv allow` or `nix develop` to enter the dev shell
2. `cabal repl`, `import Tech.Ghci`

## Commands

The `Tech.Ghci` module implements a simple CLI for mucking about, and maintains state in the
form of some global `IORef`s. Note this means if you `:reload` or exit the GHCi REPL all your
state goes away without warning, so make sure to `saveFactory`.

### General

- `history :: IO ()`

  Show up to last 50 changes to state as well as any undone states that could be redo-ed.

- `undo :: IO ()`

  Revert to the previous state.

- `redo :: IO ()`

  Un-undo.

### Factory manipulation

- `printFactory :: IO ()`

  Print the current factory's static structure, i.e. the machines and belts specified.

  E.g.:

  ```
    ghci> printFactory
    ┏━ 1: cluster of 1 planter/kindlevine: (130.000sec/cycle) 1 kindlevineSeed >-> 1 kindlevine
    ┠┄┄┄ 1 ↘ 2: belt carrying kindlevine
    ┣━ 2: cluster of 1 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    ┠┄┄┄ 2 ↘ 3: belt carrying kindlevineSticks
    ┗━ 3: cluster of 1 thresher/kindlevineSticks: (3.000sec/cycle) 1 kindlevineSticks >-> {4 kindlevineExtract, 4 plantmatterFiber}
    ghci> 
  ```

- `saveFactory :: FilePath -> IO ()`

  Save the current factory out as a YAML file.

  E.g.:

  ```
    ghci> saveFactory "test.yaml"
    ghci> 
  ```

- `loadFactory :: FilePath -> IO ()`

  Load a factory back from a YAML file, and also verify it.

  E.g.:
  ```
    ghci> clearFactory
    ghci> loadFactory "test.yaml"
    ghci> printFactory
    ┏━ 1: cluster of 1 planter/kindlevine: (130.000sec/cycle) 1 kindlevineSeed >-> 1 kindlevine
    ┠┄┄┄ 1 ↘ 2: belt carrying kindlevine
    ┣━ 2: cluster of 1 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    ┠┄┄┄ 2 ↘ 3: belt carrying kindlevineSticks
    ┗━ 3: cluster of 1 thresher/kindlevineSticks: (3.000sec/cycle) 1 kindlevineSticks >-> {4 kindlevineExtract, 4 plantmatterFiber}
    ghci> 
  ```

- `setFactory :: FactorySt -> IO ()`

  Replace the current factory state in GHCi with some specific factory.

  E.g.:
  ```
    ghci> clearFactory
    ghci> setFactory Tech.TestFixtures.linearSt
    ghci> printFactory
    ┏━ 1: cluster of 1 test/a1b1: (1.000sec/cycle) 1 testA >-> 1 testB
    ┠┄┄┄ 1 ↘ 2: belt carrying testB
    ┗━ 2: cluster of 1 test/b1c1: (1.000sec/cycle) 1 testB >-> 1 testC
    ghci> 
  ```

- `clearFactory :: IO ()`

  Big delete.


- `verifyFactory :: IO ()`

  Do various sanity checks on the current factory graph. Currently that means ensuring all belt
  edges have valid connections, both that they connect to valid nodes and that those nodes
  produce or consume the expected things.

  E.g.:
  ```
    ghci> printFactory
    ┏━ 1: cluster of 1 planter/kindlevine: (130.000sec/cycle) 1 kindlevineSeed >-> 1 kindlevine
    ┠┄┄┄ 1 ↘ 2: belt carrying kindlevine
    ┣━ 2: cluster of 1 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    ┠┄┄┄ 2 ↘ 3: belt carrying kindlevineSticks
    ┗━ 3: cluster of 1 thresher/kindlevineSticks: (3.000sec/cycle) 1 kindlevineSticks >-> {4 kindlevineExtract, 4 plantmatterFiber}
    ghci> verifyFactory
    Verify OK!

    ghci> addBelt 2 3 ironComponents
    ghci> printFactory
    ┏━━━━ 1: cluster of 1 planter/kindlevine: (130.000sec/cycle) 1 kindlevineSeed >-> 1 kindlevine
    ┠┄┄┄┄┄┄ 1 ↘ 2: belt carrying kindlevine
    ┣━━┳━ 2: cluster of 1 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    ┠┄┄┃┄┄┄ 2 ↘ 3: belt carrying kindlevineSticks
    ┃  ┠┄┄┄ 2 ↘ 3: belt carrying ironComponents
    ┗━━┻━ 3: cluster of 1 thresher/kindlevineSticks: (3.000sec/cycle) 1 kindlevineSticks >-> {4 kindlevineExtract, 4 plantmatterFiber}
    ghci> verifyFactory
    Warning: Belt downstream recipe does not accept requested item.
      2 ↘ 3: belt carrying ironComponents
      3: cluster of 1 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    Warning: Belt upstream recipe does not produce requested item.
      2 ↘ 3: belt carrying ironComponents
      2: cluster of 1 thresher/kindlevineSticks: (3.000sec/cycle) 1 kindlevineSticks >-> {4 kindlevineExtract, 4 plantmatterFiber}
    Verified with 2 warning(s).

    ghci> 
  ```

- `addCluster :: IO Recipe -> Quantity -> IO Node`

  Add a cluster (of machines) to the current factory. The first parameter is `IO Recipe` so
  that `findRecipe` fits in there. You can `pure` some recipe you already have let-bound also.

  E.g.:
  ```
    ghci> addCluster (findRecipe planter "kindlevine") 1
    4
  ```
- `editCluster :: Node -> (ClusterSt -> ClusterSt) -> IO ()`

  Edit an existing cluster in-place without detaching any belts as deleting it and recreating
  it would.

  E.g.:
  ```
    ghci> loadFactory "test.yaml"
    ghci> printFactory
    ┏━ 1: cluster of 1 planter/kindlevine: (130.000sec/cycle) 1 kindlevineSeed >-> 1 kindlevine
    ┠┄┄┄ 1 ↘ 2: belt carrying kindlevine
    ┣━ 2: cluster of 1 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    ┠┄┄┄ 2 ↘ 3: belt carrying kindlevineSticks
    ┗━ 3: cluster of 1 thresher/kindlevineSticks: (3.000sec/cycle) 1 kindlevineSticks >-> {4 kindlevineExtract, 4 plantmatterFiber}
    ghci> editCluster 2 (set quantity 2)
    ghci> printFactory
    ┏━ 1: cluster of 1 planter/kindlevine: (130.000sec/cycle) 1 kindlevineSeed >-> 1 kindlevine
    ┠┄┄┄ 1 ↘ 2: belt carrying kindlevine
    ┣━ 2: cluster of 2 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    ┠┄┄┄ 2 ↘ 3: belt carrying kindlevineSticks
    ┗━ 3: cluster of 1 thresher/kindlevineSticks: (3.000sec/cycle) 1 kindlevineSticks >-> {4 kindlevineExtract, 4 plantmatterFiber}
    ghci> 
  ```

- `delCluster :: Node -> IO ()`

  Remove a cluster from the current factory.

- `addBelt :: Node -> Node -> Item -> IO ()`

  `addBelt upstream downstream item`

  Add a belt carrying the given item between an upstream cluster and downstream cluster.

  E.g.:
  ```
    ghci> clearFactory
    ghci> addCluster (findRecipe planter "kindlevine") 1
    1
    ghci> addCluster (findRecipe thresher "kindlevine") 1
    2
    ghci> addBelt 1 2 kindlevine
    ghci> printFactory
    ┏━ 1: cluster of 1 planter/kindlevine: (130.000sec/cycle) 1 kindlevineSeed >-> 1 kindlevine
    ┠┄┄┄ 1 ↘ 2: belt carrying kindlevine
    ┗━ 2: cluster of 1 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    ghci> 
  ```

### Factory Planning

- `estimateFactory :: IO ()`

  Estimate the flows of the current factory configuration and output them.

  E.g.:
  ```
    ghci> loadFactory "test.yaml"
    ghci> printFactory
    ┏━ 1: cluster of 1 planter/kindlevine: (130.000sec/cycle) 1 kindlevineSeed >-> 1 kindlevine
    ┠┄┄┄ 1 ↘ 2: belt carrying kindlevine
    ┣━ 2: cluster of 1 thresher/kindlevine: (6.000sec/cycle) 1 kindlevine >-> {1 kindlevineSeed, 4 kindlevineSticks}
    ┠┄┄┄ 2 ↘ 3: belt carrying kindlevineSticks
    ┗━ 3: cluster of 1 thresher/kindlevineSticks: (3.000sec/cycle) 1 kindlevineSticks >-> {4 kindlevineExtract, 4 plantmatterFiber}
    ghci> estimateFactory
    ┏━━━━━━━ external resources ↓ 0.461/min kindlevineSeed
    ┠┄┄┄┄┄┄┄┄┄ external ↘ 1: belt carrying kindlevineSeed (0.461/min >-> 0.461/min)
    ┣━━━━━━━ 1: cluster of 1 planter/kindlevine: 0.461/min kindlevineSeed >-> 0.461/min kindlevine
    ┠┄┄┄┄┄┄┄┄┄ 1 ↘ 2: belt carrying kindlevine (0.461/min >-> 0.461/min)
    ┣━━┳━━━━ 2: cluster of 1 thresher/kindlevine: 9.999/min kindlevine >-> {9.999/min kindlevineSeed, 39.999/min kindlevineSticks}
    ┃  ┠┄┄┄┄┄┄ 2 ↘ 3: belt carrying kindlevineSticks (39.999/min >- short 39.999/min -> 79.999/min)
    ┠┄┄┃┄┄┄┄┄┄ 2 ↘ external: belt carrying kindlevineSeed (9.999/min >-> 9.999/min)
    ┃  ┣━━┳━ 3: cluster of 1 thresher/kindlevineSticks: 19.999/min kindlevineSticks >-> {79.999/min kindlevineExtract, 79.999/min plantmatterFiber}
    ┃  ┠┄┄┃┄┄┄ 3 ↘ external: belt carrying plantmatterFiber (79.999/min >-> 79.999/min)
    ┃  ┃  ┠┄┄┄ 3 ↘ external: belt carrying kindlevineExtract (79.999/min >-> 79.999/min)
    ┗━━┻━━┻━ byproducts ↓ {79.999/min kindlevineExtract, 9.999/min kindlevineSeed, 79.999/min plantmatterFiber}
    ghci> 
  ```

- `proposeFactory :: Image Rate -> ProposalConstraints Last -> IO ()`

  Propose additions to the factory structure such that the resulting factory produces as
  byproducts all of the items given.

  E.g.:
  ```
    ghci> clearFactory 
    ghci> proposeFactory (One ("ironComponents", 1)) (assumeGiven (Just "ironIngot"))
    Generated 1 proposals, 1 successful, 0 not.
    Proposal 1 succeeded after 1 steps (add assembler/ironComponents to get 60.000/min ironComponents for goal)
      
        Structure:
            new 1: cluster of 2 assembler/ironComponents: (4.000sec/cycle) 1 ironIngot >-> 2 ironComponents
        Estimated:
           ┏━ external-1 ↓ 30.000/min ironIngot
           ┠┄┄┄ external-1 ↘ 1: belt carrying ironIngot (30.000/min >-> 30.000/min)
           ┣━ 1: cluster of 2 assembler/ironComponents: 30.000/min ironIngot >-> 60.000/min ironComponents
           ┠┄┄┄ 1 ↘ external-2: belt carrying ironComponents (60.000/min >-> 60.000/min)
           ┗━ byproducts-2 ↓ 60.000/min ironComponents
  ```

  As implied by the example, additional constraints can be given to control how much is added
  versus assumed and whether overflows in the factory are proposed to be wired into new
  construction.

  `ProposalConstraints Last` is a `Monoid`, so you use `<>` to combine constraints. Constraints
  currently understood:

  - `assumeGiven :: Foldable f => f Item -> ProposalConstraints Last`

    Assume the named item(s) are available as feedstock. If an item isn't assumed given, then
    the planner will try to find recipes to produce the intermediate items required, and fail
    if there are no such recipes.

  - `doUseOverflows, dontUseOverflows :: ProposalConstraints Last`

    When looking for required intermediate items do or do not (the default is do not) consider
    overflowing belts and propose splitting those belts to consume the overflow in newly
    proposed machines.

    This is not enabled by default because the resulting factory might be messier. It's also
    possibly very wrong as at the moment the estimator naïvely splits products evenly among
    consumers which means some belts will be over and some under, when in fact all product is
    being used.

  Multiple proposals might be generated in the case where multiple recipes are found which
  produce a required item. `proposeFactory` only prints the first but also prints how many
  proposals in total were generated. Use `printProposals` to print them all, or `printProposal`
  to print proposals one at a time.

  A proposal can be accepted with `acceptProposal`. Proposals can't be cleared (rejected)
  explicitly, but will become invalidated if other changes are made to the factory state. If
  changes are made and the proposals invalidated, you have to `proposeFactory` again if you
  want revised proposals.

- `printProposals :: IO ()`

  Print all proposals previously generated by `proposeFactory`.

- `printProposal :: Int -> IO ()`

  Print a single 1-indexed proposal previously generated by `proposeFactory`.

- `acceptProposal :: Int -> IO ()`

  Accept a single 1-indexed proposal previously generated by `proposeFactory`.

  If you regret, see also `undo`.

### Recipes and Items

- `loadRecipes :: FilePath -> IO ()`

  Load recipes and items from a YAML file.

- `saveRecipes :: FilePath -> IO ()`

  Save recipes and items to a YAML file.

- `findRecipe :: Machine -> RecipeIdentifier -> IO Recipe`

  Look for a recipe, `fail`ing if not found. This looks in the current recipes state, which
  starts out with only the builtin recipes but you can add to.

  Intended for use as the first argument to `addCluster`.

- `listAllRecipes :: IO ()`

  List all recipes currently in state, for all machines.

- `listRecipes :: Machine -> IO ()`

  List all recipes currently in state, for a single machine.

- `addRecipe :: Machine -> RecipeIdentifier -> NominalDiffTime -> Transfer Quantity -> IO ()`

  `addRecipe machine recipeId cycleTime transferFunction`

  Register a recipe so you can look it up by identifier later.

  You don't need to `addRecipe` a recipe, it just registers the name. You can also just mention
  the recipe when adding a cluster.

- `editRecipe :: Machine -> RecipeIdentifier -> (Recipe -> Recipe) -> IO ()`

  Edit a recipe in-place. Not different from `delRecipe` and `addRecipe` other than being more
  convenient.

- `delRecipe :: Machine -> RecipeIdentifier -> IO ()`

  Like `addRecipe`, but more violent. And with fewer questions.

- `listItems :: IO ()`

  List all registered items.

- `addItem :: Item -> IO ()`

  Add an item to the set of known items.

- `delItem :: Item -> IO ()`

  Delete an item to the set of known items.

## Machines?

`:browse Tech.Machine`

## Items?

`listItems`. They come from `recipes.yaml`.

## Recipes?

`listAllRecipes` or `listRecipe`. They come from `recipes.yaml`.

# Data Model?

## Scalars

- `Item` represents a single type of item, e.g. `ironIngot`. They are defined in `Tech.Items`
  though you can make up new ones as you like. `newtype` over `Text`.

- `Machine` represents a single type of machine, e.g. `assembler`. They are defined in
  `Tech.Machines`. `newtype` over `Text`.

- `RecipeIdentifier` identifies a particular recipe. They are unique within the scope of a
  particular machine. Nothing will prevent you from having two recipes with the same identifier
  and machine but distinct parameters, other than zalgo. `newtype` over `Text`.

- `Quantity` represents a unit quantity of an item, usually per machine cycle. `newtype` over
  `Data.Fixed.Micro`.

- `Rate` represents a quantity of items per unit time, typically per second internally. The
  prettyprinter shows them divided to per minute rates usually.

### `FactorySt`, `FactoryProp`, and `FactoryDy`

Factories come in several flavors:

* `FactorySt`: Static structure which describes the clusters (machines) and connections between
  them, but doesn't include information about flow.
* `FactoryDy`: Dynamic flow which augments `FactorySt` with flow information.
  `Tech.Planner.estimate` is the primary way to go from static to dynamic. There isn't a
  corresponding reverse, though the `FactoryDy` is a strict superset of the corresponding
  `FactorySt`.
* `FactoryProp`: Proposed static structure which augments `FactorySt` with flags indicating that
  clusters or belts are new relative to some original `FactorySt`. `factoryStFromProp` discards
  the `IsNew` flag.

Factories are [FGL](https://hackage.haskell.org/package/fgl) directed acyclic graphs with
`ClusterSt` (or `ClusterDy`) nodes representing clusters of identical machines and `BeltSt`
(`BeltDy`) edges which represent belts carrying items between clusters.

## `BeltSt` and `BeltDy`

Belts are fairly straightforward at the moment. Statically they identify which item they carry,
and belts with mixed items aren't supported at the moment.

Dynamically they also have the estimated item flow rate measured as entering items/sec (printed
as /min) and exiting, so they show overpressure and underpressure.

## `ClusterSt` and `ClusterDy`

Clusters are described by a parallelism quantity representing the number of instances of the
machine in question, along with a `Recipe` which controls how much they consume of what items
and identifies the machine.

Dynamically, a cluster has its item rates per time calculated versus the recipe expressing it
in terms of full unit quantities and a cycle time.

## `Recipe`, `Transfer`, `Image`

`Recipe` names a `Transfer` function and how long the cycle time of that transfer function in
a particular machine.

`Recipe`s are keyed with the `RecipeKey` type, which has a `Machine` and `RecipeIdentifier`.
Note that equality and ordering for `Recipe` is defined and in terms of this key. Zalgo might
come to visit if you change a recipe's definition but not its key.

`Transfer` functions are `Image`s of their input and outputs and typically created with pattern
synonyms. `Image`s are in turn maps of `Item` to some quantity `q`, which is typically `Quantity`
in `Recipe`s and `Rate` for `ClusterDy` or `BeltDy`.

`Transfer`s and thus `Recipe`s are typically constructed using a pattern synonyms to simplify down
from the general case of `Map Item Quantity -> Map Item Quantity`:

```haskell
ghci> putDocLn . ppTransfer ppQuantity $ [(ironIngot, 1)] :>>: [(ironComponents, 1)]
1 ironIngot >-> 1 ironComponents
ghci> putDocLn . ppTransfer ppQuantity $ [(kindlevine, 1)] :>>: [(kindlevineSticks, 1), (kindlevineSeed, 1)]
1 kindlevine >-> {1 kindlevineSeed, 1 kindlevineSticks}
```

```haskell
recipe :: RecipeIdentifier -> NominalDiffTime -> Transfer Quantity -> Recipe
recipe rid = Recipe (RecipeKey thresher rid)

recipe' :: (Item, Quantity) -> NominalDiffTime -> [(Item, Quantity)] -> Recipe
recipe' src@(i, _) time outs = recipe (RecipeIdentifier . unItem $ i) time ([src] :>>: outs)

kindlevine, kindlevineSticks :: Recipe
kindlevine = recipe' (I.kindlevine, 1) 6.00 (Pair (I.kindlevineSeed, 1) (I.kindlevineSticks, 4))
kindlevineSticks = recipe' (I.kindlevineSticks, 1) 3.00 (Pair (I.kindlevineExtract, 4) (I.plantmatterFiber, 4))
```

The `Transfer` synonym is `:>>:` and takes a list of pairs as input and output.

- `(i, q) :->-: (i, q)`: one item (type) in, one item out.
- `(i, q) :->=: image`: one item in, potentially many out.
- `image :=>-: (i, q)`: potentially many in, one out.
- `image :=>=: image`: many in, many out. Equivalent to the `Transfer` constructor.

And there are synonyms for `Image` as well:

- `One (i, q)` = `Map.singleton i q`
- `Pair (i1, q1) (i2, q2)` = `Map.fromList [(i1, q1), (i2, q2)]`
- `Many [(i1, q1), .., (ix, qx)]` = `Map.fromList [(i1, q1), .., (ix, qx)]`

# Other

## Lens?

Very yes. In particular using `makeLensesWith underscoreFields` so accessors are punned. E.g.
`view quantity` works on both `ClusterSt` and `ClusterDy` by way of the auto-generated
`Has_quantity` typeclass.

## Pretty printing?

Also very yes. Most of the pretty printing is implemented in `Tech.Pretty`. Uses
[`prettyprinter`](https://hackage.haskell.org/package/prettyprinter) and
[`prettyprinter-ansi-terminal`](https://hackage.haskell.org/package/prettyprinter-ansi-terminal)
which are based on the famous Wadler/Leijen formulatio

## YAML?

By way of Aeson. A set of `*Spec` types mirrors the `*St` types to represent the serialized
version. `Spec` types are internally keyed as opposed to externally keyed and have symbolic
references to recipes rather than direct inclusion.

E.g.:

```haskell
data ClusterSpec = ClusterSpec
  { _clusterSpec_node :: Node
  , _clusterSpec_recipeKey :: RecipeKey
  , _clusterSpec_quantity :: Quantity
  }

data ClusterSt = ClusterSt
  { _clusterSt_recipe :: Recipe
  , _clusterSt_quantity :: Quantity
  }
```

See `Tech.Store`.

## Automated tests?

Yep. Uses [tasty](https://hackage.haskell.org/package/tasty),
[`tasty-hunit`](https://hackage.haskell.org/package/tasty-hunit),
[`tasty-quickcheck`](https://hackage.haskell.org/package/tasty-quickcheck), as one do.

## Nix

For sure. Uses [`haskell-flake`](https://github.com/srid/haskell-flake) and thus the
[`flake-parts`](https://flake.parts/) modules, based on on the example
[`haskell-template`](https://github.com/srid/haskell-template).

h/t to @srid et al.

This uses flakes, so you'll need a decently recent nix, and maybe to enable the `flakes` and
`nix-command` `experimental-features`.

## TODO

https://github.com/Dridus/techtonica/issues

## Help?

If that's an offer, sure. https://github.com/Dridus/techtonica/issues

