module Tech.Types where

import Control.Lens (Wrapped (type Unwrapped), each, over, view, _Wrapped')
import Control.Lens.TH (makeLensesWith, makeWrapped)
import Data.Align (alignWith)
import Data.Graph.Inductive (Gr, Node, nodeRange)
import Data.Graph.Inductive.Graph qualified as Gr
import Data.Map.Strict qualified as Map
import Data.These (fromThese)
import Data.Time (NominalDiffTime)
import Tech.LensOptions (techFields)
import Text.Show (Show (showsPrec), showParen, showString)

-- * Symbols

-- ** Items

newtype Item = Item {unItem :: Text}
deriving newtype instance Eq Item
deriving newtype instance IsString Item
deriving newtype instance Ord Item
makeWrapped ''Item
instance Show Item where
  showsPrec d (Item i) = showParen (d > 9) $ showString "Item " . showsPrec (d + 1) i

-- ** Machines

newtype Machine = Machine {unMachine :: Text}
deriving newtype instance Eq Machine
deriving newtype instance IsString Machine
deriving newtype instance Ord Machine
makeWrapped ''Machine
instance Show Machine where
  showsPrec d (Machine i) = showParen (d > 9) $ showString "Machine " . showsPrec (d + 1) i

-- ** Recipe identifiers

newtype RecipeIdentifier = RecipeIdentifier {unRecipeIdentifier :: Text}
deriving newtype instance Eq RecipeIdentifier
deriving newtype instance IsString RecipeIdentifier
deriving newtype instance Ord RecipeIdentifier
makeWrapped ''RecipeIdentifier
instance Show RecipeIdentifier where
  showsPrec d (RecipeIdentifier i) = showParen (d > 9) $ showString "RecipeIdentifier " . showsPrec (d + 1) i

-- * Measures

-- ** Cluster quantities

newtype Quantity = Quantity {unQuantity :: Rational}
deriving newtype instance Eq Quantity
deriving newtype instance Fractional Quantity
deriving newtype instance Num Quantity
deriving newtype instance Real Quantity
deriving newtype instance RealFrac Quantity
deriving newtype instance Ord Quantity
makeWrapped ''Quantity
instance Show Quantity where
  showsPrec d (Quantity i) = showParen (d > 9) $ showString "Quantity " . showsPrec (d + 1) i

-- ** Clusters per second

newtype PerMinute = PerMinute {unPerMinute :: Rational}
deriving newtype instance Eq PerMinute
deriving newtype instance Fractional PerMinute
deriving newtype instance Num PerMinute
deriving newtype instance Real PerMinute
deriving newtype instance RealFrac PerMinute
deriving newtype instance Ord PerMinute
makeWrapped ''PerMinute
instance Show PerMinute where
  showsPrec d (PerMinute i) = showParen (d > 9) $ showString "PerMinute " . showsPrec (d + 1) i

-- * Recipes and Flows

-- ** Item-indexed Images

type Image q = Map Item q

pattern Many :: (Ord k) => () => [(k, v)] -> Map k v
pattern Many ps <- (Map.toList -> ps)
  where
    Many ps = Map.fromList ps

pattern Pair :: (Ord k) => () => (k, v) -> (k, v) -> Map k v
pattern Pair p q <- (Map.toList -> [p, q])
  where
    Pair p q = Map.fromList [p, q]

pattern One :: (k, v) -> Map k v
pattern One p <- (Map.toList -> [p])
  where
    One p = uncurry Map.singleton p

negateI :: Num q => Image q -> Image q
negateI = fmap negate

infixl 6 `addI`
addI :: Num q => Image q -> Image q -> Image q
addI = Map.unionWith (+)

infixl 6 `subI`
subI :: Num q => Image q -> Image q -> Image q
subI = alignWith (uncurry (-) . fromThese 0 0)

infixl 7 `mulI`
mulI :: (Wrapped q, Unwrapped q ~ n, Num n) => n -> Image q -> Image q
mulI = over (each . _Wrapped') . (*)

infixl 7 `divI`
divI :: (Wrapped q, Unwrapped q ~ n, Fractional n) => Image q -> n -> Image q
divI i n = over (each . _Wrapped') (/ n) i

newtype SumImage q = SumImage {getSumImage :: Image q}
makeWrapped ''SumImage
instance forall q. Num q => Semigroup (SumImage q) where
  (<>) = coerce (addI :: Image q -> Image q -> Image q)
instance Num q => Monoid (SumImage q) where
  mempty = SumImage Map.empty
  mconcat = foldl' (<>) mempty
  {-# INLINE mconcat #-}

-- ** Item-indexed transfer functions

data Transfer q = Transfer
  { _transfer_inputs :: Image q
  , _transfer_outputs :: Image q
  }
deriving stock instance Eq q => Eq (Transfer q)
deriving stock instance Functor Transfer
deriving stock instance Ord q => Ord (Transfer q)
deriving stock instance Show q => Show (Transfer q)
makeLensesWith techFields ''Transfer

infix 4 :>>:
pattern (:>>:) :: [(Item, q)] -> [(Item, q)] -> Transfer q
pattern ins :>>: outs <- Transfer (Many ins) (Many outs)
  where
    ins :>>: outs = Transfer (Many ins) (Many outs)

infixr 7 `mulT`
mulT :: (Wrapped q, Unwrapped q ~ n, Num n) => n -> Transfer q -> Transfer q
mulT (mulI -> f) = over fInputs f . over fOutputs f

infixr 7 `divT`
divT :: (Wrapped q, Unwrapped q ~ n, Fractional n) => Transfer q -> n -> Transfer q
divT t n = over fInputs (`divI` n) . over fOutputs (`divI` n) $ t

-- ** Machine Recipes

data RecipeKey = RecipeKey
  { _recipeKey_machine :: Machine
  , _recipeKey_identifier :: RecipeIdentifier
  }
deriving stock instance Eq RecipeKey
deriving stock instance Ord RecipeKey
deriving stock instance Show RecipeKey
makeLensesWith techFields ''RecipeKey

-- Invariant: key -> (cycleTime, transfer)
data Recipe = Recipe
  { _recipe_key :: RecipeKey
  , _recipe_cycleTime :: NominalDiffTime
  , _recipe_transfer :: Transfer Quantity
  }
deriving stock instance Show Recipe
makeLensesWith techFields ''Recipe
instance Eq Recipe where (==) = (==) `on` view fKey
instance Ord Recipe where compare = compare `on` view fKey

infixr 7 `mulR`
mulR :: Rational -> Recipe -> Recipe
mulR = over fTransfer . mulT

infixr 7 `divR`
divR :: Recipe -> Rational -> Recipe
divR r n = over fTransfer (`divT` n) r

-- * Factories

-- ** Belts carrying an item between units

-- *** Belt structure

newtype BeltSt = BeltSt
  { _beltSt_item :: Item
  }
deriving stock instance Eq BeltSt
deriving stock instance Ord BeltSt
deriving stock instance Show BeltSt
makeLensesWith techFields ''BeltSt

-- *** Belt dynamically

data BeltDy = BeltDy
  { _beltDy_static :: BeltSt
  , _beltDy_entering :: PerMinute
  , _beltDy_exiting :: PerMinute
  }
deriving stock instance Eq BeltDy
deriving stock instance Ord BeltDy
deriving stock instance Show BeltDy
makeLensesWith techFields ''BeltDy
instance Has_fItem BeltDy Item where fItem = fStatic . fItem

shortfall :: BeltDy -> PerMinute
shortfall b = max 0 (view fExiting b - view fEntering b)

overflow :: BeltDy -> PerMinute
overflow b = max 0 (view fEntering b - view fExiting b)

-- ** Clusters of machines running a recipe

-- *** Cluster statically

data ClusterSt = ClusterSt
  { _clusterSt_recipe :: Recipe
  , _clusterSt_quantity :: Quantity
  }
deriving stock instance Eq ClusterSt
deriving stock instance Ord ClusterSt
deriving stock instance Show ClusterSt
makeLensesWith techFields ''ClusterSt
instance Has_fMachine ClusterSt Machine where fMachine = fRecipe . fKey . fMachine
instance Has_fCycleTime ClusterSt NominalDiffTime where fCycleTime = fRecipe . fCycleTime

-- *** Cluster dynamically

data ClusterDy = ClusterDy
  { _clusterDy_static :: ClusterSt
  , _clusterDy_transfer :: Transfer PerMinute
  }
deriving stock instance Eq ClusterDy
deriving stock instance Show ClusterDy
makeLensesWith techFields ''ClusterDy
instance Has_fRecipe ClusterDy Recipe where fRecipe = fStatic . fRecipe
instance Has_fMachine ClusterDy Machine where fMachine = fRecipe . fKey . fMachine
instance Has_fCycleTime ClusterDy NominalDiffTime where fCycleTime = fRecipe . fCycleTime
instance Has_fQuantity ClusterDy Quantity where fQuantity = fStatic . fQuantity

-- ** Factory graphs

type FactorySt = Gr ClusterSt BeltSt
type FactoryDy = Gr ClusterDy BeltDy

newNode :: Gr a b -> Node
newNode g = if Gr.isEmpty g then 1 else succ . snd $ nodeRange g
