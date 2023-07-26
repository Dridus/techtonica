module Tech.Types where

import Control.Lens (Wrapped (type Unwrapped), each, over, view, _Wrapped')
import Control.Lens.TH (makeLensesWith, makeWrapped, underscoreFields)
import Data.Align (alignWith)
import Data.Graph.Inductive (Gr)
import Data.Map.Strict qualified as Map
import Data.These (fromThese)
import Data.Time (NominalDiffTime)
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

newtype Rate = Rate {unRate :: Rational}
deriving newtype instance Eq Rate
deriving newtype instance Fractional Rate
deriving newtype instance Num Rate
deriving newtype instance Real Rate
deriving newtype instance RealFrac Rate
deriving newtype instance Ord Rate
makeWrapped ''Rate
instance Show Rate where
  showsPrec d (Rate i) = showParen (d > 9) $ showString "Rate " . showsPrec (d + 1) i

-- * Recipes and Flows

-- ** Item-indexed Images

type Image q = Map Item q

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
makeLensesWith underscoreFields ''Transfer

infix 4 :=>=:, :->=:, :=>-:, :->-:
pattern (:=>=:) :: Image q -> Image q -> Transfer q
pattern ins :=>=: outs <- Transfer ins outs
  where
    ins :=>=: outs = Transfer ins outs
pattern (:=>-:) :: Image q -> (Item, q) -> Transfer q
pattern ins :=>-: out1 <- Transfer ins (One out1)
  where
    ins :=>-: out1 = Transfer ins (One out1)
pattern (:->=:) :: (Item, q) -> Image q -> Transfer q
pattern in1 :->=: outs <- Transfer (One in1) outs
  where
    in1 :->=: outs = Transfer (One in1) outs
pattern (:->-:) :: (Item, q) -> (Item, q) -> Transfer q
pattern in1 :->-: out1 <- Transfer (One in1) (One out1)
  where
    in1 :->-: out1 = Transfer (One in1) (One out1)

infixr 7 `mulT`
mulT :: (Wrapped q, Unwrapped q ~ n, Num n) => n -> Transfer q -> Transfer q
mulT (mulI -> f) = over inputs f . over outputs f

infixr 7 `divT`
divT :: (Wrapped q, Unwrapped q ~ n, Fractional n) => Transfer q -> n -> Transfer q
divT t n = over inputs (`divI` n) . over outputs (`divI` n) $ t

-- ** Machine Recipes

data RecipeKey = RecipeKey
  { _recipeKey_machine :: Machine
  , _recipeKey_identifier :: RecipeIdentifier
  }
deriving stock instance Eq RecipeKey
deriving stock instance Ord RecipeKey
deriving stock instance Show RecipeKey
makeLensesWith underscoreFields ''RecipeKey

-- Invariant key -> (cycleTime, transfer)
data Recipe = Recipe
  { _recipe_key :: RecipeKey
  , _recipe_cycleTime :: NominalDiffTime
  , _recipe_transfer :: Transfer Quantity
  }
deriving stock instance Show Recipe
makeLensesWith underscoreFields ''Recipe
instance Eq Recipe where (==) = (==) `on` view key
instance Ord Recipe where compare = compare `on` view key

infixr 7 `mulR`
mulR :: Rational -> Recipe -> Recipe
mulR = over transfer . mulT

infixr 7 `divR`
divR :: Recipe -> Rational -> Recipe
divR r n = over transfer (`divT` n) r

-- * Factories

-- ** Belts carrying an item between units

-- *** Belt structure

newtype BeltSt = BeltSt
  { _beltSt_item :: Item
  }
deriving stock instance Eq BeltSt
deriving stock instance Ord BeltSt
deriving stock instance Show BeltSt
makeLensesWith underscoreFields ''BeltSt

-- *** Belt dynamically

data BeltDy = BeltDy
  { _beltDy_item :: Item
  , _beltDy_entering :: Rate
  , _beltDy_exiting :: Rate
  }
deriving stock instance Eq BeltDy
deriving stock instance Ord BeltDy
deriving stock instance Show BeltDy
makeLensesWith underscoreFields ''BeltDy

shortfall :: BeltDy -> Rate
shortfall (BeltDy _ enter exit) = max 0 (exit - enter)

overflow :: BeltDy -> Rate
overflow (BeltDy _ enter exit) = max 0 (enter - exit)

-- ** Clusters of machines running a recipe

-- *** Cluster statically

data ClusterSt = ClusterSt
  { _clusterSt_recipe :: Recipe
  , _clusterSt_quantity :: Quantity
  }
deriving stock instance Eq ClusterSt
deriving stock instance Ord ClusterSt
deriving stock instance Show ClusterSt
makeLensesWith underscoreFields ''ClusterSt

-- *** Cluster dynamically

data ClusterDy = ClusterDy
  { _clusterDy_recipe :: Recipe
  , _clusterDy_quantity :: Quantity
  , _clusterDy_transfer :: Transfer Rate
  }
deriving stock instance Eq ClusterDy
deriving stock instance Show ClusterDy
makeLensesWith underscoreFields ''ClusterDy

-- ** Factory graphs

type FactorySt = Gr ClusterSt BeltSt
type FactoryDy = Gr ClusterDy BeltDy
