module Tech.Graph where

import Control.Lens (
  At (at),
  Each (each),
  FoldableWithIndex (ifoldMap),
  FunctorWithIndex (imap),
  Index,
  Indexable (indexed),
  IndexedTraversal',
  IxValue,
  Ixed (ix),
  Lens,
  Lens',
  TraversableWithIndex (itraverse),
  Traversal,
  from,
  set,
  view,
  _1,
  _2,
  _3,
  _4, Iso,
 )
import Control.Lens.TH (makePrisms)
import Data.Graph.Inductive (Adj, Context, DynGraph, Node)
import Data.Graph.Inductive qualified as Gr
import Data.List (partition)

-- * Utility functions

insContext :: DynGraph gr => Context n e -> gr n e -> gr n e
insContext = (Gr.&)

insContextMay :: DynGraph gr => Maybe (Context n e) -> gr n e -> gr n e
insContextMay = maybe id insContext

-- * Lenses into 'Context' and 'Adj'

preAdjs :: Lens' (Context n e) (Adj e)
preAdjs = _1

nodeNumber :: Lens' (Context n e) Node
nodeNumber = _2

nodeLabel :: Lens (Context n e) (Context n' e) n n'
nodeLabel = _3

sucAdjs :: Lens' (Context n e) (Adj e)
sucAdjs = _4

adjsWith :: Node -> Lens' (Adj e) [e]
adjsWith n f adjs =
  (<> adjos) . fmap (,n) <$> f bs
 where
  (adjns, adjos) = partition ((== n) . snd) adjs
  bs = fst <$> adjns

allAdjLabels :: Traversal (Context n e) (Context n e') e e'
allAdjLabels f (pre, n, l, suc) = (,n,l,) <$> (each . _1) f pre <*> (each . _1) f suc

-- * Graph optics

-- ** Traversing complete 'Context's

-- |Caution: only successor adjacencies will be honored. The traversal function will observe
-- both predecessor and successor adjacencies, but only successor adjacencies will make their
-- way into the resulting graph. This is to avoid all links being duplicated.
contexts :: DynGraph gr => IndexedTraversal' Node (gr n e) (Context n e)
contexts f gr = foldl' g (pure gr) (Gr.nodes gr)
 where
  g fgr n = insContext <$> (set _1 [] <$> indexed f n (Gr.context gr n)) <*> fgr

-- ** Traversing disjunct 'Context's

newtype DisjunctContexts gr n e = DisjunctContexts {unDisjunctContexts :: gr n e}
makePrisms ''DisjunctContexts

disjunctContexts :: Iso (gr n' e') (gr n e) (DisjunctContexts gr n' e') (DisjunctContexts gr n e)
disjunctContexts = from _DisjunctContexts

type instance Index (DisjunctContexts gr n e) = Node
type instance IxValue (DisjunctContexts gr n e) = Context n e

instance DynGraph gr => Ixed (DisjunctContexts gr n e) where
  ix n f = _DisjunctContexts $ Gr.match n >>> \(mctx, g) -> maybe (pure g) (fmap (`insContext` g) . f) mctx

instance DynGraph gr => At (DisjunctContexts gr n e) where
  at n f = _DisjunctContexts $ Gr.match n >>> \(mctx, g) -> (`insContextMay` g) <$> f mctx

instance DynGraph gr => Each (DisjunctContexts gr n e) (DisjunctContexts gr n' e') (Context n e) (Context n' e') where
  each f = _DisjunctContexts $ Gr.ufold (\ctx fg -> insContext <$> f ctx <*> fg) (pure Gr.empty)

-- ** Traversing labelled nodes

newtype Nodes gr e n = Nodes {unNodes :: gr n e}
makePrisms ''Nodes

nodes :: Iso (gr n' e') (gr n e) (Nodes gr e' n') (Nodes gr e n)
nodes = from _Nodes

type instance Index (Nodes gr e n) = Node
type instance IxValue (Nodes gr e n) = n

instance DynGraph gr => Ixed (Nodes gr e n) where
  ix n = _Nodes . disjunctContexts . ix n . nodeLabel

instance DynGraph gr => At (Nodes gr e n) where
  at n = _Nodes . disjunctContexts . at n . f
   where
    f g mctx =
      (fmap . fmap)
        (maybe ([],n,,[]) (\(pre, _, _, suc) -> (pre,n,,suc)) mctx)
        (g (view nodeLabel <$> mctx))

instance DynGraph gr => Each (Nodes gr e n) (Nodes gr e n') n n' where
  each = _Nodes . disjunctContexts . each . nodeLabel

instance DynGraph gr => Functor (Nodes gr e) where
  fmap f = Nodes . Gr.nmap f . unNodes

instance DynGraph gr => FunctorWithIndex Node (Nodes gr e) where
  imap f = Nodes . Gr.gmap (\(pre, n, l, suc) -> (pre, n, f n l, suc)) . unNodes

instance DynGraph gr => Foldable (Nodes gr e) where
  foldMap f = foldMap (f . snd) . Gr.labNodes . unNodes

instance DynGraph gr => FoldableWithIndex Node (Nodes gr e) where
  ifoldMap f = foldMap (uncurry f) . Gr.labNodes . unNodes

instance DynGraph gr => Traversable (Nodes gr e) where
  traverse f = fmap Nodes . Gr.ufold g (pure Gr.empty) . unNodes
   where
    g ctx fg = insContext <$> _3 f ctx <*> fg

instance DynGraph gr => TraversableWithIndex Node (Nodes gr e) where
  itraverse f =
    fmap Nodes
      . Gr.ufold (\ctx fg -> insContext <$> _3 (f (view _2 ctx)) ctx <*> fg) (pure Gr.empty)
      . unNodes

-- ** Traversing labelled edges

newtype Edges gr n e = Edges {unEdges :: gr n e}
makePrisms ''Edges

edges :: Iso (gr n' e') (gr n e) (Edges gr n' e') (Edges gr n e)
edges = from _Edges

type instance Index (Edges gr n e) = (Node, Node)
type instance IxValue (Edges gr n e) = [e]

instance DynGraph gr => Ixed (Edges gr n e) where
  ix (np, ns) = _Edges . disjunctContexts . ix np . sucAdjs . adjsWith ns

-- |Caution: errors if nodes that aren't existent are used as the index.
instance DynGraph gr => At (Edges gr n e) where
  at (np, ns) = _Edges . disjunctContexts . at np . f
   where
    f g (fromMaybe (error $ show np <> " doesn't exist in graph") -> (pre, _, l, suc)) =
      Just . (pre,np,l,) <$> adjsWith ns (fmap (fromMaybe []) . g . Just) suc

instance DynGraph gr => Each (Edges gr n e) (Edges gr n e') e e' where
  each = _Edges . disjunctContexts . each . allAdjLabels

instance DynGraph gr => Functor (Edges gr n) where
  fmap f = Edges . Gr.emap f . unEdges

instance DynGraph gr => FunctorWithIndex (Node, Node) (Edges gr n) where
  imap f = Edges . Gr.gmap g . unEdges
   where
    g (pre, n, l, suc) = (hp <$> pre, n, l, hs <$> suc)
     where
      hp (e, np) = (f (np, n) e, np)
      hs (e, ns) = (f (n, ns) e, ns)

instance DynGraph gr => Foldable (Edges gr n) where
  foldMap f = foldMap (\(_, _, e) -> f e) . Gr.labEdges . unEdges

instance DynGraph gr => FoldableWithIndex (Node, Node) (Edges gr n) where
  ifoldMap f = foldMap (\(np, ns, e) -> f (np, ns) e) . Gr.labEdges . unEdges

instance DynGraph gr => Traversable (Edges gr n) where
  traverse f = fmap Edges . Gr.ufold g (pure Gr.empty) . unEdges
   where
    g (pre, n, l, suc) fg =
      insContext
        <$> ((,n,l,) <$> traverse hp pre <*> traverse hs suc)
        <*> fg
     where
      hp (e, np) = (,np) <$> f e
      hs (e, ns) = (,ns) <$> f e

instance DynGraph gr => TraversableWithIndex (Node, Node) (Edges gr n) where
  itraverse f = fmap Edges . Gr.ufold g (pure Gr.empty) . unEdges
   where
    g (pre, n, l, suc) fg =
      insContext
        <$> ((,n,l,) <$> traverse hp pre <*> traverse hs suc)
        <*> fg
     where
      hp (e, np) = (,np) <$> f (np, n) e
      hs (e, ns) = (,ns) <$> f (n, ns) e
