module Lang.Util.OMap where

import Control.Applicative

import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord (comparing)

data OMap k a = OM Integer (Map k (Integer, a)) deriving (Show)

empty = OM 0 Map.empty

(!) :: Ord k => OMap k a -> k -> a
(OM _ m) ! k = snd (m Map.! k)

null :: OMap k a -> Bool
null (OM _ m) = Map.null m

size :: OMap k a -> Int
size (OM _ m) = Map.size m

member :: Ord k => k -> OMap k a -> Bool
member k (OM _ m) = Map.member k m

notMember :: Ord k => k -> OMap k a -> Bool
notMember k (OM _ m) = Map.notMember k m

lookup :: Ord k => k -> OMap k a -> Maybe a
lookup k (OM _ m) = snd <$> Map.lookup k m

findWithDefault :: Ord k => a -> k -> OMap k a -> a
findWithDefault def k (OM _ m) = case Map.lookup k m of
  Nothing     -> def
  Just (_, x) -> x

singleton :: k -> a -> OMap k a
singleton k x = OM 1 (Map.singleton k (0, x))

insert :: Ord k => k -> a -> OMap k a -> OMap k a
insert k x (OM n m) = OM (n + 1) (Map.insert k (n, x) m)

insertWith :: Ord k => (a -> a -> a) -> k -> a -> OMap k a -> OMap k a
insertWith f k x (OM n m) = OM (n + 1) (Map.insertWith (g f) k (n, x) m)
  where
    g f (b, x) (_, y) = (b, f x y)

delete :: Ord k => k -> OMap k a -> OMap k a
delete k (OM n m) = OM n (Map.delete k m)

elems :: OMap k a -> [a]
elems (OM _ m) = snd <$> Map.elems m

order :: OMap k a -> [a]
order (OM _ m) = snd <$> sortBy (comparing fst) (Map.elems m)

keys :: OMap k a -> [k]
keys (OM _ m) = Map.keys m

orderedKeys :: OMap k a -> [k]
orderedKeys (OM _ m) = fst <$> sortBy (comparing (\(_,(n,_)) -> n)) (Map.toList m)

toMap :: OMap k a -> Map k a
toMap (OM _ m) = Map.map snd m
