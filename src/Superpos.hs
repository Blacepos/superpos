module Superpos
( Superpos(Superpos, getSuperpos)
, sumWeights
, normWeights
, collapse
, simplify
) where

import System.Random (randomRIO, Random)
import Control.Monad.IO.Class ( MonadIO )
import Data.Bifunctor ( Bifunctor(second, first) )
import Data.List (groupBy, sortBy, foldl1', foldl')

-- | A value which is in multiple states at once until collapsed. Represented as a list of values and weights.
newtype Superpos p v = Superpos { getSuperpos :: [(v, p)] } deriving(Eq, Show)

-- | Calculates the sum of all weights in the superposition
sumWeights :: Num b => Superpos b a -> b
sumWeights (Superpos p) = foldl' (\s (_, b) -> s+b) 0 p

-- | Normalizes the weights in the superpositino to add to 1
normWeights :: Fractional b => Superpos b a -> Superpos b a
normWeights s@(Superpos p) = Superpos $ map (\(v, pr) -> (v, pr/tot)) p
    where tot = sumWeights s

-- | Pick a random value based on probability weights i.e., "collapse" the superposition.
-- Raises an exception if there are no values (the list is empty).
-- Uses system entropy for randomness as implemented in randomRIO.
collapse :: (Fractional b, Random b, Ord b, MonadIO m) => Superpos b a -> m a
collapse (Superpos s) = do
    let (vs, ps) = unzip s
    let psums = partialSumR ps
    let psv = zip vs psums
    rand <- randomRIO (0, head psums)
    return $ fst . last . filter (\(_, p) -> rand <= p) $ psv

-- | Add together duplicate terms. This is not included in the Monad implementation because it is computationally expensive
simplify :: (Num p, Ord v) => Superpos p v -> Superpos p v
simplify (Superpos s) = Superpos (map foldingFunc groups)
    where groups = groupBy (\(x, _) (y, _) -> x==y) . sortBy (\(x, _) (y, _) -> compare x y) $ s
          foldingFunc = foldl1' (\(x, acc) (_, p) -> (x, acc+p)) -- folds [[(v, p)]] into [(v, p)]


partialSumR :: Num a => [a] -> [a]
partialSumR = init . foldr (\v l -> case l of
    (x:xs) -> (v+x):x:xs
    []     -> []) [0]


instance Fractional b => Functor (Superpos b) where
    fmap f (Superpos ps) = Superpos $ map (first f) ps

instance Fractional b => Applicative (Superpos b) where
    pure x = Superpos [(x, 1)]
    Superpos fs <*> Superpos xs = Superpos [(f x, pf*px) | (f, pf) <- fs, (x, px) <- xs]

-- Uses the same definition as in the Prob type from http://learnyouahaskell.com/for-a-few-monads-more
instance Fractional b => Monad (Superpos b) where
    return x = Superpos [(x, 1)]
    m >>= f = flatten (fmap f m)

instance Fractional b => MonadFail (Superpos b) where
    fail _ = Superpos []

flatten :: Fractional b => Superpos b (Superpos b a) -> Superpos b a
flatten (Superpos xs) = Superpos $ concatMap multAll xs
    where multAll (Superpos innerxs, p) = map (second (p *)) innerxs