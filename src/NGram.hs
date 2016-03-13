{-# LANGUAGE BangPatterns #-}

module NGram (
      NGram
    , empty
    , insert
    , NGram.filter
    , union
    , unions
    , intersection
    ) where

import           Data.Binary
import           Data.List           (foldl1')
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM


data NGram a = NGram !Int !(HM.HashMap [a] Int)
    deriving (Show)

instance (Binary a, Eq a, Hashable a) => Binary (NGram a) where
    put (NGram n hm) = do
        put n
        put $ HM.toList hm

    get = NGram <$> get <*> (HM.fromList <$> get)

insert :: (Hashable a, Show a, Eq a) => [a] -> NGram a -> NGram a
insert !gram !(NGram n hm) | length gram /= n = error "User error: NGram.insert: length /= n"
                           | otherwise        = NGram n (HM.alter f gram hm)
  where
    {-# INLINE f #-}
    f :: Maybe Int -> Maybe Int
    f = Just . maybe 1 (+1)

union :: (Eq a, Hashable a, Show a) => NGram a -> NGram a -> NGram a
union !(NGram n t) !(NGram m t') | n /= m    = error "User error: NGram.union: n /= m"
                                 | otherwise = NGram n (HM.unionWith (+) t t')

unions :: (Eq a, Hashable a, Show a) => [NGram a] -> NGram a
unions = foldl1' union

intersection :: (Eq a, Hashable a, Show a) => NGram a -> NGram a -> NGram a
intersection !(NGram n hm) !(NGram m hm') | n /= m    = error "User error: NGram.intersection: n /= m"
                                          | otherwise = NGram n (HM.intersection hm hm')

empty :: (Hashable a, Show a) => Int -> NGram a
empty n = NGram n HM.empty

filter :: (Hashable a, Show a) => (Int -> Bool) -> NGram a -> NGram a
filter f !(NGram n hm) = NGram n (HM.filter f hm)
