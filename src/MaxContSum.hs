module MaxContSum (maxContSum) where

import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty

maximalSubsequences :: Foldable f => f Integer -> [NonEmpty Integer]
maximalSubsequences = groupBy (\x y -> x > 0 && y > 0)

maxContSum :: NonEmpty Integer -> Integer
maxContSum = maximum . map sum . maximalSubsequences

-- Could just use NonEmpty.groupBy, but that felt like cheating
groupBy :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy equals = foldr addToSubsequence []
    where
        addToSubsequence element [] = [pure element]
        addToSubsequence element acc@(x:xs)
            | element `equals` NonEmpty.head x = (element <| x):xs
            | otherwise = pure element:acc
