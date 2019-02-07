module MaxContSum (maxContSum) where

import Control.Monad ((<=<))
import Data.List.NonEmpty (NonEmpty(..), (<|), uncons)

maxContSum :: NonEmpty Integer -> Integer
maxContSum = maximum . fmap sum . contiguousSubsequences

contiguousSubsequences :: NonEmpty a -> NonEmpty (NonEmpty a)
contiguousSubsequences = inits <=< tails

inits :: NonEmpty a -> NonEmpty (NonEmpty a)
inits xs =
    case uncons xs of
        (x, Nothing) -> pure (pure x)
        (x, Just rest) -> pure x <| fmap (x<|) (inits rest)

tails :: NonEmpty a -> NonEmpty (NonEmpty a)
tails xs =
    case uncons xs of
        (x, Nothing) -> pure (pure x)
        (_, Just rest) -> pure xs <> tails rest
