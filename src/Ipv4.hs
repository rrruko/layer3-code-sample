module Ipv4 (possibleAddresses) where

import Control.Monad (guard)
import Data.List (isPrefixOf, nub, uncons)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)
import Text.Read (readMaybe)

data Ipv4 = Ipv4 Word8 Word8 Word8 Word8
    deriving (Eq, Show)

possibleAddresses :: String -> [Ipv4]
possibleAddresses = mapMaybe toIpv4 . flatten . parseTree
    where
        toIpv4 [a,b,c,d] = Just (Ipv4 a b c d)
        toIpv4 _         = Nothing

parseOctets :: String -> [(Word8, String)]
parseOctets str = nub $ mapMaybe (readDigits str) [1, 2, 3]
    where
        readDigits str n = do
            let (prefix, suffix) = splitAt n str
            n <- readValidOctet prefix
            fmap (\x -> (x, suffix)) (toWord8 n)
        readValidOctet str = do
            guard (not ("0" `isPrefixOf` str) || str == "0")
            readMaybe str

toWord8 :: Int -> Maybe Word8
toWord8 n
    | n >= 0 && n < 256 = Just (fromIntegral n)
    | otherwise = Nothing

data ParseTree = ParseTree { getParseTree :: [(Word8, ParseTree)] }
    deriving (Show)

parseTree :: String -> ParseTree
parseTree str = ParseTree (map (fmap parseTree) (parseOctets str))

flatten :: ParseTree -> [[Word8]]
flatten (ParseTree []) = [[]]
flatten (ParseTree tree) = do
    (octet, ips) <- map (fmap flatten) tree
    map (octet:) ips
