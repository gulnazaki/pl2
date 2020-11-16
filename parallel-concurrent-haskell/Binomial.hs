module Binomial (combs, readLines) where

import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

extGCD :: Int -> Int -> (Int, Int, Int)
extGCD a 0 = (1, 0, a)
extGCD a b = let
                 (q, r) = a `quotRem` b
                 (x, y, g) = extGCD b r
             in  (y, x - q * y, g)

divModulo :: Int -> Int -> Int -> Int
divModulo a b p = let
                   (modInv, _, _) = extGCD b p
               in  (a * modInv) `mod` p

combs :: (Int, Int, Int) -> Int
combs (n, k, p) = binom n min_denom 1 1
    where
        min_denom = min k (n - k)
        binom n k num denom
            | n == 0    = 0
            | k == 0    = divModulo num denom p
            | otherwise = let
                              num'   = (num * n) `mod` p
                              denom' = (denom * k) `mod` p
                          in  num' `seq` denom' `seq` binom (n-1) (k-1) num' denom'

readLine :: IO (Int, Int, Int)
readLine = do
    line <- C.getLine
    let [a, b, c] = (fst . fromJust . C.readInt) <$> C.words line
    return (a, b, c)

readLines :: Int -> [(Int, Int, Int)] -> IO ([(Int, Int, Int)])
readLines 0 l = return l
readLines t l = do
    ln <- readLine
    readLines (t - 1) $ l ++ [ln]

processLines :: Int -> IO ()
processLines 0 = return ()
processLines t = do
    (n, k, p) <- readLine
    let res = combs (n, k, p)
    print res
    processLines (t - 1)

processLine :: IO ()
processLine = do
    (n, k, p) <- readLine
    let res = combs (n, k, p)
    print res