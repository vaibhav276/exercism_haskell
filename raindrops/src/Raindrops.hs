module Raindrops (convert) where

import Control.Monad.Trans.Writer

convert :: Int -> String
convert = failSafe . plingPlangPlong

failSafe :: Writer [String] Int -> String
failSafe w
    | null s    = show n
    | otherwise = concat s
        where (n,s) = runWriter w

plingPlangPlong :: Int -> Writer [String] Int
plingPlangPlong n = do
    n <- isFactor 3 n "Pling"
    n <- isFactor 5 n "Plang"
    n <- isFactor 7 n "Plong"
    return n

isFactor :: Int -> Int -> String -> Writer [String] Int
isFactor d n s
    | n `mod` d == 0 = writer (n, [s])
    | otherwise      = writer (n, [])
