{-# LANGUAGE FlexibleInstances #-}
module SecretHandshake (handshake) where

import Data.Char (intToDigit)
import Data.List (unfoldr)

class Handshake a where
    handshake :: a -> [String]

instance Handshake String where
    handshake s = snd $ rev . jump . closeYourEyes . doubleBlink . wink $ ((reverse s), [])

instance Handshake Int where
    handshake n = handshake (dec2Bin n)

dec2Bin :: Int -> String
dec2Bin = reverse . unfoldr step

step :: Int -> Maybe (Char, Int)
step 0 = Nothing
step n = Just (b, d) 
    where
        b = intToDigit b'
        (d, b') = divMod n 2

wink :: (String, [String]) -> (String, [String])
wink = apply (++["wink"])

doubleBlink :: (String, [String]) -> (String, [String])
doubleBlink = apply (++["double blink"])

closeYourEyes :: (String, [String]) -> (String, [String])
closeYourEyes = apply (++["close your eyes"])

jump :: (String, [String]) -> (String, [String])
jump = apply (++["jump"])

rev :: (String, [String]) -> (String, [String])
rev = apply reverse

apply :: ([String] -> [String]) -> (String, [String]) -> (String, [String])
apply f (s,res) 
    | null s = ([], res)
    | head s == '1' = (tail s, f res)
    | otherwise = (tail s, res)

