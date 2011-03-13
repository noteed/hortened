{-# LANGUAGE OverloadedStrings #-}
module Hortened.Base62 where

-- TODO check column sizes for passwords, hashes, ...
-- link_id should not be auto-increment (to reuse ids
-- when the corresponding link is removed from db).

import qualified Data.ByteString.Char8 as B
import Data.Char

-- TODO (benchmark?) make the code below more efficient (the BS is slower).

alphabet :: String
alphabet = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphabetBS :: B.ByteString
alphabetBS = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphalen :: Int
alphalen = length alphabet

alphalenBS :: Int
alphalenBS = B.length alphabetBS

alphabet' :: String
alphabet' = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphabetBS' :: B.ByteString
alphabetBS' = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphalen' :: Int
alphalen' = length alphabet'

alphalenBS' :: Int
alphalenBS' = B.length alphabetBS'

-- Base n encoding. The n digits are given by 'alphabet'.
-- Actually, the first digit is taken from 'alphabet'' so
-- it is always an uppercase character.
encode :: Int -> String
encode 0 =  [head alphabet']
encode i = let (n, r) = (i `div` alphalen', i `mod` alphalen')
           in go n [alphabet' !! r]
  where go 0 enc = reverse enc
        go x enc = let (n, r) = (x `div` alphalen, x `mod` alphalen)
                   in go n (alphabet !! r : enc)

encodeBS :: Int -> B.ByteString
encodeBS 0 =  B.take 1 alphabetBS'
encodeBS i = let (n, r) = (i `div` alphalenBS', i `mod` alphalenBS')
           in go n (B.singleton $ B.index alphabetBS' r)
  where go 0 enc = enc
        go x enc = let (n, r) = (x `div` alphalenBS, x `mod` alphalenBS)
                   in go n (B.snoc enc $ B.index alphabetBS r)

-- Sum di k^i where k is the base and di is the ith digit, from right to left.
-- Actually, from left to right as the string is reversed in 'encode'.
decode :: String -> Int -- TODO should be Maybe Int
decode [] = error "Can't decode the empty string."
decode (d:ds) = d' + sum (zipWith (\d_ i -> d_ * alphalen'*alphalen^(i::Integer)) ds' [0..])
  where d' | 65 <= ord d && ord d <= 90 = ord d - 65 -- 0..25 
           | otherwise = error "Can't decode"
        ds' = map value ds
        value c | 48 <= ord c && ord c <=  57 = ord c - 48 -- 0..9
                | 97 <= ord c && ord c <= 122 = ord c - 87 -- 10..35 
                | 65 <= ord c && ord c <=  90 = ord c - 29 -- 36..61
                | otherwise = error "Can't decode"

decodeBS :: B.ByteString -> Int -- TODO should be Maybe Int
decodeBS dds = case B.uncons dds of
  Nothing -> error "Can't decode' the empty string."
  Just (d, ds) -> d' + go ds 0 0
    where d' | 65 <= ord d && ord d <= 90 = ord d - 65 -- 0..25 
             | otherwise = error "Can't decode"
          value c | 48 <= ord c && ord c <=  57 = ord c - 48 -- 0..9
                  | 97 <= ord c && ord c <= 122 = ord c - 87 -- 10..35 
                  | 65 <= ord c && ord c <=  90 = ord c - 29 -- 36..61
                  | otherwise = error "Can't decode"
          go :: B.ByteString -> Int -> Int -> Int
          go bs i n = case B.uncons bs of
            Nothing -> n
            Just (b, bs') -> go bs' (i + 1) (n + value b * alphalenBS' * alphalenBS ^ i)

-- TODO QuickCheck test

decodeEncodeProp :: Int -> Bool
decodeEncodeProp i = (decode . encode) i == i

decodeEncodeProp' :: Bool
decodeEncodeProp' = all decodeEncodeProp [1..1000000]

decodeEncodePropBS :: Int -> Bool
decodeEncodePropBS i = (decodeBS . encodeBS) i == i

decodeEncodePropBS' :: Bool
decodeEncodePropBS' = all decodeEncodePropBS [1..1000000]
