{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import Data.Char

--block_size = 128
---- TODO: random from Crypto.Random.genBytes ?
---- Just use 128 A's to start
--key = [ 'a' | xs <- [0..127] ]
--keyLeft = take 64 key
--key_size = 128
--num_rounds = 16

--little endian -- needed?

-- TODO:
--http://www.quadibloc.com/crypto/co0401.htm

--Each round uses a 72-bit subkey.
--The subkey for the first round consists of the first byte of the key repeated twice,
--followed by the next seven bytes of the key. Rotate the key left by seven bytes,
--then generate the subkey for the next round.

-- Not sure what's the best way to do the key scheduling algorithm.

--let key = "1234567890123456"

-- http://stackoverflow.com/a/16379034/2954849
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

key_schedule_algorithm :: [a] -> [a]
key_schedule_algorithm key = take 2 (cycle $ take 1 (take 8 key)) ++ tail (take 8 key)

nextSubkey :: [Char] -> a -> [Char]
nextSubkey key _ = ((rotate 7) . key_schedule_algorithm) key

-- Build a list of all rounds' subkeys
key_schedule :: [Char] -> Int -> [[Char]]
key_schedule key num_rounds = scanl nextSubkey initSubkey [1..num_rounds-1]
  where initSubkey = key_schedule_algorithm key

subkey :: [Char] -> Int -> [Char]
subkey key round_number = (key_schedule key 16) !! round_number

--e :: B.ByteString -> B.ByteString
--let e text = B.zipWith (.&.) $ BC.pack([chr 127])

--let e text = B.pack $ B.zipWith (.&.) "keytext" text

--let getByte n = BC.pack $ [chr $ 2^(n-1)-1]
--let genkey n = BL.cycle $ [chr $ 2^(n-1)-1]
--let e' text = B.pack $ B.zipWith (.&.) (cycle $ genkey 8) text

--blocks = chunksOf block_size text


--main = do
--  g <- newStdGen
--  print . take 10 $ (randoms g :: [Char])