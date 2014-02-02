import Data.Bits (xor)
import Text.Printf (printf)
import Data.Char (toUpper)
import Numeric (showHex)

rc4 :: [Char] -> [Char] -> [Char]
rc4 key plaintext = ciphertext
  where ciphertext = format_hex ciphertext_bytes
        ciphertext_bytes = zipWith (xor) keystream (map fromEnum plaintext)
        keystream = prga key (length plaintext)

prga :: [Char] -> Int -> [Int]
prga key n =
  -- TODO: make lazy (so you can zipWith the infinite list)
  let prga_step 0 _ _ _ keystream = keystream
      prga_step n i j s keystream = prga_step (n-1) next_i next_j next_s extended_keystream
        where
          next_i = (i + 1) `mod` 256
          next_j = (j + (s !! next_i)) `mod` 256
          next_s = swapElts next_i next_j s
          key_index = ((next_s !! next_i) + (next_s !! next_j)) `mod` 256
          extended_keystream = keystream ++ [ next_s !! key_index ]
  in prga_step n 0 0 (ksa key) []

ksa :: [Char] -> [Int]
ksa key = 
  let ksa_step 256 j s key = s
      ksa_step i j s key = ksa_step next_i next_j next_s key
        where next_i = i + 1
              key_value = key !! (i `mod` length key)
              next_j = (j + (s !! i) + fromEnum key_value) `mod` 256
              next_s = swapElts i next_j s
  in ksa_step 0 0 [0..255] key

-- https://gist.github.com/ijt/2010183
swapElts i j ls = [get k x | (k, x) <- zip [0..length ls - 1] ls]
  where get k x | k == i = ls !! j
                | k == j = ls !! i
                | otherwise = x

format_hex :: [Int] -> [Char]
format_hex bytes = map toUpper (concatMap ((\x -> if (length x) == 1 then "0" ++ x else x ) . (\x -> showHex x "")) bytes)

-- TODO: Tests
-- http://book.realworldhaskell.org/read/testing-and-quality-assurance.html
-- Test vectors from the Wikipedia RC4 page: https://en.wikipedia.org/wiki/RC4#Test_vectors
-- rc4 "Key" "Plaintext"         == "BBF316E8D940AF0AD3"
-- rc4 "Wiki" "pedia"            == "1021BF0420"
-- rc4 "Secret" "Attack at dawn" == "45A01F645FC35B383552544B9BF5"