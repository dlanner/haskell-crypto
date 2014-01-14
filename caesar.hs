module Caesar (caesar_encrypt, caesar1_encrypt, caesar_decrypt, caesar1_decrypt) where

import Data.Char

caesar_encrypt :: Int -> [Char] -> [Char]
caesar_encrypt n text = map ((chr) . (`mod` 128) . (+n) . ord) $ filter (\ x -> x /= ' ') text

caesar1_encrypt :: [Char] -> [Char]
caesar1_encrypt = caesar_encrypt 1

caesar_decrypt :: Int -> [Char] -> [Char]
caesar_decrypt n = map ((chr) . (`mod` 128) . (subtract n) . ord)

caesar1_decrypt :: [Char] -> [Char]
caesar1_decrypt = caesar_decrypt 1
