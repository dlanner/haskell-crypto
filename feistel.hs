import Data.Bits
import Data.List
import Data.List.Split (chunksOf)
import Data.Char

half_length :: [Char] -> Int
half_length text = div (length text) 2

halve :: [Char] -> [[Char]]
halve text
    | even $ length text = chunksOf (half_length(text)) text 
    | otherwise          = [left, right ++ "0"] -- "0" for padding
    where [left, right] = chunksOf (half_length(text)+1) text

split' :: [Char] -> [[Int]]
split' = ((map . map) ord . halve)

-- TODO:
--http://www.quadibloc.com/crypto/co0401.htm
--key_scheduling_algorithm = 

--combine :: [Char] -> [Char] -> ([Char] -> Int) -> Int -> [Char]
--combine left right round_function key = right ++ (map chr) $ left `xor` round_function(right, key)

-- TODO: get working
--feistel :: [Char] -> ([Char] -> Int) -> (TODO -> TODO) -> Int -> Text
--feistel text round_function ksa 0 = text
--feistel text round_function ksa current_round =
--    feistel (combine (split' text) round_function ksa(current_round)) round_function ksa (current_round-1)

-- TODO:
--main = feistel "Hello world" round_function key_scheduling_algorithm 32

-- TOOD: export as module?