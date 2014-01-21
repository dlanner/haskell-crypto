import Data.Bits
import Data.List
import Data.List.Split (chunksOf)
import Data.Char

halve :: [Char] -> [[Char]]
halve text
    | even $ length text = chunksOf ((length text) `div` 2) text 
    | otherwise          = [left, right ++ "0"] -- "0" for padding
    where [left, right] = chunksOf (((length text) `div` 2)+1) text

split' :: [Char] -> [[Int]]
split' = ((map . map) fromEnum . halve)

combine :: [Char] -> [Char] -> ([Char] -> Int) -> Int -> [Char]
combine left right round_function subkey = right ++ (map chr) $ left `xor` round_function(right, subkey)

-- TODO: get working
feistel :: [Char] -> ([Char] -> Int) -> (TODO -> [Char]) -> Int -> Text
feistel text _ _ 0 = text
feistel text round_function ksa current_round =
    feistel (combine (split' text) round_function ksa (current_round-1)

-- TODO:
--main = feistel "Hello world" round_function key_scheduling_algorithm 32

-- TOOD: export as module?