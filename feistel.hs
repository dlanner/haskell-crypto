import Data.Bits
import Data.List
import Data.List.Split (chunksOf)
import Data.Char

xor' :: [Char] -> [Char] -> [Char]
xor' x y = map chr $ zipWith xor (map fromEnum x) (map fromEnum y)

halve :: [Char] -> [[Char]]
halve text
    | even $ length text = chunksOf ((length text) `div` 2) text 
    | otherwise          = [left, right ++ "0"] -- "0" for padding
    where [left, right] = chunksOf (((length text) `div` 2)+1) text

split' :: [Char] -> [[Int]]
split' = ((map . map) fromEnum . halve)

combine :: [[Char]] -> ([Char] -> [Char] -> [Char]) -> [Char] -> [Char]
combine [left, right] round_function subkey = right ++ (xor' left (round_function right subkey))

-- TODO: get working
feistel :: [Char] -> ([Char] -> [Char] -> [Char]) -> [Char] -> Int -> [Char]
feistel text _ _ 0 = text
feistel text round_function subkey current_round =
    feistel (combine (halve text) round_function subkey) round_function subkey (current_round-1)

-- TODO:
--main = feistel "Hello world" round_function key_scheduling_algorithm 32

-- TOOD: export as module?