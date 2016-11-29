module Sierpinsky where

import Data.List

sierpinsky :: Int -> String
sierpinsky n = intercalate "\n" (sierpinskyList n)

sierpinskyList :: Int -> [String]
sierpinskyList 0 = ["L"]
sierpinskyList n = 
    let s = (normalize . sierpinskyList) (n-1) in 
        (map removeFinalSpaces s) ++ (map removeFinalSpaces . nextIteration) s


removeFinalSpaces :: String -> String
removeFinalSpaces = dropWhileEnd (\x -> x == ' ')

nextIteration :: [String] -> [String]
nextIteration xs = map (\x -> x++" "++x) xs

normalize :: [String] -> [String]
normalize xs = normalizeWithLength (maxLength xs) xs

normalizeWithLength :: Int -> [String] -> [String]
normalizeWithLength _ [] = []
normalizeWithLength l (x:xs) = (x ++ concat (replicate (l - (length x)) " "))
                    : normalizeWithLength l xs

maxLength :: [String] -> Int
maxLength (x:[]) = length x
maxLength (x:xs) = max (length x) (maxLength xs)
