import Data.List

main = do  
    contents <- readFile "brady_input.txt"  --p1: 983
    let base = format contents    --p2: 1836
    print $ map (length . filter valid . map sort) [base, rotate base]

format :: String -> [[Int]]
format = map (map read) . map words . lines

rotate :: [[Int]] -> [[Int]]
rotate [] = []
rotate ([a1,a2,a3]:[b1,b2,b3]:[c1,c2,c3]:xs) = [a1,b1,c1]:[a2,b2,c2]:[a3,b3,c3]:(rotate xs)

valid :: [Int] -> Bool
valid [a,b,c] = a+b > c 
