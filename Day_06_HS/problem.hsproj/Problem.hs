import Data.List

main = do  
    contents <- readFile "input.txt"  
    print $ colIterate (maximum) (lines $ contents) --p1: umejzgdw
    print $ colIterate (minimum) (lines $ contents) --p2: aovueakv    
foo ([]:_) = []
foo xs =  [(map head xs)]++(foo $ map tail xs)
    
colIterate :: ([(Int, String)] -> (Int, String)) -> [[Char]] -> [Char]
colIterate _ ("":_) = []
colIterate f xs = (decodeColBy f xs)++(colIterate f $ map tail xs)

decodeColBy :: ([(Int, String)] -> (Int, String)) -> [String] -> String
decodeColBy f xs = snd . f . map (\x -> (length x, nub x)) . group . sort $ map (head) xs
