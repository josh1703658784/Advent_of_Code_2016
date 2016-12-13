import Data.List
import Data.List.Split
import Data.Function
import Data.Char

type Room = (String, String, String, Int, String) 


main = do  
    contents <- readFile "input.txt"
    print $ sectorSum $ realRooms $ map format $ parse contents --sol1: 361724
    print $ findTheRoom $ decryptRooms $ realRooms $ map format $ parse contents --sol2: 482


findTheRoom :: [(String,String)] -> Maybe (String,String)
findTheRoom = find (\x -> fst x == "storage-object-northpole-")

sectorSum :: [Room] -> Int
sectorSum = foldl (+) 0 . map (\(_,_,_,x,_) -> x)

realRooms :: [Room] -> [Room]
realRooms = filter (\(_,s,t,_,_) -> s == t)

parse :: String -> [[String]]
parse = map (splitOn "-") . lines

format :: [String] -> Room
format xs = (name, checksum, myChecksum, sectorId, dirtyName)
        where dirtyName   = foldl (\acc x -> x++"-"++acc) "" $ init xs
              name        = foldl (++) "" $ init xs
              checksum    = init . last . splitOn ("[") $ last xs
              sectorId    = read (head . init . splitOn ("[") $ last xs) :: Int
              myChecksum  = calcChecksum name

calcChecksum :: String -> String
calcChecksum = createChecksum . reverse . groupCounts . sort . countChars . sort
                where countChars        = map (\x -> (length x, head x)) . group
                      groupCounts       = groupBy ((==) `on` fst)
                      createChecksum ns = take 5 $ foldl (++) "" $ map (map (snd)) ns

decryptRooms :: [Room] -> [(String, String)]
decryptRooms xs = map decryptedName xs
                where decryptedName (name,_,_,id, dName) = (map (\c -> shiftChar c id) dName, (show id))


shiftChar :: Char -> Int -> Char
shiftChar '-' _ = '-'
shiftChar x n = chr (((n + ord x - ord 'a') `mod` 26) + ord 'a')

