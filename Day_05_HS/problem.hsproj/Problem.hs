import Data.ByteString.Char8 hiding (map, take, filter, foldl, reverse)
import Crypto.Hash.MD5 as MD5
import Data.ByteString.Base16

crackPwd1 :: String -> String
crackPwd1 = buildPwd1 . take 8 . findValid1 . genHashes . genCombos

crackPwd2 :: String -> String
crackPwd2 = buildPwd2 . findValid2 7 . findValid1 . genHashes . genCombos

--logic
buildPwd1 :: [String] -> String
buildPwd1 = reverse . foldl (\acc x -> (x!!5):acc) ""

buildPwd2 :: [String] -> String
buildPwd2 = reverse . map (\x -> (x!!6))

findValid1 :: [String] -> [String]
findValid1 = filter (\x -> take 5 x == "00000")

findValid2 :: Int -> [String] -> [String]
findValid2 (-1) _ = []
findValid2 len xs = (findit len)++findValid2 (len-1) xs
                    where findit n = take 1 $ filter (\x -> (take 6 x) == "00000"++(show n)) xs

genHashes :: [String] -> [String]
genHashes = map (unpack . encode . MD5.hash . pack)

genCombos :: String -> [String]
genCombos str = map (\x -> str++(show x)) [1..]
