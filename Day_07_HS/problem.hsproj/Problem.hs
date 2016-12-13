--import Text.Regex
--
data IP = IP { hyper :: String, super :: String }
data Item a = One [a] | Many [Item a]
main = do  
    contents <- readFile "input.txt"  
    print $ p1 contents --p1: 118
    print $ p2 contents --p2: 260

--shared logic
createHyperList :: [Char] -> Bool -> [Char]
createHyperList []         _     =   []
createHyperList ('[':xs)   _     = '[':createHyperList xs True
createHyperList (']':xs)   _     = ']':createHyperList xs False
createHyperList (x:xs)     True  = x  :createHyperList xs True
createHyperList (_:xs)     False =     createHyperList xs False

createSuperList :: [Char] -> Bool -> [Char]
createSuperList []         _     =   []
createSuperList ('[':xs)   _     = '[':createSuperList xs True
createSuperList (']':xs)   _     = ']':createSuperList xs False
createSuperList (x:xs)     False = x  :createSuperList xs False
createSuperList (_:xs)     True  =     createSuperList xs True

parse ::String -> [IP]
parse = map (\x -> IP{hyper = hyperList x, super = superList x}) . lines
      where hyperList x = createHyperList x False
            superList x = createSuperList x False

--part one logic
p1 :: String -> Int
--p1 = length . filter (==True) . map isTls . map (\IP{hyper = h, super = s} -> (isAbba h, isAbba s)) . parse
p1 = length . filter (==True) . map isTls . map (\IP{hyper = h, super = s} -> (isAbba h, isAbba s)) . parse

isAbba :: String -> Bool
isAbba [] = False
isAbba xs = isMatch || (isAbba $ tail xs)
                where checkStr = splitAt 2 $ take 4 xs
                      isMatch = (fst checkStr /= snd checkStr) && (fst checkStr == (reverse $ snd checkStr))


isTls :: (Bool,Bool) -> Bool
isTls (False,True)  = True
isTls (_,_) = False


--part two logic
p2 :: String -> Int
p2 c = length .filter (\x -> True `elem` x) . map findBab . map (\IP{hyper = h, super = s} -> (findAba h, findAba s)) $ parse c



findBab :: ([String],[String]) -> [Bool]
findBab ([],_) = []
findBab ((h:hyper),super) = (expectedSuper h `elem` super):findBab(hyper,super)
            where expectedSuper h = (h!!1):(h!!0):(h!!1):[]

findAba :: String -> [String]
findAba [] = []
findAba xs | isMatch = [inspect]++(findAba $ tail xs)
           | otherwise = findAba $ tail xs
                where inspect = take 3 xs
                      isMatch = inspect == (reverse inspect) && length inspect == 3 && not (xs!!0 == xs!!1)


