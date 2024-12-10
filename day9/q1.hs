import Data.Map (Map)
import qualified Data.Map as Map

processDiskMap :: String -> Int -> Map Int (Int,Int) -> Map Int (Int,Int)
processDiskMap [] i m = m
processDiskMap (a:b:rest) i m = processDiskMap rest (i+1) (Map.insert i (read [a], read [b]) m)
processDiskMap [s] i m = Map.insert i (read [s], 0) m

buildDefragmentedDisk :: [(Int, (Int,Int))] -> [(Int, (Int,Int))] -> [String] -> [String]
buildDefragmentedDisk map revMap defragmentedDisk
    | fst curFill == fst backFill = defragmentedDisk ++ replicate (fst (snd backFill)) (show (fst backFill))
    | otherwise = buildDefragmentedDisk
        (if frontSpace > backCount then (fst curFill, (0, frontSpace-backCount)):tail map else tail map)
        (if backCount <= frontSpace then tail revMap else (fst backFill, (backCount - frontSpace, 0)):tail revMap)
        (defragmentedDisk ++ replicate (fst (snd curFill)) (show (fst curFill)) ++ replicate (min frontSpace backCount) (show (fst backFill)))
    where
        curFill = head map
        backFill = head revMap
        frontSpace = snd (snd curFill)
        backCount = fst (snd backFill)

getChecksum :: [String] -> Int -> Int
getChecksum [] i = 0
getChecksum s i = (i * read (head s)) + getChecksum (tail s) (i+1)

main :: IO ()
main = do
    diskMap <- readFile "input.txt"
    let processedDiskMap = Map.toList (processDiskMap diskMap 0 Map.empty)
    let defragmented = buildDefragmentedDisk processedDiskMap (reverse processedDiskMap) []
    print $ processedDiskMap
    print $ defragmented
    print $ getChecksum defragmented 0

-- wow this one took me ages lol