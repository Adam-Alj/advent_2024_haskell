create2dList :: [String] -> [[Int]]
create2dList = map (map read . words)

-- Trivial thanks to this amazing comment https://stackoverflow.com/a/20527845
getSafetyList :: [[Int]] -> [[Int]]
getSafetyList = map (\a -> zipWith (-) a (drop 1 a))

hasConsistentRate :: [Int] -> Bool
hasConsistentRate reportRate = all (> 0) reportRate || all (< 0) reportRate

hasLowRate :: [Int] -> Bool
hasLowRate = all (\a -> 1 <= abs a && abs a <= 3)

isSafeReport :: [Int] -> Int
isSafeReport report =
    if hasConsistentRate report && hasLowRate report then 1 else 0

main :: IO ()
main = do
    fileContent <- readFile "./input.txt"
    let inputLines = lines fileContent
    let safetyList = getSafetyList $ create2dList inputLines
    print $ sum $ map isSafeReport safetyList