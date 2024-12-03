create2dList :: [String] -> [[Int]]
create2dList = map (map read . words)

-- Trivial thanks to this amazing comment https://stackoverflow.com/a/20527845
getSafetyList :: [[Int]] -> [[Int]]
getSafetyList = map (\a -> zipWith (-) a (init a))

-- num positive, num negative, num 0, num high
checkValue :: Int -> (Int, Int, Int, Int)
checkValue num =
    (
        if num > 0 then 1 else 0,
        if num < 0 then 1 else 0,
        if num == 0 then 1 else 0,
        if abs num > 3 then 1 else 0
    )

accValue :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
accValue (a1, b1, c1, d1) cur  = do
    let (a2, b2, c2, d2) = checkValue cur
    (a1 + a2, b1 + b2, c1 + c2, d1 + d2)


-- pos to neg delta, num 0, num High
-- we can only have 1 total here
-- any present high is a failure, as the next is meant to be lower or higher anyway
getSafetyScore :: [Int] -> (Int, Int, Int)
getSafetyScore report = do
    let (pos, neg, zero, high)= foldl accValue (0,0,0,0) report
    (min pos neg, zero, high)

checkSafetyScore :: (Int, Int, Int) -> Int
checkSafetyScore (lowCount, zero, high) =
    if high == 0 && lowCount + zero <= 1
        then 1
        else 0

isSafe :: [Int] -> Int
isSafe report = checkSafetyScore (getSafetyScore report)

main :: IO ()
main = do
    fileContent <- readFile "./input.txt"
    let inputLines = lines fileContent
    let safetyList = getSafetyList $ create2dList inputLines
    let mappedList = map getSafetyScore safetyList
    let sumWithoutEdgeCaseFix = sum (map isSafe safetyList)
    -- hacking our way to a fix by handling edge cases where a large num is on one end or the other
    let z = zip safetyList mappedList
    -- get all of our edge cases
    let y = filter (\(a,(x,y,z)) -> x == 0 && y == 0 && z == 1) z
    -- sum taking off the start
    let leftRemovedSafetyList = map (tail . fst) y
    let b = map getSafetyScore leftRemovedSafetyList
    print $ zip leftRemovedSafetyList b
    let sumEdgeCaseLeft = sum(map isSafe leftRemovedSafetyList)
    print sumEdgeCaseLeft
    -- sum taking off the end
    let rightRemovedSafetyList = map (init. fst) y
    let sumEdgeCaseRight = sum(map isSafe rightRemovedSafetyList)
    let b = map getSafetyScore leftRemovedSafetyList
    print $ zip leftRemovedSafetyList b
    print $ sumEdgeCaseLeft + sumEdgeCaseRight + sumWithoutEdgeCaseFix

-- Fail attemp. Off by like 7