createAscShiftedDiagonalMap :: [String] -> Int -> Int -> [String]
createAscShiftedDiagonalMap wm i j =
    if length wm == 1
        then [replicate i '0' ++ head wm ++ replicate j '0']
        else (replicate i '0' ++ head wm ++ replicate j '0') : createAscShiftedDiagonalMap(tail wm) (i-1) (j+1)

createDescShiftedDiagonalMap :: [String] -> Int -> Int -> [String]
createDescShiftedDiagonalMap wm i j =
    if length wm == 1
        then [replicate i '0' ++ head wm ++ replicate j '0']
        else (replicate i '0' ++ head wm ++ replicate j '0') : createDescShiftedDiagonalMap(tail wm) (i+1) (j-1)

getAscDiagonalInputLines :: [String] -> [String]
getAscDiagonalInputLines wordmap =
    getVerticalInputLines(createAscShiftedDiagonalMap wordmap (length wordmap-1) 0)

getDescDiagonalInputLines :: [String] -> [String]
getDescDiagonalInputLines wordmap =
    getVerticalInputLines(createDescShiftedDiagonalMap wordmap 0 (length wordmap-1))

get2dTail :: [String] -> [String]
get2dTail = map tail

-- Given a word map, generates a horizontal line from the first input line
getVerticalInputLine :: [String]-> String
getVerticalInputLine = foldl (\acc cur -> acc ++ [head cur]) ""

getVerticalInputLines :: [String] -> [String]
getVerticalInputLines s =
    if length (head s) == 1
        then [getVerticalInputLine s]
        else getVerticalInputLine s : getVerticalInputLines (get2dTail s)

-- wildly inefficent
getXmasCountFromIndex :: String -> Int -> Int
getXmasCountFromIndex s x =
    if reverse (drop (x - 4) (take x s)) == "XMAS"
        || take 4 (drop (x - 1) s) == "XMAS"
        then 1
        else 0

getXmasSum :: String -> Int
getXmasSum s =
    sum (map (getXmasCountFromIndex s) [1..length s])

main :: IO ()
main = do
    fileContent <- readFile "./input2.txt"
    let horizontalInputLines = lines fileContent
    let horizontalXmasSum = foldl (\a c -> a + getXmasSum c) 0 horizontalInputLines
    let verticalInputLines = getVerticalInputLines horizontalInputLines
    let verticalXmasSum = foldl (\a c -> a + getXmasSum c) 0 verticalInputLines
    let ascDiagInputLines = getAscDiagonalInputLines horizontalInputLines
    let ascDiagXmasSum = foldl (\a c -> a + getXmasSum c) 0 ascDiagInputLines
    let descDiagInputLines = getDescDiagonalInputLines horizontalInputLines
    let descDiagXmasSum = foldl (\a c -> a + getXmasSum c) 0 descDiagInputLines
    mapM_ putStrLn $ createAscShiftedDiagonalMap horizontalInputLines (length horizontalInputLines-1) 0
    mapM_ putStrLn $ createDescShiftedDiagonalMap horizontalInputLines 0 (length horizontalInputLines-1)

    --print horizontalXmasSum
    --print verticalXmasSum
    --print ascDiagXmasSum
    --print descDiagXmasSum
    print $ horizontalXmasSum + verticalXmasSum + ascDiagXmasSum + descDiagXmasSum

-- DOES NOT WORK. ME NOT KNOW WHY