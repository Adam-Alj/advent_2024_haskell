getNextGuardDirection :: Char -> Char
getNextGuardDirection guard =
    head (tail (dropWhile (/= guard) (cycle "^>v<")))

getCharAtCoords :: (Int, Int) -> [String] -> Char
getCharAtCoords (x,y) guardMap =
    head (drop x (head (drop y guardMap)))



findInitialGuardXLocation :: String -> Int
findInitialGuardXLocation s =
    length (
        takeWhile (`notElem` "^>v<") s
    )

findInitialGuardLocation :: [String] -> Int -> (Int, Int)
findInitialGuardLocation guardMap offset =
    if any (`elem` "^>v<") (head guardMap)
        then (findInitialGuardXLocation (head guardMap), offset)
        else findInitialGuardLocation (tail guardMap) (offset+1)

lookAheadInBounds :: [String] -> Char -> (Int, Int) -> Bool
lookAheadInBounds guardMap direction (x,y) =
    case direction of
        '^' -> y /= 0
        'v' -> y < length guardMap - 1
        '>' -> x < length (head guardMap) - 1
        '<' -> x /= 0

-- could re use func but whatever this works
lookAheadChar :: [String] -> (Int, Int) -> Char -> Char
lookAheadChar guardMap (x, y) direction =
    case direction of
        '^' -> if lookAheadInBounds guardMap '^' (x,y)
            then head (drop x (head (drop (y-1) guardMap)))
            else '_'
        'v' -> if lookAheadInBounds guardMap 'v' (x,y)
            then head (drop x (head (drop (y+1) guardMap)))
            else '_'
        '>' -> if lookAheadInBounds guardMap '>' (x,y)
            then head (drop (x+1) (head (drop (y) guardMap)))
            else '_'
        '<' -> if lookAheadInBounds guardMap '<' (x,y)
            then head (drop (x-1) (head (drop (y) guardMap)))
            else '_'

getNextCoords :: Char -> (Int, Int) -> (Int, Int)
getNextCoords direction (x,y) =
    case direction of
        '^' -> (x, y-1)
        'v' -> (x, y+1)
        '>' -> (x+1, y)
        '<' -> (x-1, y)

traceGuardPath :: [String] -> (Int, Int) -> Char -> Int -> [(Int, Int)] -> Int
traceGuardPath guardMap (x, y) direction totalSteps visited = do
    let nextChar = lookAheadChar guardMap (x,y) direction
    let unvisited = (x,y) `notElem` visited
    let newVisited = if unvisited then (x,y):visited else visited
    if nextChar == '_'
        then totalSteps+1
        else if nextChar == '#'
            then traceGuardPath guardMap (x,y) (getNextGuardDirection direction) (if unvisited then totalSteps + 1 else totalSteps) newVisited
        else traceGuardPath guardMap (getNextCoords direction (x,y)) direction (if unvisited then totalSteps + 1 else totalSteps) newVisited

main :: IO ()
main = do
    inputFile <- readFile "input.txt"
    let guardMap = lines inputFile
    let initialGuardLocation = findInitialGuardLocation guardMap 0
    print $ traceGuardPath guardMap initialGuardLocation (getCharAtCoords initialGuardLocation guardMap) 0 []

-- works!!!!