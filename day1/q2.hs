import Data.Map (Map, empty, member, insert, lookup, adjust, update, toList)

-- read file into two lists, right and left cols
-- sort both lists, can we do this during read? some buffer or something?
-- iterate through right list, generate hashmap of <value, numInstances>
-- iterate through left list, multiply val with 0 or result of hashmap lookup 
-- sum and return

lineToPair :: String -> (Int, Int)
lineToPair line = 
    let [left, right] = words line
    in (read left, read right)

readFileToLists :: IO ([Int], [Int])
readFileToLists = do
    content <- readFile "./input.txt"
    let linesOfFile = lines content
    let leftList = [fst $ lineToPair a | a <- linesOfFile ]
    let rightList = [snd $ lineToPair a | a <- linesOfFile ]
    return(leftList, rightList)

handleFold :: Map Int Int -> Int -> Map Int Int
handleFold acc cur =
    case Data.Map.lookup cur acc of
        Just val -> adjust (1 +) cur acc
        Nothing -> insert cur 1 acc

calculateFrequencyRating :: Map Int Int -> Int -> Int
calculateFrequencyRating map val =
    case Data.Map.lookup val map of
        Just freq -> val*freq
        Nothing -> 0

main :: IO () 
main = do 
    (list1, list2) <- readFileToLists -- O(n) time estimate
    let list2Map = Prelude.foldl handleFold Data.Map.empty list2
    print $ sum $ [calculateFrequencyRating list2Map a | a <- list1]