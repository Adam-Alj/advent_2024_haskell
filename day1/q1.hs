import Data.List (sort)
-- read file into two lists, right and left cols
-- sort both lists, can we do this during read? some buffer or something?
-- iterate through both lists, taking the abs value of each diff

lineToPair :: String -> (Int, Int)
lineToPair line = 
    let [left, right] = words line
    in (read left, read right)

readFileToSortedLists :: IO ([Int], [Int])
readFileToSortedLists = do
    content <- readFile "./input.txt"
    let linesOfFile = lines content
    let leftList = [fst $ lineToPair a | a <- linesOfFile ]
    let rightList = [snd $ lineToPair a | a <- linesOfFile ]
    return(sort leftList, sort rightList)

main :: IO () 
main = do 
    (list1, list2)<- readFileToSortedLists -- O(n log(n)) time estimate
    print $ sum $ zipWith (\a b -> abs (a-b)) list1 list2