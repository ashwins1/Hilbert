import Data.Function
import qualified Data.List as L
import qualified Data.Set as S
import Foreign.C
import Foreign.C.Types
import System
import System.IO

type Point a = (a, a)

data Rect = Rect [Point Int] | Degenerate

data HilbertRTree = Empty
                    | Leaf { mbr :: Rect, lhv :: Int, dataRects :: [Rect], parent :: HilbertRTree }
                    | InteriorNode { mbr :: Rect, lhv :: Int, children :: [HilbertRTree] }

leafCapacity :: Int
leafCapacity = 4

interiorCapacity :: Int
interiorCapacity = 5

-- this "Hilbert value" function is really just the Cantor pairing function...I couldn't get the real Hilbert function working :(
hilbertValue :: Point Int -> Int
hilbertValue (x, y) = y + (((x + y) * (x + y + 1)) `div` 2)

rectToList :: Rect -> [Point Int]
rectToList (Rect list) = list
rectToList Degenerate = []

readRect :: String -> Rect
readRect str = if isRect fromString then fromString else Degenerate
     where fromString = listToRect (read ("[" ++ str ++ "]") :: [Int])
           listToRect coords
               | length coords /= 8 = Degenerate
               | otherwise          = Rect (zip (get even coords 0) (get odd coords 0))
                                          where get fn (x:xs) counter = if fn counter then x:(get fn xs (counter + 1)) else get fn xs (counter + 1)
                                                get fn [] counter = []
           isRect Degenerate = False
           isRect (Rect list) = ((S.size $ foldl (flip S.insert) S.empty (map fst list)) == 2) && ((S.size $ foldl (flip S.insert) S.empty (map snd list)) == 2)

midpoint :: Rect -> Point Int
midpoint (Rect list) = (average $ map fst list, average $ map snd list)
    where average nums = sum nums `div` length nums

minimumBoundingRectangle :: [Rect] -> Rect
minimumBoundingRectangle rs = Rect( [(x,y) | x <- [mn xlist, mx xlist], y <- [mn ylist, mx ylist]] )
    where xlist = map fst $ concat $ map rectToList rs
          ylist = map snd $ concat $ map rectToList rs
          mn = minimum
          mx = maximum

computeLHV :: [Rect] -> Int
computeLHV = maximum . map (hilbertValue . midpoint)

intersect :: Rect -> Rect -> Bool
intersect (Rect r1) (Rect r2) = (overlap fst) && (overlap snd)
    where overlap fn = ((head $ L.sortBy (compare `on` fn) r1) > (last $ L.sortBy (compare `on` fn) r2)) || ((head $ L.sortBy (compare `on` fn) r2) > (last $ L.sortBy (compare `on` fn) r1))
intersect _ _ = False

search :: HilbertRTree -> Rect -> [Rect]
search _ Degenerate = []
search (Leaf {dataRects=rs}) r = filter (intersect r) rs
search (InteriorNode {mbr=mbr, children=children}) r = if not $ intersect mbr r then [] else concat [search child r | child <- children]

insert :: Rect -> HilbertRTree -> HilbertRTree
insert Degenerate tree = tree
insert r Empty = Leaf {mbr = r, lhv = computeLHV [r], dataRects = [r], parent = Empty} 
insert r l@(Leaf {mbr = mbr, dataRects = rects, parent = p}) = l {dataRects = (r:rects)}
    -- | length rects < leafCapacity = createLeaf p (r:rects)
    -- | otherwise                   = handleOverflow l r
    -- where createLeaf p rs = Leaf {mbr = minimumBoundingRectangle rs, lhv = computeLHV rs, dataRects = rs, parent = p}
insert r interior = interior {children = (tail ch) ++ [insert r (head ch)]}
    where ch = children interior
    -- | L.any (\child -> lhv child > hilbertValue r) (children i) = i {children = [child | child <- children i, child /=  
    -- | otherwise = i {children = (children i) ++ 

printTree :: HilbertRTree -> IO ()
printTree i@(InteriorNode {children = ch}) = printChildren ch
    where printChildren (c:cs) = do printTree c
                                    printChildren cs
          printChildren [] = return ()
printTree l@(Leaf {dataRects = rects}) = printRects rects
    where printRects [] = return ()
          printRects ((Rect list):rs) = do print list
                                           printRects rs
          printRects ((Degenerate):rs) = printRects rs

getInput :: String -> IO HilbertRTree
getInput filename = do handle <- openFile filename ReadMode
                       input <- hGetContents handle
                       return (foldl (\hrtree str -> insert (readRect str) hrtree) Empty (words input))

queryLoop :: HilbertRTree -> IO ()
queryLoop tree = do putStr ">>> "
                    hFlush stdout
                    input <- getLine
                    putStr "Found "
                    print $ length $ search tree (readRect input)
                    putStr " rectangles intersecting your given rectangle."
                    printList $ map (show . rectToList) $ take 4 $ search tree (readRect input)
                    queryLoop tree
                      where printList [] = putStrLn ""
    			    printList (x:xs) = do putStrLn x
                                                  printList xs

main = do args <- getArgs
          tree <- getInput $ head args
          printTree tree
          putStrLn "Done reading tree. Please enter queries now..."
          queryLoop tree
