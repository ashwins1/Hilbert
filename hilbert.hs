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
                    | Leaf { mbr :: Rect, dataRects :: [Rect] }
                    | InteriorNode { mbr :: Rect, lhv :: Int, children :: [HilbertRTree] }

leafCapacity :: Int
leafCapacity = 4

interiorCapacity :: Int
interiorCapacity = 5

hilbertValue :: Point Int -> Int
hilbertValue (x, y) = x + y

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
minimumBoundingRectangle rs = Rect( [(mn xlist, mn ylist), (mn xlist, mx ylist), (mx xlist, mn ylist), (mx xlist, mx ylist)] )
    where xlist = map fst $ concat $ map rectToList rs
          ylist = map snd $ concat $ map rectToList rs
          mn = minimum
          mx = maximum

intersect :: Rect -> Rect -> Bool
intersect r1 r2 = not (noOverlap r1 r2 fst && noOverlap r2 r1 fst && noOverlap r1 r2 snd && noOverlap r2 r1 snd)
    where noOverlap a b relation = (head $ L.sort $ map relation (rectToList a)) > (last $ L.sort $ map relation (rectToList b))

search :: HilbertRTree -> Rect -> [Rect]
search _ Degenerate = []
search (Leaf {dataRects=rs}) r = filter (intersect r) rs
search (InteriorNode {mbr=mbr, children=children}) r = if not $ intersect mbr r then [] else concat [search child r | child <- children]

insert :: Rect -> HilbertRTree -> HilbertRTree
insert Degenerate tree = tree
insert r@(Rect list) Empty = Leaf {mbr = r, dataRects = [r]}
insert r@(Rect list) (Leaf {mbr = mbr, dataRects = rects})
    | length rects < leafCapacity = Leaf {mbr = minimumBoundingRectangle(r:rects), dataRects = r:rects}
    | otherwise                   = Empty

printTree :: HilbertRTree -> IO ()
printTree (Leaf mbr rects) = printRects rects
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
          queryLoop tree
