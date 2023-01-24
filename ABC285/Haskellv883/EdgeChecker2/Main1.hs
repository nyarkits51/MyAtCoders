{-- EdgeChecker2 --}
-- Author: Nyarkits51 
-- Copyright: Nyarkits JAPAN 2023
-- Made: 2023-01-24-Tues JAPAN
-- Liscence: BSD3 

-- Name: checkEdge
-- Function: checking whether the given edge is in "tree" or not.
--           And if it is, checkEdge function returns "Yes"
--           but not, the function returns "No".
checkEdge :: (Eq a, Eq b) => (a, b) -> [(a, b)] -> String
checkEdge _ [] = "No"
checkEdge (x, y) ((p,q):ts) =
  if (x == p) && (y == q)
  then "Yes" else checkEdge (x, y) ts

-- Name: tree
-- Value: List of Integer Tuple, Data Structure
--        The tree contains the given edges
--        which are made of Integer Tuple,
--        a pair of the parent node and the child node.
tree :: (Num a, Num b) => [(a, b)]
tree = [(1, 2), (1, 3), (2, 4), (2, 5), (3, 6), (3, 7), (4, 8), (4, 9), (5, 10), (5, 11), (6, 12), (6, 13), (7, 14), (7, 15)]

-- Name: main
-- Function: outputing in the standard output format with Monad bind input/optput operator
main :: IO ()
main = getLine >>= \ioStr -> return (words ioStr) >>= \[a, b] -> return (read a::Integer, read b::Integer) >>= \(a, b) -> return (checkEdge (a, b) tree) >>= \ans -> return ans >>= putStrLn 
