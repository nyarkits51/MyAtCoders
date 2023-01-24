{-- EdgeChecker2 --}
-- Author: Nyarkits
-- Copyright: Nyakits JAPAN 2023
-- MadeIn: 2023-01-24-Tues JAPAN
-- Liscence: BSD3

-- Name: seqn 
-- Function: Monad-type function:
-- Sequencing Monad-type value or function to generate list
seqn :: Monad m => [m a] -> m [a]
seqn [] = return []
seqn (act:acts) = do
  x <- act
  xs <- seqn acts
  return (x:xs)

-- Name: checkEdge2
-- Function: checking whether the edge is in the tree or not.
--           And if it is, checkEdge function returns "Yes"
--           but not, the function returns "No".
checkEdge2 :: (Eq a, Eq b) => (a, b) -> [(a, b)] -> String
checkEdge2 _ [] = "No"
checkEdge2 (x, y) ((p,q):ts) =
  if (x == p) && (y == q)
  then "Yes" else checkEdge2 (x, y) ts

-- Name: tree
-- Value: List of Integer Tuple.
--        The tree contains the given edges
--        which are made of Integer Tuple,
--        a pair of the parent node and the child node.
tree :: (Num a, Num b) => [(a, b)]
tree = [(1, 2), (1, 3), (2, 4), (2, 5), (3, 6), (3, 7), (4, 8), (4, 9), (5, 10), (5, 11), (6, 12), (6, 13), (7, 14), (7, 15)]


-- Name: main
-- Function: outputing in standard output format.
main :: IO () 
main = do
  ioStr <- seqn [getLine]
  [str] <- return (map (\x -> words x) ioStr)
  [a, b] <- return (map (\x -> read x::Integer) str)
  putStrLn (checkEdge2 (a, b) tree)
