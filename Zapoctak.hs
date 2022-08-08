{-# LANGUAGE RecordWildCards #-}
-- | Main module.
module Zapoctak
    () where

import Data.List ( elemIndex )
import Data.Maybe ( fromJust )
import qualified Data.Map

data Tree a =
    Node [a] [Tree a] (Tree a) (Tree a) Bool Integer | -- ^ Node in the middle of the trie, has format: Node [edge weights] [Links to subtrees] (suffix link to suffix node) (output link to solutions with same suffix) {does word end in this node?} {index of the node}
    Root [a] [Tree a] (Tree a) (Tree a) Bool Integer | -- ^ Root of the tree, has the same format as Node
    Nil -- ^ uninicialised node, usually new singleton or root points to this node , since there is no node that is it's suffix / shorter match
    deriving (Eq)

getRoot :: Integer -> Tree a
getRoot index = t where t = Root [] [] t Nil False index
{- ^ gets index of the root of new tree, outputs fully functional Root
>>> display (getRoot 0 :: Tree Int)
Koren [] [] (Just 0) Nothing False 0
-}

singleton :: Integer -> Tree k -- ^ returns new internal node of the tree
singleton = Node [] [] Nil Nil False

setelem:: Int -> [a] -> a -> [a]
setelem index list newValue =
  let (ys,zs) = splitAt index list in  ys ++ [newValue] ++ tail zs

{- | inserts new word into the trie. Indices of hte new nodes start from Integer on the input. Does not compute any output links or suffix links
>>> x = (insertWith 1 "a" (getRoot 0))
>>> display (snd x)
>>> fst x
Koren "a" [Uzol "" [] Nothing Nothing True 1] (Just 0) Nothing False 0
1
>>> x = (insertWith 1 "ab" (getRoot 0))
>>> y = (insertWith 3 "ac" (snd x))
>>> display (snd y)
>>> [fst x, fst y]
Koren "a" [Uzol "cb" [Uzol "" [] Nothing Nothing True 3,Uzol "" [] Nothing Nothing True 2] Nothing Nothing False 1] (Just 0) Nothing False 0
[2,3]
-}
insertWith :: Eq k => Integer -> [k] -> Tree k -> (Integer, Tree k)
insertWith _ _ Nil = error "insertWith: inserting into Nil"
insertWith _ [] t@(Node a b c d e f) = (f, Node a b c d True f) -- set that word ends in this node
insertWith _ [] t@(Root a b c d e f) = (f, Root a b c d True f) -- set that word ends in this node
insertWith n (x:xs) t@(Node a b c d e f)
  | x `elem` a = let (insertedIndex, newSubT) = insertWith n xs (b !! fromJust (elemIndex x a)) in (insertedIndex, Node a (setelem (fromJust (elemIndex x a)) b newSubT) c d e f)
  | otherwise  = let (insertedIndex, newSubT) = insertWith (n+1) xs (singleton n) in (insertedIndex, Node (x:a) (newSubT : b) c d e f)
  -- check if exists edge with weight 'x' in the current node, if yes, insert to subtree, otherwise insert entirely new subtree
insertWith n (x:xs) t@(Root a b c d e f)
  | x `elem` a = let (insertedIndex, newSubT) = insertWith n xs (b !! fromJust (elemIndex x a)) in (insertedIndex, Root a (setelem (fromJust (elemIndex x a)) b newSubT) c d e f)
  | otherwise  = let (insertedIndex, newSubT) = insertWith (n+1) xs (singleton n) in (insertedIndex, Root (x:a) (newSubT : b) c d e f)
  -- similar to the above

{-| Adds array of "WORDS" into trie. Starting index of new nodes is the Integer parameter. To prevent colisions of node indices, new words get inserted with Integer parameter greater by length of the previous word
>>>  x = (patternsToTrie 1 ["a"] (getRoot 0))
>>> fst x
>>> display (snd x)
[1,0]
Koren "a" [Uzol "" [] Nothing Nothing True 1] (Just 0) Nothing False 0
>>> x = patternsToTrie 1 ["ab", "ac"] (getRoot 0)
>>> fst x
>>> display (snd x)
[3,2,0]
Koren "a" [Uzol "bc" [Uzol "" [] Nothing Nothing True 3,Uzol "" [] Nothing Nothing True 2] Nothing Nothing False 1] (Just 0) Nothing False 0
-}
patternsToTrie :: Eq k => p -> [[k]] -> Tree k -> ([Integer], Tree k)
patternsToTrie n []     t = ([0], t)
patternsToTrie n (x:xs) t = let (insertedIndices, tree) = patternsToTrie n xs t in 
  let (newIndex, newTree) = insertWith (1 + head insertedIndices) x tree in (newIndex:insertedIndices, newTree)

{-| Finds first Child of lowest Parent Node that can extend some shorter suffix of Current Node by letter k.
For example in the picture bellow, let Current Node be Node 2 and k = B. Parent Node is Root 0 and the Child is Node 1.
Hence if we extend Node 2 by letter B, the longest matching suffix is Node 1.

         Root 0
        /    \
       /A    B\
      /        \
    Node 2     Node 1
    |
    |B
    |
   Node 3
Code for the example above:
>>> root = Root "AB" [node1, node2] root Nil False 0 where (node1, node2) = (Node "B" [node3] root Nil True 1, Node [] [] root Nil True 2) where node3 = (Node [] [] Nil Nil True 3)
>>> display (findMatchingSuffix 'B' ((\x@(Root _ b _ _ _ _) -> b !! 0) root))
Uzol "" [] (Just 0) Nothing True 2
-}
findMatchingSuffix :: Eq t => t -> Tree t -> Tree t
findMatchingSuffix k Nil = error "findMatchingSuffix: accesing uninicialized node"
findMatchingSuffix k (Node _ _ Nil _ _ _) = error "findMatchingSuffix: parent of the node is uninicialized"
findMatchingSuffix k t@(Node _ _ c@(Root a b _ _ _ _) _ _ _)
  | k `elem` a = b !! fromJust (elemIndex k a) -- we found the Child
  | otherwise = c -- no Child exists, therefore we return the Root (empty suffix ends with any letter)
findMatchingSuffix k t@(Node _ _ c@(Node a b _ _ _ _) _ _ _)
  | k `elem` a = b !! fromJust (elemIndex k a) -- we found the Child
  | otherwise = findMatchingSuffix k c -- recurse on the first shorter suffix
findMatchingSuffix k t@Root {} = t -- we already are in Root, so return it

{-| If some WORD ends in current Node, return current Node, otherwise return Node with longest
shorter suffix that has some word ending in it / Nil if no shorter suffix is a valid WORD
>>> root = Root "A" [node1] root Nil False 0 where node1 = Node "A" [node2] root Nil True 1 where node2 = Node [] [] node1 node1 False 2
>>> display (findOutputLink (down 0 $ down 0 $ root))
Uzol "A" [Uzol "" [] (Just 1) (Just 1) False 2] (Just 0) Nothing True 1
>>> root = Root "A" [node1] root Nil False 0 where node1 = Node "A" [node2] root Nil True 1 where node2 = Node [] [] node1 node1 True 2
>>> display (findOutputLink (down 0 $ down 0 $ root))
Uzol "" [] (Just 1) (Just 1) True 2
-}
findOutputLink :: Tree k -> Tree k
findOutputLink t@(Node _ _ _ d e _)
  | e = t
  | otherwise = d
findOutputLink t@(Root _ _ _ _ e _)
  | e = t
  | otherwise = Nil
findOutputLink t = Nil

{- |Creates all backlink edges, meaning: suffix link and output link for every node
>>> display (createBacklinks (snd (patternsToTrie 1 ["b", "ab", "bab"] (getRoot 0))))
Koren "ab" [Uzol "b" [Uzol "" [] (Just 1) (Just 1) True 5] (Just 0) Nothing False 4,Uzol "a" [Uzol "b" [Uzol "" [] (Just 5) (Just 5) True 3] (Just 4) Nothing False 2] (Just 0) Nothing True 1] (Just 0) Nothing False 0
-}
createBacklinks :: Eq k => Tree k -> Tree k
createBacklinks ol'tree = go ol'tree ol'tree (error "createBacklinks: internal error") -- node, parent, edge weight from parent to node, if you don't have parent and still try to read edge weight, throws an error
  where
    go Nil _ _ = Nil
    go _ Nil _ = error "createBacklinks: internal error"
    go t@(Root a b c d e f) _ _ = node
      where
        node = Root a subT c d e f -- reference to the final root
        subT = [go x node l | (x,l) <- zip b a] -- pass reference to the final root and compute the subtrees
    go t@(Node a b _ _ e f) p@Root {} letter = node
      where
        c' = findMatchingSuffix letter p -- get node that is shorter suffix of current node t.
        node = Node a subT c' (findOutputLink c') e f -- replace t with it's final version on the output
        subT = [go x node l | (x,l) <- zip b a] -- recursively compute children of t.
    go t@(Node a b _ _ e f) p@Node {} letter = node -- analogous
      where
        c' = findMatchingSuffix letter p
        node = Node a subT c' (findOutputLink c') e f
        subT = [go x node l | (x,l) <- zip b a]

{- |Outputs the index of vertex on the input
>>> value (getRoot 0)
Just 0
>>> value $ down 0 $ down 0 $ down 1 $ createBacklinks (patternsToTrie 1 ["b", "ab", "bab", ""] (getRoot 0))
Just 5
-}
value :: Tree a -> Maybe Integer
value Nil = Nothing
value (Node _ _ _ _ _ f) = Just f
value (Root _ _ _ _ _ f) = Just f

{-| Output is array of indices of nodes which are reachable by outut links from node on input (including index of node on the input)
>>> findShorterMatches $ down 0 $ down 0 $ down 1 $ createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab"] (getRoot 0))
[Just 3,Just 5,Just 1]
>>> findShorterMatches $ down 0 $ down 0 $ down 1 $ createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab", ""] (getRoot 0))
>>> display (createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab"] (getRoot 0)))
[Just 3,Just 5,Just 1,Just 0]
Koren "ab" [Uzol "b" [Uzol "" [] (Just 1) (Just 1) True 5] (Just 0) Nothing False 4,Uzol "a" [Uzol "b" [Uzol "" [] (Just 5) (Just 5) True 3] (Just 4) Nothing False 2] (Just 0) Nothing True 1] (Just 0) Nothing False 0
-}
findShorterMatches :: Tree a -> [Maybe Integer]
findShorterMatches t@(Node _ _ _ d _ _) = value t : findShorterMatches d
findShorterMatches t@(Root _ _ _ _ e _) = [value t]
findShorterMatches _ = []

{-| gets a string and root of a tree on the input. Returns list, where on index i is list of
  indices of vertices that have some WORD ending in them and this WORD forms a suffix of text
>>> display (createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab", ""] (getRoot 0)))
Koren "ab" [Uzol "b" [Uzol "" [] (Just 1) (Just 1) True 5] (Just 0) (Just 0) False 4,Uzol "a" [Uzol "b" [Uzol "" [] (Just 5) (Just 5) True 3] (Just 4) (Just 0) False 2] (Just 0) (Just 0) True 1] (Just 0) Nothing True 0
>>> matchText "abbababa" (createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab", ""] (getRoot 0)))
[[Just 0],[Just 0],[Just 5,Just 1,Just 0],[Just 1,Just 0],[Just 0],[Just 3,Just 5,Just 1,Just 0],[Just 0],[Just 3,Just 5,Just 1,Just 0],[Just 0]]
>>> matchText "abbababa" (createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab"] (getRoot 0)))
[[],[],[Just 5,Just 1],[Just 1],[],[Just 3,Just 5,Just 1],[],[Just 3,Just 5,Just 1],[]]
-}
matchText :: (Eq a) => [a] -> Tree a -> [[Maybe Integer]]
matchText _ Nil = error "matchText: access to Nil error" -- something failed and we got into Nil node
matchText (x:xs) t@(Node a b _ d e _) -- general case
  | e = findShorterMatches t : rest -- either we include current node in matches
  | otherwise = findShorterMatches d : rest -- or we don't if no WORD ends in it
    where
      rest
        | x `elem` a = matchText xs (b !! fromJust (elemIndex x a)) -- there exists edge with weight x
        | otherwise = matchText xs (findMatchingSuffix x t) -- we have to find node that extends longest possible suffix
matchText (x:xs) t@(Root a b _ d e _)
  | e = findShorterMatches t : rest -- either we include root in matches
  | otherwise = findShorterMatches d : rest -- or we don't if no WORD ends in it
    where
      rest
        | x `elem` a = matchText xs (b !! fromJust (elemIndex x a)) -- same
        | otherwise = matchText xs (findMatchingSuffix x t) -- same
matchText [] t@(Node _ _ _ d e _) -- we are at the end of input
  | e = [findShorterMatches t]
  | otherwise = [findShorterMatches d]
matchText [] t@(Root _ _ _ d e _) -- we are at the end of input
  | e = [findShorterMatches t]
  | otherwise = [findShorterMatches d]

{-| Extracts list of node indices that have some WORD ending in them
>>> trueNodes (createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab", ""] (getRoot 0)))
[3,1,5]
-}
trueNodes :: Tree k -> [Integer]
trueNodes t@(Root _ b _ _ _ _) = concat [trueNodes subT | subT <- b]
trueNodes t@(Node _ b _ _ e f)
  | e = f:concat [trueNodes subT | subT <- b]
  | otherwise = concat [trueNodes subT | subT <- b]
trueNodes Nil = []

{-| gets two lists and zips them into classic dictionary. Takes only min(len list1, len list2) elements from both lists
>>> matchNodesToStrings [1,2,3] ["Gimme", "zapocet", "pls", "a", "povedz", "ze", "moj", "zapoctak", "je", "uzastny"]
fromList [(1,"Gimme"),(2,"zapocet"),(3,"pls")]
-}
matchNodesToStrings :: Ord k => [k] -> [a] -> Data.Map.Map k a
matchNodesToStrings wordIndices words = Data.Map.fromList (zip wordIndices words)

{-| Creates the whole tree needed for Aho-Corasick just from the list of words. Output also includes dictionary for decoding, which
words are on the output, since by default the tree outputs only indices of vertices that we matched.
>>> x = createTree ["b", "ab", "bab"]
>>> display $ fst x
>>> snd x
Koren "ab" [Uzol "b" [Uzol "" [] (Just 1) (Just 1) True 5] (Just 0) Nothing False 4,Uzol "a" [Uzol "b" [Uzol "" [] (Just 5) (Just 5) True 3] (Just 4) Nothing False 2] (Just 0) Nothing True 1] (Just 0) Nothing False 0
fromList [(1,"b"),(3,"bab"),(5,"ab")]
-}
createTree :: Eq k => [[k]] -> (Tree k, Data.Map.Map Integer [k])
createTree xs = let (wordIndices,t) = patternsToTrie 1 xs (getRoot 0) in let t' = createBacklinks t in (t', matchNodesToStrings wordIndices xs)

{-| Decodes output of the tree with the dictionary
>>> x = createTree ["b", "ab", "bab"]
>>> matchText "abbababa" (fst x)
>>> matchedToStrings (matchText "abbababa" (fst x)) (snd x)
[[],[],[Just 5,Just 1],[Just 1],[],[Just 3,Just 5,Just 1],[],[Just 3,Just 5,Just 1],[]]
[[],[],["ab","b"],["b"],[],["bab","ab","b"],[],["bab","ab","b"],[]]
-}
matchedToStrings :: Ord k => [[Maybe k]] -> Data.Map.Map k a -> [[a]]
matchedToStrings xs slov = [[fromJust (Data.Map.lookup (fromJust y) slov) | y <- x] | x <- xs]

{-| Erases the first null elemet from matches which comes from the fact that I consider empty string as first input and I always just add letters
>>> x = createTree ["b", "ab", "bab"]
>>> matchText "abbababa" (fst x)
>>> getJustMatches "abbababa" x 
[[],[],[Just 5,Just 1],[Just 1],[],[Just 3,Just 5,Just 1],[],[Just 3,Just 5,Just 1],[]]
[[],[Just 5,Just 1],[Just 1],[],[Just 3,Just 5,Just 1],[],[Just 3,Just 5,Just 1],[]]
-}
getJustMatches :: Eq a => [a] -> (Tree a, b) -> [[Maybe Integer]]
getJustMatches text strom = tail (matchText text (fst strom))

{-| Creates list where on position K are patterns S_1,...,S_M if they are suffix of first K letters of Text in which we are searching
>>> x = createTree ["b", "ab", "bab"]
>>> getTextualMatches "abbababa" x 
[[],["ab","b"],["b"],[],["bab","ab","b"],[],["bab","ab","b"],[]]
-}
getTextualMatches :: Eq a1 => [a1] -> (Tree a1, Data.Map.Map Integer a2) -> [[a2]]
getTextualMatches text strom = matchedToStrings (getJustMatches text strom) (snd strom)

{-| Creates ordered dictionary from list where one entry has the form (index, elem)
>>> compress 1 [[],["ab","b"],["b"],[],["bab","ab","b"],[],["bab","ab","b"],[]]
[(2,"ab"),(2,"b"),(3,"b"),(5,"bab"),(5,"ab"),(5,"b"),(7,"bab"),(7,"ab"),(7,"b")]
-}
compress :: Num a1 => a1 -> [[a2]] -> [(a1, a2)]
compress n ([]:xs) = compress (n+1) xs
compress n (x:xs) = (n, head x) : compress n (tail x:xs)
compress _ [] = []

{-| Finds patterns in text:
>>> findText ["hello", "ll"] "hello world hello"  
[(4,"ll"),(5,"hello"),(16,"ll"),(17,"hello")]
-}
findText :: (Num a2, Eq a3) => [[a3]] -> [a3] -> [(a2, [a3])]
findText patterns text = compress 1 (getTextualMatches text (createTree patterns))

{-Example trees-}
strom :: (Tree Char, Data.Map.Map Integer [Char])
strom = createTree ["abc", "dabc", "bc"]
inystrom :: (Tree Char, Data.Map.Map Integer [Char])
inystrom = createTree ["he", "she", "hers", "his"]

{-| Data structure without the obnoxious backlinks making it Showable-}
data Dtree k =
  Koren [k] [Dtree k] (Maybe Integer) (Maybe Integer) Bool Integer |
  Uzol [k] [Dtree k] (Maybe Integer) (Maybe Integer) Bool Integer |
  List deriving (Show, Eq)

{-| Function to convert Aho-Corasick Tree into showable Dtree.
>>> display (getRoot 0 :: Tree Int)
Koren [] [] (Just 0) Nothing False 0
-}
display :: Tree a -> Dtree a
display t@(Root a b c d e f) = Koren a [display x | x <- b] (value c) (value d) e f
display t@(Node a b c d e f) = Uzol a [display x | x <- b] (value c) (value d) e f
display Nil = List

{-| Function to traverse Aho-Corasick Tree downwards.
>>> display $ down 0 $ down 0 $ down 1 $ createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab", ""] (getRoot 0))
Uzol "" [] (Just 5) (Just 5) True 3
>>> display $ down 0 $ down 1 $ createBacklinks (snd $ patternsToTrie 1 ["b", "ab", "bab", ""] (getRoot 0))
Uzol "b" [Uzol "" [] (Just 5) (Just 5) True 3] (Just 4) (Just 0) False 2
-}
down :: Int -> Tree a -> Tree a
down k t@(Root _ b _ _ _ _) = b !! k
down k t@(Node _ b _ _ _ _) = b !! k
down _ Nil = Nil

{-| Go to node pointed by output link-}
resUp :: Tree a -> Tree a
resUp t@(Root _ _ _ d _ _) = d
resUp t@(Node _ _ _ d _ _) = d
resUp Nil = Nil

{-| Go to node pointed by suffix link-}
backUp :: Tree a -> Tree a
backUp t@(Root _ _ c _ _ _) = c
backUp t@(Node _ _ c _ _ _) = c
backUp Nil = Nil
