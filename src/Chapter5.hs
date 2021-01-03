module Chapter5 where

import Data.Bifunctor

data LNTree a b = Leaf a | Node (LNTree a b) b (LNTree a b)

-- Exercise 5.1
type LNTreeAlgebra a b r = (
        a -> r,
        r -> b -> r -> r
    )

foldLNTree :: LNTreeAlgebra a b r -> LNTree a b -> r
foldLNTree (leaf, node) = fold
    where 
        fold (Leaf x) = leaf x
        fold (Node l x r) = node (fold l) x (fold r)


-- Exercise 5.2
data BinTree x = BinLeaf x | Bin (BinTree x) (BinTree x)

type BinTreeAlgebra x r = (
        x -> r, 
        r -> r -> r
    )

foldBinTree :: BinTreeAlgebra x r -> BinTree x -> r
foldBinTree (leaf, bin) = fold
    where 
        fold (BinLeaf x) = leaf x
        fold (Bin l r) = bin (fold l) (fold r)

binHeight :: BinTree x -> Int
binHeight = foldBinTree (const 0, \l r -> (l `max` r) + 1)

binFlatten :: BinTree x -> [x]
binFlatten = foldBinTree ((:[]), (++))

maxBinTree :: Ord x => BinTree x -> x
maxBinTree = foldBinTree (id, max)

binShortestPath :: BinTree x -> Int
binShortestPath = foldBinTree (const 0, \l r -> (l `min` r) + 1) 

mapBinTree :: (x -> r) -> BinTree x -> BinTree r
mapBinTree f = foldBinTree (BinLeaf . f, Bin)

-- Exercise 5.3
data Direction = L | R

allPathsExplicit :: BinTree x -> [(x, [Direction])]
allPathsExplicit (BinLeaf x) = [(x, [])]
allPathsExplicit (Bin l r) = map (second (L:)) (allPathsExplicit l) ++ map (second (R:)) (allPathsExplicit l)

allPathsFold :: BinTree x -> [(x, [Direction])]
allPathsFold = foldBinTree (
        \x -> [(x, [])], 
        \l r -> map (second (L:)) l ++ map (second (R:)) r
    )

-- Exercise 5.4
data Resist = Res Float | Resist :|: Resist | Resist :*: Resist

type ResistAlgebra r = (
        Float -> r,
        r -> r -> r,
        r -> r -> r
    )

foldResist :: ResistAlgebra r -> Resist -> r
foldResist (res, par, seq) = fold
    where 
        fold (Res f) = res f
        fold (l :|: r) = par (fold l) (fold r)
        fold (l :*: r) = seq (fold l) (fold r)

result :: Resist -> Float
result = foldResist (id, \l r -> 1.0 / (1.0/l + 1.0/r), (+))