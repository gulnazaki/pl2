import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List

data Tree a = Node a [Tree a]
  deriving Show

dfn :: Tree a -> Tree (a, Integer)
dfn t = fst (aux 1 t)
  where aux :: Integer -> Tree a ->
               (Tree (a, Integer), Integer)
        aux k (Node x ts) = (Node (x, k) ts', k')
          where (ts', k') = auxs (k+1) ts
        auxs :: Integer -> [Tree a] ->
                ([Tree (a, Integer)], Integer)
        auxs k [] = ([], k)
        auxs k (t : ts) = (t' : ts', k'')
          where (t', k') = aux k t
                (ts', k'') = auxs k' ts

bfn :: Tree a -> Tree (a, Integer)
bfn t = t'
  where (t', ks') = aux ks t
        ks = 1 : ks'
        aux (k : ks) (Node x ts) = (Node (x, k) ts', (k+1) : ks')
          where (ts', ks') = auxs ks ts
        auxs ks [] = ([], ks)
        auxs ks (t : ts) = (t' : ts', ks'')
          where (t', ks') = aux ks t
                (ts', ks'') = auxs ks' ts

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized arbitraryTree
    shrink (Node x []) = []
    shrink (Node x ts) = init $ subTrees (Node x ts)

subTrees :: Arbitrary a => Tree a -> [Tree a]
subTrees (Node x []) = [Node x []]
subTrees (Node x ts) = concatMap subTrees ts ++ [Node x t | t <- tail $ subsequences ts]

arbitraryTree :: Arbitrary a => Int -> Gen (Tree a)
arbitraryTree m = do
    node <- arbitrary
    n <- choose (0, m)
    d <- choose (0, n)
    children <- vectorOf (m `div` (d+1)) $ arbitraryTree $ n `div` (d + 1)
    return $ Node node children

prop_size_unchanged :: Tree a -> Bool
prop_size_unchanged t =
    let size_ = size t
    in (size_ == size (dfn t)) && (size_ == size (bfn t))

prop_root_one :: Tree a -> Bool
prop_root_one t =
    let (Node (_,dfRoot) _) = dfn t
        (Node (_,bfRoot) _) = bfn t
    in (dfRoot == 1) && (bfRoot == 1)

-- ensures that each node's mark is smaller than the minimum mark of it's children
prop_ascending_order :: Tree a -> Bool
prop_ascending_order t = (ascending $ dfn t) && (ascending $ bfn t)

-- keeping all marks on dfs order as defined in allNums makes the list [1..<num-of-nodes>]
-- which ensures that there are no duplicates, no mark missing and the order of the dfs traversal is correct
prop_dfs_order :: Tree a -> Bool
prop_dfs_order t = [1..size t] == (allNums $ dfn t)

-- this ensures that bfs traversal marks no duplicates and misses no number in range [1..<num-of-nodes>]
prop_bfs_nums :: Tree a -> Bool
prop_bfs_nums t = [1..size t] == (sort $ allNums $ bfn t)

-- this ensures that every node's child has a larger mark than any of this node's "siblings"
-- this combined with the property above proves that the order of the bfs traversal is correct
prop_bfs_order :: Tree a -> Bool
prop_bfs_order t = minChildMaxSibling 1 $ bfn t

size :: Tree a -> Integer
size (Node a []) = 1
size (Node a ts) = foldr ((+) . size) 1 ts

ascending :: Tree (a, Integer) -> Bool
ascending (Node _ []) = True
ascending (Node (_, a) ts) = foldr ((&&) . ascending) (a < minimum [a | (Node (_, a) t) <- ts]) ts

allNums :: Tree (a, Integer) -> [Integer]
allNums (Node (_, a) []) = [a]
allNums (Node (_, a) ts) = a : concatMap allNums ts

minChildMaxSibling :: Integer -> Tree (a, Integer) -> Bool
minChildMaxSibling m (Node _ []) = True
minChildMaxSibling m (Node (_, a) ts) = foldr ((&&) . minChildMaxSibling max) (m < min) ts where
    max = maximum [a | (Node (_, a) t) <- ts]
    min = minimum [a | (Node (_, a) t) <- ts]


merge :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
merge f (Node x tsx) (Node y tsy) = Node (f x y) $ zipTreesWith (merge f) tsx tsy

zipTreesWith :: (Tree a -> Tree a -> Tree a) -> [Tree a] -> [Tree a] -> [Tree a]
zipTreesWith m (x:xs) (y:ys) = m x y : zipTreesWith m xs ys
zipTreesWith m [] (y:ys) = y : zipTreesWith m [] ys
zipTreesWith m (x:xs) [] = x : zipTreesWith m xs []
zipTreesWith _ _ _ = []

wrong :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
wrong f (Node x tsx) (Node y tsy) = Node (f x y) $ zipWith (wrong f) tsx tsy

common_size :: (Tree a, Tree a) -> Integer
common_size ((Node _ tsx), (Node _ tsy)) = foldr ((+) . common_size) 1 $ zip tsx tsy

prop_size_of_merge :: ((a -> a -> a) -> Tree a -> Tree a -> Tree a) -> Fun (a, a) a -> Tree a -> Tree a -> Bool
prop_size_of_merge merge (Fn2 f) tx ty = 
    let common = common_size(tx, ty)
        x_only = size tx - common
        merged_size = size ty + x_only
    in merged_size == size (merge f tx ty)


main :: IO ()
main = do
    quickCheck (prop_size_unchanged :: Tree Int -> Bool)
    quickCheck (prop_root_one :: Tree Int -> Bool)
    quickCheck (prop_ascending_order :: Tree Int -> Bool)
    quickCheck (prop_dfs_order :: Tree Int -> Bool)
    quickCheck (prop_bfs_nums :: Tree Int -> Bool)
    quickCheck (prop_bfs_order :: Tree Int -> Bool)

    quickCheck (prop_size_of_merge (wrong :: (Int -> Int -> Int) -> Tree Int -> Tree Int -> Tree Int))
    quickCheck (prop_size_of_merge (merge :: (Int -> Int -> Int) -> Tree Int -> Tree Int -> Tree Int))
