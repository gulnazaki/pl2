import qualified Data.ByteString.Char8 as C
import qualified Data.Maybe as M
import Data.Vector as V  
import Data.Vector.Unboxed as U
import Data.List (foldl1')

readline::IO (Int,Int)
readline = do
	line <- C.getLine
	let [a,b] = (fst . M.fromJust . C.readInt) <$> C.words line
	return (a,b)

processlines::Int -> V.Vector Int -> Int -> IO ()
processlines 0 _ _ = return ()
processlines n cumvector m = do
	(a,b) <- readline
	let x = if a == 0 then 1 else cumvector V.! (a - 1)
	let y = cumvector V.! b
	print $ (y - x) `mod` m
	processlines (n - 1) cumvector m

createvector::Int -> Int -> V.Vector Int
createvector n m = vector
	where
		vector = V.generate n ways
		wins = [2^powerof - 1 | powerof <- [1..19]]
		ways n
			| (n == 0 || n == 1 || n == 2) = 2
			| otherwise 		 = (Data.List.foldl1' (\x y -> (x + y) `mod` m) [vector V.! (n - streak) | streak <- wins, n >= streak])


main::IO ()
main = do
	(n,m) <- readline
	let limit = 1000000
	let vector = createvector (limit+1) m
	let cumvector = V.scanl1' (\x y -> (x + y) `mod` m) vector
	processlines n cumvector m
