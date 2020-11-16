import qualified Data.ByteString.Char8 as C
import qualified Data.Maybe as M
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V
import Data.List (foldl1')
import Control.Monad.Primitive (RealWorld)

readline :: IO (Int,Int)
readline = do
    line <- C.getLine
    let [a,b] = (fst . M.fromJust . C.readInt) <$> C.words line
    return (a,b)

processlines :: Int -> V.Vector Int -> Int -> IO ()
processlines 0 _ _ = return ()
processlines n cumvector m = do
    (a,b) <- readline
    let x = if a == 0 then 1 else cumvector V.! (a - 1)
    let y = cumvector V.! b
    print $ (y - x) `mod` m
    processlines (n - 1) cumvector m

compute :: VM.IOVector Int -> Int -> Int -> [Int] -> Int -> IO Int
compute vector n m [] acc = return acc
compute vector n m ws acc =
    if n < 0
        then return acc
        else do
            curr <- VM.unsafeRead vector n
            compute vector (n - head ws) m (tail ws) ((acc + curr) `mod` m)

createvector :: Int -> Int -> IO (VM.IOVector Int)
createvector n m = do
    vector <- VM.unsafeNew n :: IO (VM.IOVector Int)
    let w:ws = [2^powerof | powerof <- [0..19]]
    let ways 0 = VM.unsafeWrite vector 0 2 :: IO ()
        ways 1 = VM.unsafeWrite vector 1 2
        ways 2 = VM.unsafeWrite vector 2 2
        ways n = do
            res <- compute vector (n - w) m ws 0
            VM.unsafeWrite vector n res
    mapM_ ways [0..n]
    return vector

main :: IO ()
main = do
    (n,m) <- readline
    let limit = 1000000
    mvector <- createvector (limit+1) m
    vector <- V.unsafeFreeze mvector
    let cumvector = V.scanl1' (\x y -> (x + y) `mod` m) vector
    processlines n cumvector m
