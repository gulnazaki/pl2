import Binomial
import Control.Concurrent.Async
import qualified Data.Vector.Unboxed.Mutable as VM
import qualified Data.Vector.Unboxed as V
import Control.Monad.Primitive (RealWorld)
import Control.Exception
import Control.DeepSeq

writeToVector :: VM.IOVector Int -> (Int,(Int,Int,Int)) -> IO (VM.IOVector Int)
writeToVector vector (t, cs) = do
    let res = combs cs
    VM.unsafeWrite vector t res
    return vector

main :: IO ()
main = do
    t <- readLn
    cases <- readLines t []
    evaluate $ force cases
    mvector <- VM.unsafeNew t :: IO (VM.IOVector Int)
    mapConcurrently_ (writeToVector mvector) $ zip [0..t-1] cases
    vector <- V.unsafeFreeze mvector
    mapConcurrently_ (\i -> print $ vector V.! i) [0..t-1]
