import Binomial
import Control.Concurrent.Async
import Control.Exception
import Control.DeepSeq

main :: IO ()
main = do
    t <- readLn
    cases <- readLines t []
    evaluate $ force cases
    wait_results <- mapM (async . return . combs) cases
    results <- mapM wait wait_results
    mapM_ print results
    return ()