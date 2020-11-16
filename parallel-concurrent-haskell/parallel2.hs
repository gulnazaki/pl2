import Binomial
import Control.Monad.Par
import Control.Exception
import Control.DeepSeq

main :: IO ()
main = do
    t <- readLn
    cases <- readLines t []
    evaluate $ force cases
    let results = runPar $ parMap combs cases
    mapM_ print results