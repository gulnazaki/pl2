import Binomial
import Control.Parallel.Strategies
import Control.Exception
import Control.DeepSeq

main :: IO ()
main = do
    t <- readLn
    cases <- readLines t []
    evaluate $ force cases
    let results = parMap rpar combs cases
    mapM_ print results