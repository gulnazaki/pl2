import Binomial

main :: IO ()
main = do
    t <- readLn
    cases <- readLines t []
    let results = map combs cases
    mapM_ print results