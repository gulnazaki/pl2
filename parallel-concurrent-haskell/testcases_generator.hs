import Test.QuickCheck
import Control.Monad
import Data.List (intercalate)
import Data.Numbers.Primes (isPrime)

newtype TestCase = TestCase (Int, Int, Int)
                deriving Show

instance Arbitrary TestCase where
    arbitrary = do
        p <- choose (0, lim) `suchThat` isPrime
        n <- oneof [chooseInt (0, p `div` 10), chooseInt (0, p `div` 100), chooseInt (0, p `div` 10000)]
        k <- chooseInt (0, n)
        return $ TestCase (n, k, p)

writeTC :: Int -> IO ()
writeTC t = do
    tc <- generate $ vectorOf t arbitrary :: IO ([TestCase])
    let tcs = map (\(TestCase (n, k, p)) -> intercalate " " $ map show [n, k, p]) tc
    let s = intercalate "\n" ((show t):tcs)
    let f = "testcase_" ++ show t ++ ".txt"
    writeFile f s

lim = 10^9
testcases = 10
starting_from = 5

main :: IO ()
main = mapM_ writeTC $ take testcases $ iterate (*2) starting_from