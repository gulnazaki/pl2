import Data.Char
import System.IO
import Text.Read
import Data.Maybe
import Data.List (sort, nub)
import qualified Data.HashMap.Strict as M
import qualified Data.IntMap.Strict as IM

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq

type Constraints = [(Type, Type)]
type Context     = M.HashMap String Int
type Mapping     = IM.IntMap Int

-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Main program

exprToType :: Expr -> (Context, Int) -> (Type, (Constraints, Int))
exprToType (Evar x) (c, d) = case M.lookup x c of
                                 Just v  -> ((Tvar v, ([], d)))
                                 Nothing -> ((Tvar d, ([], succ d)))

exprToType (Eabs x e) (c, d) = 
    let c' = M.insert x d c
        (t, (cs, d')) = exprToType e (c', succ d)
    in  ((Tfun (Tvar d) t), (cs, d'))

exprToType (Eapp e e') (c, d) = 
    let (t1, (cs1, d1)) = exprToType e  (c, succ d)
        (t2, (cs2, d2)) = exprToType e' (c, d1)
    in  (Tvar (d), (cs1 ++ cs2 ++ [(t1, Tfun t2 $ Tvar d)], d2))

mgu :: Maybe Type -> Constraints -> Maybe Type
mgu t []                                  = t
mgu t (c:cs)
    | fst c == snd c                      = mgu t cs
    | (Tvar a, t2) <- c , (a `notIn` t2)  = mgu (Just (replace (fromJust t) (a, t2))) $ replaceConstraints cs (a,t2)
    | (t1, Tvar a) <- c , (a `notIn` t1)  = mgu (Just (replace (fromJust t) (a, t1))) $ replaceConstraints cs (a,t1)
    | (Tfun t11 t12, Tfun t21 t22) <- c   = mgu t ((t11, t21):(t12, t22):cs)
    | otherwise                           = Nothing

notIn :: Int -> Type -> Bool
notIn a (Tvar b)     = a /= b
notIn a (Tfun t1 t2) = a `notIn` t1 && a `notIn` t2

replace :: Type -> (Int, Type) -> Type
replace (Tvar a) (b, t) = if a == b then t else Tvar a
replace (Tfun t1 t2) r  = Tfun (replace t1 r) (replace t2 r)

replaceConstraints :: Constraints -> (Int, Type) -> Constraints
replaceConstraints cs r = map (\(t1, t2) -> (replace t1 r, replace t2 r)) cs

getNums :: Type -> [Int]
getNums (Tvar a)     = [a]
getNums (Tfun t1 t2) = getNums t1 ++ getNums t2

smallify :: Type -> Mapping -> Type
smallify (Tvar a) m     = Tvar $ fromJust $ IM.lookup a m
smallify (Tfun t1 t2) m = Tfun (smallify t1 m) (smallify t2 m)

smallest :: Type -> Type
smallest t = 
    let nums = nub $ getNums t
        small = [0..length nums]
    in smallify t $ IM.fromList $ zip nums small


readOne  =  do  s <- getLine
                let e = read s :: Expr
                    (t, (cs, _)) = exprToType e (M.empty, 0)
                case mgu (Just t) cs of
                    Just general_t -> print $ smallest general_t
                    Nothing -> putStrLn "type error"

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                count n readOne
