module Calc where
import ExprT
import Parser

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7 = Mod7 Integer deriving (Eq, Show, Ord)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add left right) = (eval left) + (eval right)
eval (Mul left right) = (eval left) * (eval right)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x = x > 0
  add x y = or [x,y]
  mul x y = and [x,y]

instance Expr MinMax where
  lit x = MinMax x
  add x y = max x y
  mul x y = min x y

instance Expr Mod7 where
  lit x = Mod7 x
  add (Mod7 x) (Mod7 y) = Mod7 (head $ dropWhile (>7) $ iterate (subtract 7) (x+y))
  mul (Mod7 x) (Mod7 y) = Mod7 (head $ dropWhile (>7) $ iterate (subtract 7) (x*y))

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
               Just expr -> Just (eval expr)
               _ -> Nothing

exprt :: ExprT -> ExprT
exprt = id

