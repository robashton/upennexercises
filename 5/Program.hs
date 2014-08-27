{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Program where

import StackVM
import Parser

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr Program where
  lit x = [(PushI x)]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile str = parseExp lit add mul str

interpret :: String -> Either String StackVal
interpret str = case compile str of
                     Just program -> stackVM program
                     _ -> Left "Oh dear, didn't compile"

