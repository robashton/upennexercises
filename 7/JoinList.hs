module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) one two = Append ((tag one) <> (tag two)) one two

tag :: Monoid m => JoinList m a -> m
tag (Single annotation _) = annotation
tag (Append annotation _ _) = annotation

-- This also feels unnecessary
getInt :: Sized b => b -> Int
getInt = getSize . size

-- This feels unnecessary
tagval :: (Sized m, Monoid m) => JoinList m a -> Int
tagval = getInt . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single t v)
  | i == getInt t = Just v
  | otherwise = Nothing
indexJ i (Append t l r)
  | i < (getInt t) = indexJ i l
  | otherwise = indexJ (i - tagval l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i n@(Single t _)
  | i >= getInt t = n
  | otherwise = Empty
dropJ i (Append t l r)
  | i >= getInt t = Empty
  | i == tagval l = r
  | i < tagval l = let nl = (dropJ i l) in
                       Append ((tag nl) <> tag r) nl r
  | otherwise = dropJ (i - tagval l) r


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i n@(Single t _)
  | i >= getInt t = n
  | otherwise = Empty
takeJ i n@(Append t l r)
  | i >= getInt t = n
  | i == tagval l = l
  | i < tagval l = takeJ i l
  | otherwise = let nr  = (takeJ (i - tagval l) r) in
                    Append (tag l <> (tag nr)) l nr


-- Testing helpficiations

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

testLeaf :: String -> JoinList Size String
testLeaf phrase = Single (Size 1) phrase

-- A balanced list
testList :: JoinList Size String
testList = ((testLeaf "say") +++ (testLeaf "hello")) +++ ((testLeaf "to") +++ (testLeaf "my")) +++ ((testLeaf "little") +++ (testLeaf "friend"))


