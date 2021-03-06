module LogAnalysis where

import Log
import Control.Applicative


parseMessage :: String -> LogMessage
parseMessage raw =
  case words raw of
       ("I":(time:rest)) -> LogMessage Info (read time :: Int) (unwords rest)
       ("W":(time:rest)) -> LogMessage Warning (read time :: Int) (unwords rest)
       ("E":(code:(time:rest))) -> LogMessage (Error (read code :: Int)) (read time :: Int) (unwords rest)
       _ -> Unknown raw

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines


insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert inmsg Leaf = Node Leaf inmsg Leaf
insert inmsg@(LogMessage _ intime _) (Node left destmsg@(LogMessage _ desttime _) right)
  | intime < desttime = Node (insert inmsg left) destmsg right
  | intime > desttime = Node right destmsg (insert inmsg right)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder (Leaf) = []
inOrder (Node left msg right)  = (inOrder left) ++ [msg] ++ (inOrder right)

isSevereError :: LogMessage -> Bool
isSevereError (LogMessage (Error severity) _ _)
  | severity >= 50 = True
  | otherwise = False
isSevereError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = map (\(LogMessage (Error _) _ msg) -> msg) $ inOrder $ build $ filter isSevereError xs

-- Not sure how he intended us to test intermediate code without writing something like the below
-- The use of fmap is a bit ahead of ourselves..?
doit :: Int -> IO [LogMessage]
doit count = inOrder <$> build <$> (testParse parse count "error.log")




