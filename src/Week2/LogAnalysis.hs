module Week2.LogAnalysis where

import Data.String
import Week2.Log

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractMsg . inOrder . build . filter (isSevere 50) . filter isError
  where
    extractMsg :: LogMessage -> String
    extractMsg (LogMessage _ _ msg) = msg
    extractMsg _ = error "expected LogMessage but was not"

    isSevere :: Int -> LogMessage -> Bool
    isSevere n1 (LogMessage (Error n2) _ _) = n2 >= n1
    isSevere _ _ = False 

    isError :: LogMessage -> Bool
    isError (LogMessage (Error _) _ _) = True
    isError _ = False

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree msg rTree) = inOrder lTree ++ [msg] ++ inOrder rTree

build :: [LogMessage] -> MessageTree
build logs = foldr insert Leaf (reverse logs)

insert :: LogMessage -> MessageTree -> MessageTree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ newTime _) (Node left tmsg@(LogMessage _ time _) right)
  | newTime < time = Node (insert msg left) tmsg right
  | otherwise = Node left tmsg (insert msg right)
insert _ tree = tree

parse :: String -> [LogMessage]
parse s = parseMessage <$> lines s

parseMessage :: String -> LogMessage
parseMessage m = let wl = words m in
  case wl of
    ("I":ts:mx) -> LogMessage Info (read ts) (unwords mx)
    ("W":ts:mx) -> LogMessage Warning (read ts) (unwords mx)
    ("E":sev:ts:mx) -> LogMessage (Error (read sev)) (read ts) (unwords mx)
    _ -> Unknown $ unwords wl
