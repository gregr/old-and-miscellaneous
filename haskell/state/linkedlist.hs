module Main where

import Control.Monad.ST
import Data.STRef

type NodeRef a = forall s. ST s (STRef s (LinkedList a))
--type NodeRef a = forall s. STRef s (LinkedList a)

data LinkedList a = Node {prev :: (NodeRef a),
                          next :: (NodeRef a),
                          element :: a}
                  | Empty

showSecond = let orig = lToLL [1..4]
             in do
                 r <- next (orig)
                 ll <- readSTRef r
                 attachToPrev ll orig
                 showLLnormal ll

showLL Empty shower = return "Empty"
showLL (Node p n v) shower = do
  pref <- p
  nref <- n
  pval <- readSTRef pref
  nval <- readSTRef nref
  rest <- (showLL nval showNothing)
  return ((shower pval) ++ "| "
          ++ (show v) ++ " |"
          ++ rest)

showLLnormal n = showLL n showLLelement

showNothing _ = ""

showLLelement Empty = "Empty"
showLLelement (Node _ _ v) = "more <-"-- show v

getLL nr = do
  lr <- nr
  readSTRef lr

llToL Empty = return []
llToL (Node _ nxt a) = do
  nxtLL <- getLL nxt
  rest <- llToL nxtLL
  return (a:rest)

lToLL [] = Empty
lToLL (x:xs) = Node (newSTRef Empty) (newSTRef (lToLL xs)) x

attachToPrev node Empty = return ()
attachToPrev node prv = do
  ref <- prev node
  writeSTRef ref prv

--appendTo ll [] = 
--appendTo ll (x:xs) = 

newEmptyNode v = Node (newSTRef Empty) (newSTRef Empty) v

appendReturn ll v = do
  nxt <- next ll
  writeSTRef nxt (newEmptyNode v)
  dl <- readSTRef nxt
  return $ element dl

appendToList ll v = do
  nxt <- next ll
  writeSTRef nxt (newEmptyNode v)

--lookAtNext :: LinkedList Integer -> (forall s. ST s Integer)
lookAtNext ll = do
  r <- next ll
  v <- readSTRef r
  case v of
    Empty -> return 0
    Node _ _ n -> return n

stuff = let ll = newEmptyNode 4
        in do
            appendToList ll 17
            lookAtNext ll

--main = print $ runST $ lookAt (newEmptyNode 4)


--main = print $ element (newEmptyNode 4)
