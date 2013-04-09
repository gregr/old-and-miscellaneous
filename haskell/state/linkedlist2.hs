module Main where

import Control.Monad.ST
import Data.STRef

type NodeRef s a = STRef s (LinkedList s a)

data LinkedList s a = Node {prev :: (NodeRef s a),
                            next :: (NodeRef s a),
                            element :: a}
                    | Empty

--newEmptyNode :: a -> ST s (LinkedList s a)
newEmptyNode v = do
  p <- newSTRef Empty
  n <- newSTRef Empty
  return (Node p n v)


setPrev Empty p = return ()
setPrev ll p = writeSTRef (prev ll) p

appendToList ll v = let nxt = next ll
                    in do
                        newEmpty <- (newEmptyNode v)
                        writeSTRef nxt newEmpty
                        setPrev newEmpty ll


lookAtNext ll = do
  v <- readSTRef $ next ll
  case v of
    Empty -> return 0
    Node _ _ n -> return n


stuff = do
  ll <- newEmptyNode 4
  appendToList ll 17
  lookAtNext ll


newNode nxt v = do
  p <- newSTRef Empty
  n <- newSTRef nxt
  return (Node p n v)


listToLL [] = return Empty
listToLL (x:xs) = do
  nxt <- listToLL xs
  this <- newNode nxt x
  setPrev nxt this
  return this

getNext ll = readSTRef $ next ll

examine ll = return $ element ll

--llToList