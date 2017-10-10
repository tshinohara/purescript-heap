module Data.Heap
  ( Heap
  , Min, Max
  , class HeapOrder
  , HTProxy
  , prefer
  , empty
  , singleton
  , null
  , merge
  , insert
  , root
  , min
  , max
  , deleteRoot
  , deleteMin
  , deleteMax
  ) where

import Prelude

import Data.Maybe (Maybe(..))

foreign import kind HeapType
foreign import data Min :: HeapType
foreign import data Max :: HeapType

data HTProxy (t :: HeapType) = HTProxy

class HeapOrder (t :: HeapType) where
  prefer :: forall a. Ord a => HTProxy t -> a -> a -> Boolean

instance heapOrderMin :: HeapOrder Min where
  prefer _ a b = a < b
instance heapOrderMax :: HeapOrder Max where
  prefer _ a b = a > b

data Heap (t :: HeapType) a
  = Empty
  | Node { elem :: a, rank :: Int, left :: Heap t a, right :: Heap t a }

empty :: forall t a. Ord a => Heap t a
empty = Empty

null :: forall t a. Heap t a -> Boolean
null Empty = true
null _ = false

singleton :: forall t a. Ord a => a -> Heap t a
singleton a =
  Node { elem: a, rank: 1, left: Empty, right: Empty }

merge :: forall t a. HeapOrder t => Ord a => Heap t a -> Heap t a -> Heap t a
merge Empty h = h
merge h Empty = h
merge a'@(Node a) b'@(Node b) =
  if prefer (HTProxy :: HTProxy t) a.elem b.elem
     then node' a.elem a.left (merge a.right b')
     else node' b.elem b.left (merge b.right a')
  where
    rank Empty = 0
    rank (Node {rank}) = rank

    node' root n0 n1 =
      let
        rank0 = rank n0
        rank1 = rank n1
      in
        if rank0 < rank1
           then Node { elem: root, rank: rank0+1, left: n1, right: n0 }
           else Node { elem: root, rank: rank1+1, left: n0, right: n1 }

insert :: forall t a. HeapOrder t => Ord a => a -> Heap t a -> Heap t a
insert a = merge (singleton a)

root :: forall t a. Heap t a -> Maybe a
root Empty = Nothing
root (Node {elem}) = Just elem

min :: forall a. Heap Min a -> Maybe a
min = root

max :: forall a. Heap Max a -> Maybe a
max = root

deleteRoot :: forall t a. HeapOrder t => Ord a => Heap t a -> Heap t a
deleteRoot Empty = Empty
deleteRoot (Node {left,right}) = merge left right

deleteMin :: forall a. Ord a => Heap Min a -> Heap Min a
deleteMin = deleteRoot

deleteMax :: forall a. Ord a => Heap Max a -> Heap Max a
deleteMax = deleteRoot
