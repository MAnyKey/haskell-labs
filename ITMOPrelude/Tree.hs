{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPrelude.Tree where

import Prelude (Show,Read,error, show)
import ITMOPrelude.Primitive
import ITMOPrelude.Category

data Tree a =
  Node a (Tree a) (Tree a) |
  Leaf
  deriving (Show, Read)

instance Functor Tree where
  fmap = tmap

emptyTree = Leaf
addToRoot x t = Node x t Leaf

addToLeftLeaf x (Node a l r) = Node a (addToLeftLeaf x l) r
addToLeftLeaf x Leaf = Node x Leaf Leaf

addToRightLeaf x (Node a l r) = Node a l (addToRightLeaf x r)
addToRightLeaf x Leaf = Node x Leaf Leaf

leftRotate (Node x l (Node y l' r')) = Node y (Node x l l') r'
leftRotate n = error "leftRotate: Incorrect tree"

rightRotate (Node x (Node y l' r') r) = Node y l' (Node x r' r)
rightRotate n = error "rightRotate: Incorrect tree"

tmap :: (a -> b) -> Tree a -> Tree b
tmap f Leaf = Leaf
tmap f (Node x l r) = Node (f x) (tmap f l) (tmap f r)

tfold :: (a -> b -> b) -> b -> Tree a -> b
tfold f z Leaf = z
tfold f z (Node x l r) = f x (tfold f (tfold f z r) l)

