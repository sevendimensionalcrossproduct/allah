module Tree where

import Data.Monoid

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance (Semigroup a, Monoid a) => Semigroup (Tree a) where
  Leaf x <> Leaf y = Leaf (x <> y)
  Leaf x <> Node t1 t2 = Node (Leaf x <> t1) t2
  Node t1 t2 <> Leaf y = Node t1 (t2 <> Leaf y)
  Node t1 t2 <> Node t3 t4 = Node (t1 <> t3) (t2 <> t4)

instance (Semigroup a, Monoid a) => Monoid (Tree a) where
  mempty = Leaf mempty
  mappend = (<>)

main :: IO ()
main =
  let tree1 = Node (Leaf "Hello") (Leaf " ")
      tree2 = Node (Leaf "World") (Leaf "!")
      concatenatedTree = mappend tree1 tree2
  in
  print concatenatedTree
