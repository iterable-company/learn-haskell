module CustomData.Functor
where

import CustomData.Tree

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x left right) =
        Node (f x) (fmap f left) (fmap f right)

instance Functor (Map k v) where
    fmap f (Map k v) = Map k (f v)