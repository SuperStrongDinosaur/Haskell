{-# LANGUAGE RankNTypes #-}

module Iso where 

import Data.Profunctor
import Data.Tagged
import Data.Functor.Const
import Data.Tree
import FileSystemLenses

type Iso b a = forall p f. (Profunctor p, Functor f) => p a (f a) -> p b (f b)

iso :: (b -> a) -> (a -> b) -> Iso b a
iso a b = dimap a (fmap b)

from :: Iso b a -> Iso a b
from x =
  let a = unTagged $ x (Tagged id)
      b = getConst . x Const
  in iso a b

fsIso :: Iso FS (Tree FilePath)
fsIso = iso fsToTree treeToFs where
  treeToFs :: Tree FilePath -> FS
  treeToFs (Node x []) = File x
  treeToFs (Node x c) = Dir x $ map treeToFs c

  fsToTree :: FS -> Tree FilePath
  fsToTree (File x) = Node x []
  fsToTree (Dir x c) = Node x $ map fsToTree c
