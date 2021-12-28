{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Lens where

import Data.Functor.Identity
import Data.Functor.Const
import Control.Arrow

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

view :: Lens s t a b -> s -> a
view l s = getConst (l Const s)

set  :: Lens s t a b -> b -> s -> t
set l b = over l (const b)

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
s ^. l = view l s

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

--_1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 f (x, y) = (, y) <$> f x

--_2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 f (x, y) = (x, ) <$> f y

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens gt st f s = st s <$> f (gt s)

choosing :: Lens s1 t1 a b -> Lens s2 t2 a b -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 = lens (either (view l1) (view l2)) (either (\s b -> Left (set l1 b s)) (\s b -> Right (set l2 b s)))

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f = l (f &&& f)

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f = l (id &&& f)