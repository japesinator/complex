module Data.Complex.Mobius

import Control.Algebra
import Data.Complex

data ZStar = Finite (Complex Float)
           | Infinity

instance Num ZStar where
  (Finite a) + (Finite b) = Finite $ a + b
  _          + _          = Infinity

  (Finite a) - (Finite b) = Finite $ a - b
  _          - _          = Infinity

  (Finite a) * (Finite b) = Finite $ a * b
  _          * _          = Infinity

  fromInteger x = Finite $ fromInteger x:+0

  abs Infinity   = Infinity
  abs (Finite a) = Finite $ abs a

(/) : ZStar -> ZStar -> ZStar
Infinity        / _                   = Infinity
_               / Infinity            = Finite (0:+0)
_               / (Finite (0.0:+0.0)) = Infinity
(Finite (a:+b)) / (Finite (c:+d))     = Finite $
  ((a * c + b * d) / (c * c + d * d)) :+ ((b * c - a * d) / (c * c + d * d))

data MT : Type where
  MkMT : (a : ZStar) -> (b : ZStar) -> (c : ZStar) -> (d : ZStar) -> MT

apply : MT -> ZStar -> ZStar
apply (MkMT a b c d) z = (a * z + b) / (c * z + d)

compose : MT -> MT -> MT
compose (MkMT a b c d) (MkMT a' b' c' d') =
  MkMT (a' * a + b' * c) (a' * b + b' * d) (c' * a + d' * c) (c' * b + d' * d)

instance Semigroup MT where
  (<+>) = compose

instance Monoid MT where
  neutral = MkMT (Finite $ 1:+0) (Finite $ 0:+0) (Finite $ 0:+0) (Finite $ 0:+1)

instance Group MT where
  inverse (MkMT a b c d) = MkMT d ((Finite $ -1:+0) * b) ((Finite $ -1:+0) * c) a

determinant : MT -> ZStar
determinant (MkMT a b c d) = a * d - b * c

trace : MT -> ZStar
trace (MkMT a _ _ d) = a + d

normalize : MT -> MT
normalize m@(MkMT a b c d) = MkMT (a/determinant m) (b/determinant m)
                                  (c/determinant m) (d/determinant m)
