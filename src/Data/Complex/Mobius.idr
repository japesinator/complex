module Data.Complex.Mobius

import Control.Algebra
import Data.Complex

data ZStar = Finite (Complex Float)
           | Infinity

instance Eq ZStar where
  Infinity   == Infinity   = True
  (Finite a) == (Finite b) = a == b
  _          == _          = False

instance Num ZStar where
  (Finite a) + (Finite b) = Finite $ a + b
  _          + _          = Infinity

  (Finite a) - (Finite b) = Finite $ a - b
  Infinity   - Infinity   = Finite $ 0
  _          - _          = Infinity

  (Finite a) * (Finite b) = Finite $ a * b
  _          * _          = Infinity

  fromInteger x = Finite $ fromInteger x

  abs Infinity   = Infinity
  abs (Finite a) = Finite $ abs a

instance Neg ZStar where
  negate = (*) (Finite $ -1:+0)

(/) : ZStar -> ZStar -> ZStar
Infinity        / Infinity            = Finite $ 1
Infinity        / _                   = Infinity
_               / Infinity            = Finite $ 0
_               / (Finite $ 0.0:+0.0) = Infinity
(Finite $ a:+b) / (Finite $ c:+d)     = Finite $
  ((a * c + b * d) / (c * c + d * d)) :+ ((b * c - a * d) / (c * c + d * d))

data MT : Type where
  MkMT : (a : ZStar) -> (b : ZStar) -> (c : ZStar) -> (d : ZStar) -> MT

apply : MT -> ZStar -> ZStar
apply (MkMT a b c d) z = (a * z + b) / (c * z + d)

instance Semigroup MT where
  (MkMT a b c d) <+> (MkMT a' b' c' d') =
    MkMT (a' * a + b' * c) (a' * b + b' * d) (c' * a + d' * c) (c' * b + d' * d)

instance Monoid MT where
  neutral = MkMT (Finite $ 1) (Finite $ 0) (Finite $ 0) (Finite $ 0:+1)

instance Group MT where
  inverse (MkMT a b c d) = MkMT d (-b) (-c) a

determinant : MT -> ZStar
determinant (MkMT a b c d) = a * d - b * c

valid : MT -> Bool
valid = (/=) (Finite $ 0) . determinant

class VerifiedMT (m : MT) where
  MTValid : valid m = True

trace : MT -> ZStar
trace (MkMT a _ _ d) = a + d

normalize : MT -> MT
normalize m@(MkMT a b c d) = MkMT (a/determinant m) (b/determinant m)
                                  (c/determinant m) (d/determinant m)

instance Eq MT where
  a == b = (\(MkMT a b c d),(MkMT a' b' c' d') =>
    (a,b,c,d) == (a',b',c',d')) (normalize a) (normalize b)

data MTClass = Elliptical
             | Parabolic
             | Hyperbolic
             | Loxodromic

classify : MT -> MTClass
classify m = case trace m of
                  Infinity   =>                               Hyperbolic
                  (Finite t) => if t == conjugate t
                         then ( if realPart (t * t) >  4 then Hyperbolic
                           else if realPart (t * t) == 4 then Parabolic
                                                         else Elliptical
                              )                          else Loxodromic
