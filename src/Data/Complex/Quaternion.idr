module Data.Complex.Quaternion

import Control.Algebra
import Data.Complex
import Data.Floats
import Data.Vect

||| A type representing an extension of the complex numbers to four dimensions.
||| [a, b, c, d] (Idris) ⇔ a + bi + cj + dk (Mathematics)
Quaternion : Type
Quaternion = Vect 4 Float

||| Extract the real part of a given Quaternion
realPart : Quaternion -> Float
realPart = head

||| Extract the imaginary part of a given Quaternion
imagPart : Quaternion -> Float
imagPart = head . tail

||| Extract the complex part, of form a + bi, of a given Quaternion
complexPart : Quaternion -> Complex Float
complexPart [a,b,_,_] = a :+ b

instance Num Quaternion where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) [a1,b1,c1,d1] [a2,b2,c2,d2] = [ a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2
                                    , a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2
                                    , a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2
                                    , a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
                                    ]

  fromInteger = flip (::) [0,0,0] . fromInteger
  abs         = flip (::) [0,0,0] . sqrt . foldr1 (+) . map (flip pow 2)

instance Neg Quaternion where
  negate = (*) $ fromInteger $ -1

instance Semigroup Quaternion where
  (<+>) = (+)

instance Monoid Quaternion where
  neutral = 0

instance Group Quaternion where
  inverse = (* -1)

instance AbelianGroup Quaternion

instance Ring Quaternion where
  (<.>) = (*)

instance RingWithUnity Quaternion where
  unity = 1

instance Field Quaternion where
  inverseM [a,b,c,d] = const $ map (flip (/) alpha) [a, -b, -c, -d] where
    alpha : Float
    alpha = (pow a 2) + (pow b 2) + (pow c 2) + (pow d 2)
