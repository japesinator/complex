module Data.Complex.Quaternion

import Data.Complex
import Data.Floats
import Data.Vect

Quaternion : Type
Quaternion = Vect 4 Float

realPart : Quaternion -> Float
realPart = head

imagPart : Quaternion -> Float
imagPart = head . tail

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
