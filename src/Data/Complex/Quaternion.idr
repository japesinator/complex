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

fromReal : Float -> Quaternion
fromReal = flip (::) [0,0,0]

toMatrix : Quaternion -> Vect 2 (Vect 2 (Complex Float))
toMatrix [a,b,c,d] = [[a :+ b, c :+ d], [-c :+ d, a :+ -b]]

complement : Quaternion -> Quaternion
complement [a,b,c,d] = [a,-b,-c,-d]

length : Quaternion -> Float
length = sqrt . with Foldable sum . map (flip pow 2)

normalize : Quaternion -> Quaternion
normalize q = map (flip (/) $ length q) q

act : Quaternion -> Vect 3 Float -> Vect 3 Float
act q [x,y,z] = let [a,b,c,d] = normalize q in
  [ x * (pow a 2 + pow b 2 - pow c 2 - pow d 2)
  + y * (2 * (b * c - a * d))
  + z * (2 * (b * d + a * c))
  ,
    x * (2 * (b * c + a * d))
  + y * (pow a 2 - pow b 2 + pow c 2 - pow d 2)
  + z * (2 * (c * d - a * b))
  ,
    x * (2 * (b * d - a * c))
  + y * (2 * (c * d + a * b))
  + z * (pow a 2 - pow b 2 - pow c 2 + pow d 2)
  ]

instance Num Quaternion where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) [a1,b1,c1,d1] [a2,b2,c2,d2] = [ a1 * a2 - b1 * b2 - c1 * c2 - d1 * d2
                                    , a1 * b2 + b1 * a2 + c1 * d2 - d1 * c2
                                    , a1 * c2 - b1 * d2 + c1 * a2 + d1 * b2
                                    , a1 * d2 + b1 * c2 - c1 * b2 + d1 * a2
                                    ]

  fromInteger = fromReal . fromInteger
  abs         = fromReal . length

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

-- Note: Quaternions are not a field proper, as multiplication on quaternions
-- is non-commutative
instance Field Quaternion where
  inverseM q = const $ map ( flip (/)
                           $ with Foldable sum
                           $ map (flip pow 2) q
                           ) $ complement q
