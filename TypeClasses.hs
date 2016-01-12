-- Type Classes

import Prelude ()
import HaskellPrelude

-- We know a few of the standard type classes now:
-- Eq for testing == and /=
-- http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#t:Eq
-- Ord for < >
-- http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#t:Ord
-- Num for + - *
-- http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#t:Num
-- Fractional for /
-- http://hackage.haskell.org/package/base-4.8.1.0/docs/Prelude.html#t:Fractional

-- Say we're writing the familiar type Vec2 today
data Vec2 = Vec2 Float Float
  deriving Show

-- We'd certainly like to be able to compare two vectors for equality.
-- Write an instance of Eq for Vec2
-- by replacing undefined in the line below
instance Eq Vec2 where
  Vec2 x1 y1 == Vec2 x2 y2 = (x1-x2) + (y1-y2) == 0

-- We may also want to compare two vectors with < or >
-- Write an instance of Ord for Vec2
instance Ord Vec2 where
  Vec2 x1 y1 < Vec2 x2 y2 = (x1-x2) + (y1-y2) < 0
  Vec2 x1 y1 > Vec2 x2 y2 = (x1-x2) + (y1-y2) > 0

instance Num Vec2 where
  Vec2 x1 y1 + Vec2 x2 y2 = Vec2 (x1+x2)(y1+y2)

  Vec2 x1 y1 * Vec2 x2 y2 = Vec2 (x1*x2) (y1*y2)

  Vec2 x1 y1 - Vec2 x2 y2 = Vec2 (x1-x2) (y1-y2)
-- There's at least one other way to write Ord.
-- Describe it in a sentence or two, in a code comment.

-- You could compare the portions of the vector individually (the x&y).

-- Write the most general type you can for each expression
a :: Num a => a
a = 2+3
b :: Fractional b => b
b = (7 + 11) / 4

c :: Float
c = 18.7

d :: Floating d=>d
d = sqrt 2

-- Another familiar type is Maybe
-- I'll call our version here Option
data Option a = None | Actual a

-- We'd like to be able to compare two Options.
-- This only makes sense if they are actually the same type of Option
-- We can compare an Option Number to an Option Number
-- or Option String to Option String, but never Option Number to Option String.
-- It also only makes sense if the type `a` is itself a type we can compare.

-- Fill out the definition of Eq for Option
instance Eq a => Eq (Option a) where
  Actual a == Actual b = b == a
  Actual a == None = True

instance Ord a=> Ord (Option a) where
  Actual a > Actual b = a > b
  Actual a < Actual b = a < b
  Actual a < None = False
  None < Actual a = True
  None < None = True

instance Num a=> Num (Option a) where
 Actual a + Actual b = Actual (a+b)
 None + Actual a = Actual a

 Actual a * Actual b = Actual (a*b)
 Actual a * None = Actual a

 Actual a - Actual b = Actual (a-b)
 Actual a - None = Actual a

-- Write an instance of Ord for Option
-- Like Eq, it will depend on the type `a` being comparable.

-- Write an instance Num for Option, following the same pattern

-- Write Num for Vec2.

main = return ()
