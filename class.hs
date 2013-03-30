class (Eq a) => Comparable a where
  compare :: a -> a -> Ordering

class (Comparable a) => Equal a where
  isEq :: a -> a -> Bool

data Color = Red | Blue | Green deriving (Eq)

instance Comparable Color where
  compare Red Red = EQ
  compare Red _ = LT
  compare Blue Blue = EQ
  compare Blue Red = GT
  compare Blue Green = LT
  compare Green Green = EQ
  compare Green _ = GT

instance Equal Color where
  isEq Red Red = True
  isEq Blue Blue = True
  isEq Green Green = True
  isEq _ _ = False
