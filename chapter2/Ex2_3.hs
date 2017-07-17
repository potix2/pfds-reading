module EX2_3 (insert) where
import UnbalancedSet hiding(insert)

insertCond :: Ord a => a -> UnbalancedSet a -> Maybe (UnbalancedSet a)
insertCond x E = Just (T E x E)
insertCond x s@(T a y b) =
    if x < y then fmap (\left -> (T left y b)) (insertCond x a)
    else if x > y then fmap (\right -> (T a y right)) (insertCond x b)
    else fail "Element is already in set"

insert x s =
    case insertCond x s of
      Nothing -> s
      Just res -> res
