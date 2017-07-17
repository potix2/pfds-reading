module EX2_4 (insert) where
import UnbalancedSet hiding(insert)

insertMemo :: Ord a => a -> UnbalancedSet a -> a -> Maybe (UnbalancedSet a)

insertMemo x E memo =
    if x == memo then Nothing
                 else Just (T E x E)
insertMemo x (T a y b) memo =
    if x < y then fmap (\l -> (T l y b)) (insertMemo x a memo)
             else fmap (\r -> (T a y r)) (insertMemo x b y)

insert' x E = Just (T E x E)
insert' x (T a y b) =
    if x < y then fmap (\l -> (T l y b)) (insert' x a)
             else fmap (\r -> (T a y r)) (insertMemo x b y)

insert x t = case insert' x t of
               Nothing -> t
               Just res -> res
