module EX2_5 (complete, create) where
import UnbalancedSet hiding(insert)

-- (a)
complete :: Ord a => a -> Int -> (UnbalancedSet a)
complete x 0 = E
complete x d =
    let st = complete x (d - 1)
    in T st x st

-- (b)
create _ 0 = E
create a 1 = T E a E
create a n =
    if mod n 2 == 1 then
        let st = create a (div (n - 1) 2)
        in T st a st
    else
        let m = (div (n - 1) 2)
            st1 = create a (m + 1)
            st2 = create a m
        in T st1 a st2
