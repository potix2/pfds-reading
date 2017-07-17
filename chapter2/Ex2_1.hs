module EX2_1 (suffixes) where
    suffixes [] = [[]]
    suffixes l@(x:xs) = l : suffixes xs
