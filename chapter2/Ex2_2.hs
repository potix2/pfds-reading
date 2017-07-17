module EX2_2 (member) where
import UnbalancedSet hiding(member)

myMember x E memo = x == memo
myMember x (T a y b) memo =
    if x < y then myMember x a memo
    else myMember x b y

member _ E = False
member x (T a y b) =
    if x < y then member x a
    else myMember x b y
