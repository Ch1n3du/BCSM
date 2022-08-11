module Stack (
    Stack (..),
    pop,
    popTwo,
    popApply,
    push,
) where

type Stack a = [a]

push :: a -> Stack a -> Stack a
push x xs = x : xs

pop :: Stack a -> Maybe (Stack a, a)
pop xs =
    case xs of
        a : as -> Just (as, a)
        _ -> Nothing

popTwo :: Stack a -> Maybe (Stack a, a, a)
popTwo xs =
    case xs of
        a1 : a2 : as -> Just (as, a1, a2)
        _ -> Nothing

popApply :: (a -> a -> a) -> Stack a -> Maybe (Stack a)
popApply f s =
    case popTwo s of
        Just (xs, a1, a2) -> Just $ (f a1 a2) : xs
        _ -> Nothing
