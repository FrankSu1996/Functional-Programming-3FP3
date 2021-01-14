module A1 where
    import Prelude hiding (sum, product, elem)

    -- QUESTION 1
    -- matches type signature: 
    matches :: (Eq a) => a -> [a] -> [a]
    -- base case: trying to match anything with empty list returns an empty list
    matches _ [] = []
    -- recursive definition: if first argument matches head of list: append head to the list, and recursively
    -- match the tail of the list
    matches y (x : xs)
        | y == x = x : matches y xs
        | otherwise = matches y xs

    -- elem type signature:
    elem :: (Eq a) => a -> [a] -> Bool
    -- base case: empty list returns false because the argument value can never be in an empty list
    elem _ [] = False
    -- recursive case: if first argument matches the head element, return true, otherwise recursively check
    -- the tail elements
    elem y (x : xs)
        | y == x = True
        | otherwise = elem y xs

    -- this is a helper function for pos. it creates a list of tuples representing an element in a list and it's index
    createIndexedList :: [a] -> [b] -> [(a,b)]
    createIndexedList _ [] = []
    createIndexedList [] _ = []
    createIndexedList (x:xs) (y:ys) = (x,y) : createIndexedList xs ys

    -- pos type signature:
    pos :: (Eq a) => a -> [a] -> [Integer]
    -- we can create a list of tuples from the original list, associating each value with it's index
    -- in the list. Then we use list comprehension to filter. Note 
    pos _ [] = []
    pos y xs = [i | (x, i) <- createIndexedList xs [0..], y == x]

    --QUESTION2
    -- apply all function type signature
    applyAll :: [a -> b] -> [a] -> [b]
    -- if we have no functions to apply, then we get an empty list
    applyAll [] xs = []
    -- if we have no list for the functions to operate on, we also get an empty list
    applyAll _ [] = []
    -- recursive definition: we can use map to apply the first function to the entire list, then
    -- recurse with the tail of the list of function
    applyAll (f : fs) xs = map f xs ++ applyAll fs xs

    --QUESTION3
    -- a)
    -- tripleNeg1 Type signature
    tripleNeg1 :: (Ord a, Num a) => [a] -> [a]
    -- base case: applying function to an empty list returns an empty list
    tripleNeg1 [] = []
    -- recursive case: apply function to head of list, then recurse with the tail of the list
    tripleNeg1 (x : xs)
        | x >= 0 = x : tripleNeg1 xs
        | otherwise = 3 * x : tripleNeg1 xs

    -- b)
    -- f is a function definition we create to be passed into map. Using either, we either apply 3* function, 
    -- or we apply a function that merely returns the element
    f :: (Ord a, Num a) => Either a a -> a
    f = either (*3) (\r -> r)

    -- createEitherArray is a function that takes a list of ordinal numbers, and returns a list of Eithers
    createEitherArray :: (Ord a, Num a) => [a] -> [Either a a]
    createEitherArray [] = []
    createEitherArray (x : xs)
        | x < 0 = Left x : createEitherArray xs
        | otherwise = Right x : createEitherArray xs

    -- tripleNeg2 type signature
    tripleNeg2 :: (Ord a, Num a) => [a] -> [a]
    -- applying function to an empty list returns an empty list
    tripleNeg2 [] = []
    -- we can curry our defined functions f and createEitherArray with map to produce our result
    tripleNeg2 xs = map f (createEitherArray xs)

    --QUESTION4 