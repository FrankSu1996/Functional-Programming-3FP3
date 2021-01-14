-- Example code for Jan12
module Jan12 where

    -- let's play with lists a little
    -- recall
    -- data [] a = [] | a : [a]
    map' :: (a -> b) -> [a] -> [b]
    map' f [] = []
    map' f (x : xs) = f x : map' f xs

    filter' :: (a -> Bool) -> [a] -> [a]
    filter' p [] = []
    filter' p (x : xs) | p x = x : filter' p xs | otherwise = filter' p xs

    isEven :: Integer -> Bool
    isEven x = x `mod` 2 == 0

    -- if we want to print something
    main :: IO ()
    main = print "Hellworld