module A1 where

    import Prelude hiding (sum, product, elem, zip, reverse)

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
    zip :: [a] -> [b] -> [(a,b)]
    zip _ [] = []
    zip [] _ = []
    zip (x:xs) (y:ys) = (x,y) : zip xs ys

    -- pos type signature:
    pos :: (Eq a) => a -> [a] -> [Integer]
    -- we can create a list of tuples from the original list, associating each value with it's index
    -- in the list. Then we use list comprehension to filter
    pos _ [] = []
    pos y xs = [i | (x, i) <- zip xs [0..], y == x]

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
    -- data type definition
    data OrBoth a b = One a | Other b | Both a b
    --consume1 type definition
    consume1 :: (a -> c) -> (b -> c) -> (a -> b -> c) -> OrBoth a b -> c
    -- consuming type One x will apply the first function in the argument list
    consume1 f _ _ (One x) = f x
    -- consuming type Other y will apply the second function in the argument list
    consume1 _ g _ (Other y) = g y
    -- consuming type Both x y will apply the third function the in argument list to both x and y
    consume1 _ _ g (Both x y) = g x y

    -- consume2 type definition
    consume2 :: (a -> c) -> (b -> c) -> (c -> c -> c) -> OrBoth a b -> c
    -- consuming type One x will again apply the first function
    consume2 f _ _ (One x) = f x
    -- consuming type Other y will again apply the second function
    consume2 _ g _ (Other y) = g y
    -- 'both' case: consuming type Both x y will apply the third function to the the output produced by 
    -- f x and g y, thereby consuming all of the arguments
    consume2 f g h (Both x y) = h (f x) (g y)

    --QUESTION5
    -- Ternary tree data type definition
    data Ternary a = TLeaf a | TNode  (Ternary a) (Ternary a) (Ternary a) deriving (Show)

    -- mirror type definition
    mirror :: Ternary a -> Ternary a
    -- base case: mirror of a leaf node is just the leaf node
    mirror (TLeaf a) =  TLeaf a
    -- recursive case: to mirror a node in the tree, we keep its middle child the same, while recursing with switched left and right childs
    mirror (TNode left middle right) = TNode (mirror right) (mirror middle) (mirror left)

    -- flattenTernary type definition
    flattenTernary :: Ternary a -> [a]
    -- base case: flattening a leaf node produces a list with the single node
    flattenTernary (TLeaf a) = [a]
    -- recursive case: flattening a ternary tree; we can concatenate the lists produced by recursively flattening the
    -- left subtree, then the middle subtree, then the right subtree
    flattenTernary (TNode left middle right) = flattenTernary left ++ flattenTernary middle ++ flattenTernary right

    -- QUESTION 6
    -- DEFINITIONS
    -- Range split definition: all p (xs ++ ys) = all p xs && all p ys
    -- Universal quanitification definition:
    -- all :: (a -> Bool) -> [a] -> Bool
    -- all p [] = True
    -- all p (x:xs) = p x && all p xs

    -- PROOF BY STRUCTURAL INDUCTION
    -- Base case: xs = []b
    -- all p ([] ++ ys) = all p [] && all p ys                         1. By Range split definition
    -- all p ([] ++ ys) = true && all p ys                             2. By Universal quantification definition
    -- all p ([] ++ ys) = all p ys                                     3. By Idendity of &&
    -- all p ys = all p ys                                             4. By Idendity of ++,
    -- Thus, proven by equality of lhs and rhs

    -- Inductive Step: assume inductive hypotheses holds: all p (xs ++ ys) = all p xs && all p ys
    -- we must prove:
    -- all p ((x:xs) ++ ys)    = all p (x:xs) && all p ys                 1. By Range split definition
    -- all p (x:(xs ++ ys))    = all p (x:xs) && all p ys                 2. By (++.2), taken from page 144 of textbook
    -- p x && all p (xs ++ ys) = all p (x:xs) && all p ys                 3. By Universal quanitification definition
    -- p x && all p xs && all p ys = p x && all p xs && all p ys          4. By inductive hypothesis
    -- Thus, proven by equality of lhs and rhs

    -- QUESTION 7
    -- mystery type definition
    mystery :: ((a, b) -> c) -> [a] -> [b] -> [c]
    -- the zip function produces an empty list whenever either argument is an empty list
    mystery _ [] _ = []
    mystery _ _ [] = []
    -- recursive case: we apply f to a tuple created with the head of the lists, then concat the output
    -- the recursive call with the tails
    mystery f (x : xs) (y : ys) = let tuple = (x,y) in f tuple : mystery f xs ys
    
    -- QUESTION 8
    -- foldRight function definition, taken from Jan14 lesson code
    foldRight :: (a -> b -> b) -> b -> [a] -> b
    foldRight f start [] = start
    foldRight f start (x:xs) = x `f` foldRight f start xs

    -- helper function: a lambda function that takes an element, a list of elements, and appends the element to the end of the list
    appendToEnd :: a -> [a] -> [a]
    appendToEnd a b = b ++ [a]

    -- to reverse a list, we apply foldRight with our helper function, using an empty list as our accumulator, and the list s 
    -- we wish to reverse
    reverse :: [a] -> [a]
    reverse [] = []
    reverse xs = foldRight appendToEnd [] xs
    

    -- QUESTION 9
    -- binary tree type definition
    data Tree a = Tip | Node (Tree a) a (Tree a) deriving (Show)
    -- mirrorTree code taken from A1 pdf
    mirrorTree :: Tree a -> Tree a
    mirrorTree Tip = Tip
    mirrorTree (Node l a r) = Node (mirrorTree r) a (mirrorTree l)

    -- pre type definition
    pre :: Tree a -> [a]
    -- base case: preorder traversal of a tip yields an empty list
    pre Tip = []
    -- recursive case: preorder traveral of a binary tree; first collect value stored at the root, then
    -- collect the values at the left subtree, then collect values at the right subtree
    pre (Node l a r) = [a] ++ pre l ++ pre r
    
    --post type definition
    post :: Tree a -> [a]
    -- base case: postorder traversal of a tip yields an empty list
    post Tip = []
    -- recursive case: postorder traveral of a binary tree; first recursively collect values stored in left subtree, then
    -- recursively collect the values in the right subtree, then collect value at the root node
    post (Node l a r) = post l ++ post r ++ [a]

    -- Prove P(t) = pre (mirrorTree t) = reverse (post t)
    -- base case: t = Tip
    -- pre (mirrorTree Tip) = reverse (post Tip)
    -- pre (Tip) = reverse ([])                                     1. definition of mirrorTree, definition of post
    -- [] = []                                                      1. definition of pre, reverse of an empty list is empty list
    -- Thus, proven by equality of lhs and rhs

    -- Inductive step: Assume P(t) holds for trees T1 and T2. Thus, our inductive hypothesis are:
    -- pre (mirrorTree T1) = reverse (post T1), pre (mirrorTree T2) = reverse (post T2)
    -- then we can make a new tree T3 with root value x as: Node (T1) x (T2), we now prove that P(t) holds for T3
    -- pre (mirrorTree (Node T1 x T2))                           = reverse (post (Node T1 x T2))                            1. P(T3)
    -- pre (Node (mirrorTree T2) x (mirrorTree T1))              = reverse (post T1 ++ post T2 ++ x)                        2. apply definition of mirrorTree to lhs, definition of post to rhs
    -- [x] ++ pre (mirrorTree T2) ++ (pre mirrorTree T1)         = reverse (post T1 ++ postT2 ++ x)                         3. apply definition of pre to lhs
    -- [x] ++ reverse (post T2) ++ reverse (post T1)             = reverse (post T1 ++ postT2 ++ x)                         4. apply induction hypothesis to lhs
    -- [x] ++ reverse (post T2) ++ reverse (post T1)             = reverse [x] ++ reverse (post T2) ++ reverse (post T1)    5. expand reverse on the rhs
    -- [x] ++ reverse (post T2) ++ reverse (post T1)             = [x] ++ reverse (post T2) ++ reverse (post T1)            5. reverse of single element list returns the same list
    -- Thus, proven by equality of lhs and rhs

    --QUESTION 10
    -- Rose tree type definition
    data Rose a = Rose a [Rose a] deriving (Show)
    data Fork a = Leaf a | Branch (Fork a) (Fork a) deriving (Show)

    --type definition for to'
    to' :: Tree a -> [Rose a]
    -- base case: an empty tree yields an empty list
    to' Tip = []
    -- case of Leaf node: return a list holding a single rose tree with an empty list as it's children
    to' (Node Tip v Tip) = [Rose v []]
    -- case of tree with a left subtree with no right subtree
    to' (Node left v Tip) = [Rose v (to' left)]
    -- case of tree with right subtree with no left subtree
    to' (Node Tip v right) = [Rose v (to' right)]
    -- case of tree with both left and right subtree: we need to concatenate the lists produced by recursing down both subtrees
    to' (Node left v right) = [Rose v (to' left ++ to' right)]

    --type definition for from'
    from' :: [Rose a] -> Tree a
    from' [] = Tip
    from' [Rose a []] = Node Tip a Tip
    
    -- type definition for to
    to :: Rose a -> Fork a
    to (Rose a []) = Leaf a