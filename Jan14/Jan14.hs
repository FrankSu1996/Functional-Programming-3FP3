-- Learning Objectives:
-- fold (generalize in later lectures)
module Jan14 where
    -- Folding right and left.
    -- Context: a list, a binary function 'f', and a starting value 's'
    -- l = [a1, a2, a3, ... , an]
    -- fold-right: a1 'f' (a2 'f' (a3 'f'... (an 'f' startVal)))
    -- fold-left: ((((s 'f' a1) 'f' a2) 'f' a3) 'f' an)
    -- SUPER IMPORTANT: only thing you can do with a functional is call it!!!!
     foldRight :: (a -> b -> b) -> b -> [a] -> b
     foldRight f start [] = start
     foldRight f start (x:xs) = x `f` foldRight f start xs