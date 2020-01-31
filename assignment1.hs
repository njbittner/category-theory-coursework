
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

-- PROBLEM 9
class Category obj mor | mor -> obj where
    dom :: mor -> obj
    cod :: mor -> obj
    idy :: obj -> mor
    cmp :: mor -> mor -> Maybe mor


data Object = Obj1 | Obj2
data Morphism = Id1 | Id2 | M12

-- Associate Identities
identity :: Object -> Morphism
identity Obj1 = Id1
identity Obj2 = Id2

-- Associate Morphism Starting Points
domain :: Morphism -> Object
domain Id1 = Obj1
domain Id2 = Obj2
domain M12 = Obj1

-- Associate Morphism Ending Points
codomain :: Morphism -> Object
codomain Id1 = Obj1
codomain Id2 = Obj2
codomain M12 = Obj2

-- Define Composition Table
-- "maybe" because the morphism may go to Nothin
composition :: Morphism -> Morphism -> Maybe Morphism
composition Id1 Id1 = Just Id1
composition Id1 M12 = Just M12
composition Id1 Id2 = Nothing
composition M12 Id1 = Nothing
composition M12 M12 = Nothing
composition M12 Id2 = Just M12
composition Id2 Id1 = Nothing
composition Id2 M12 = Nothing
composition Id2 Id2 = Just Id2


instance Category Object Morphism where
    dom = domain
    cod = codomain
    idy = identity
    cmp = composition



--- PROBLEM 1
f :: Int -> Int
f x = x^2

g :: Int -> Int
g x = x+1

main = putStrLn ((show (f (g 2))) ++ "\n" ++ (show (g (f 2))))