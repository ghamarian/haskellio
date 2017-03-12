{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee
import Data.Monoid
import Data.Tree
import Test.QuickCheck

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

{-instance Arbitrary a => Arbitrary (Tree a) where-}
   {-arbitrary = sized arbTree-}
   
{-arbTree :: Arbitrary a => Int -> Gen (Tree a)-}
{-arbTree 0 = do-}
         {-a <- arbitrary-}
         {-return (Node a [])-}

{-arbTree n = do-}
      {-v <- arbitrary-}
      {-m <- choose (0, n `div` 2)-}
      {-ts <- vectorOf m (arbTree (n `div` 4))-}
      {-return (Node v ts)-}

instance Monoid GuestList where
        mempty = GL [] 0
        mappend (GL as fa) (GL bs fb) = GL (as ++ bs) (fa + fb)

moreFun :: GuestList -> GuestList -> GuestList
moreFun f@(GL _ a) g@(GL _ b) | a > b = f 
                              | otherwise = g

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node v ts) = f v $ map (treeFold  f) ts

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e es =  (glCons e $ mconcat $ map snd es, mconcat $ map (uncurry moreFun) es) 

