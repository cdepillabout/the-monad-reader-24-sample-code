{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib
    ( someFunc
    ) where

import GHC.Exts (Constraint)

type Name = String

data User = U { username :: String
              , isRegistered :: Bool
              } deriving (Eq)

data Post = P { isPublic :: Bool
              , canCommentAnonymously :: Bool
              , author :: User
              } deriving (Eq)

data Pred (c :: * -> Constraint) where
    Leaf :: Name -> (forall a . c a => a -> Bool) -> Pred c
    And :: Pred c1 -> Pred c2 -> Pred (c1 `CC` c2)
    Or :: Pred c1 -> Pred c2 -> Pred (c1 `CC` c2)
    Not :: Pred c1 -> Pred c1

-- class (c1 a, c2 a) => CombineConstraint c1 c2 a
-- instance (c1 a, c2 a) => CombineConstraint c1 c2 a

type CC c1 c2 a = CombineConstraint c1 c2 a

type family CombineConstraint (c1 :: * -> Constraint) (c2 :: * -> Constraint) (a :: *) :: Constraint where
    CombineConstraint c1 c2 a = (c1 a, c2 a)

class Get r a where
    get :: a -> r

userIsRegistered :: Pred (Get User)
userIsRegistered = Leaf "userIsRegistered" (isRegistered . get)

-- userIsAuthor :: Pred (Get Post `CombineConstraint` Get User)
-- userIsAuthor = Leaf "userIsAuthor" (\x -> author (get x) == get x)

-- userCanEditPost :: Pred _
-- userCanEditPost = userIsAuthor `And` userIsRegistered

someFunc :: IO ()
someFunc = putStrLn "someFunc"
