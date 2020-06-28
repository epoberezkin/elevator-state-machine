{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Problems where

import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Elevator
import GHC.Natural

infixl 2 >>:

class ActionSequence a1 a2 a3 | a1 a2 -> a3 where
  (>>:) :: a1 -> a2 -> a3

instance ActionSequence (Action s1 s2) (Action s2 s3) (Action s1 s3) where
  Skip >>: a = a
  a >>: Skip = a
  a1 >>: a2 = a1 :>> a2

instance ActionSequence SomeAction SomeAction (Maybe SomeAction) where
  (SomeAction a1 s1 s2) >>: (SomeAction a2 s2' s3)
    | Proved Refl <- s2 %~ s2' = Just $ SomeAction (a1 >>: a2) s1 s3
    | otherwise = Nothing

instance (SingI s1, SingI s2) => ActionSequence (Action s1 s2) SomeAction (Maybe SomeAction) where
  (>>:) a = (>>:) $ SomeAction a (sing @s1) (sing @s2)

instance Show (Action s s') where
  show Open = "open"
  show Close = "close"
  show Up = "up"
  show Down = "down"
  show Stop = "stop"
  show Wait = "wait"
  show Skip = ""
  show (a :>> prog) = show a ++ " " ++ show prog

instance Show SomeAction where
  show (SomeAction a s s') =
    show a ++ ": "
      ++ show (fromSing s)
      ++ "->"
      ++ show (fromSing s')

elevatorToFloor :: SomeSing Elevator -> Natural -> Maybe SomeAction
elevatorToFloor (SomeSing st) (FromSing f') = program st f'
  where
    act a s1 s2 = Just $ SomeAction a s1 s2
    program :: forall s f'. Sing (s :: Elevator) -> Sing (f' :: Nat) -> Maybe SomeAction
    program s@(STuple3 SOpened SStopped f) f'
      | Proved Refl <- f %~ f' =
        act Skip s s
      | otherwise = Nothing --move Close s f f'
    program s@(STuple3 SClosed SStopped f) f'
      | Proved Refl <- f %~ f' =
        act Open s (STuple3 SOpened SStopped f)
      | otherwise = Nothing -- move Skip s f f'
    program _ _ = Nothing
-- move :: Action s s' -> Sing (s :: Elevator) -> Sing (f :: Nat) -> Sing (f' :: Nat) -> Maybe SomeAction
-- move a s f f' =
--   act (a >>: upOrDown) s s' ?:>>? program s' f'
--   where
--     (upOrDown, s')
--       | Proved Refl <- (f' %> f) %~ STrue =
--         (Up, STuple3 SClosed SGoingUp (sNextFloor SGoingUp f))
--       | otherwise =
--         (Down, STuple3 SClosed SGoingDown (sNextFloor SGoingDown f))
