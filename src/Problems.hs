{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Problems where

import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Elevator
import GHC.Natural

-- Problem #1

infixr 2 >>:

class ActionSequence a1 a2 where
  (>>:) :: a1 -> a2 -> Maybe SomeAction

instance ActionSequence SomeAction SomeAction where
  (SomeAction a1 s1 s2) >>: (SomeAction a2 s2' s3) =
    case s2 %~ s2' of
      Proved Refl -> Just $ SomeAction (a1 :>> a2) s1 s3
      _ -> Nothing

-- Problem #2

elevatorProgram :: SomeSing Elevator -> [String] -> Maybe SomeAction
elevatorProgram _ [] = Nothing
elevatorProgram st [x] = actionFromString x st
elevatorProgram st (x : xs) = do
  a <- actionFromString x st
  st' <- finalState st a
  as <- elevatorProgram st' xs
  a >>: as

-- Problem #3
-- see Elevator.hs

-- Problem #4
-- also see Main.hs - action "floor n" is added to REPL
instance ActionSequence (Maybe SomeAction) (Maybe SomeAction) where
  Nothing >>: _ = Nothing
  _ >>: Nothing = Nothing
  Just a1 >>: Just a2 = a1 >>: a2

elevatorToFloor :: SomeSing Elevator -> Natural -> Maybe SomeAction
elevatorToFloor (SomeSing st) (FromSing flr) = actions st flr
  where
    act :: Action s s' -> Sing s -> Sing s' -> Maybe SomeAction
    act a s1 s2 = Just $ SomeAction a s1 s2
    actions :: forall s f'. Sing (s :: Elevator) -> SNat f' -> Maybe SomeAction
    actions s@(STuple3 SOpened SStopped f) f' =
      case sCompare f' f of
        SEQ -> Nothing
        SGT ->
          act (Close :>> Move SUp) s (state' SUp f)
            >>: wait SUp f f'
        SLT -> case f %== SNat @0 of -- this check is required to satisfy the type restriction added in Problem #1
          SFalse ->
            act (Close :>> Move SDown) s (state' SDown f)
              >>: wait SDown f f'
          STrue -> Nothing
    actions _ _ = Nothing
    wait :: SMoveState m -> SNat f -> SNat f' -> Maybe SomeAction
    wait m f f' = wait_ (sNextMoveState m f) (sNextFloor m f) f'
    wait_ :: SMoveState m -> SNat f -> SNat f' -> Maybe SomeAction
    wait_ SStopped f _ =
      act Open (STuple3 SClosed SStopped f) (STuple3 SOpened SStopped f)
    wait_ m f f' = case f %~ f' of
      Proved Refl ->
        act (Stop :>> Open) (STuple3 SClosed m f) (STuple3 SOpened SStopped f)
      _ ->
        act Wait (STuple3 SClosed m f) (state' m f)
          >>: wait m f f'
    state' :: SMoveState m -> SNat f -> STuple3 '(Closed, NextMoveState m f, NextFloor m f)
    state' m f = STuple3 SClosed (sNextMoveState m f) (sNextFloor m f)
