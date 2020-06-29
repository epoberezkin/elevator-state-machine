{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Werror=incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Elevator where

import Data.Kind
import Data.Singletons
import Data.Singletons.Decide
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits

$( singletons
     [d|
       data DoorState = Opened | Closed
         deriving (Show, Read, Eq)

       data MoveState = Stopped | Up | Down
         deriving (Show, Read, Eq)
       |]
 )

$( singletonsOnly
     [d|
       nextFloor :: MoveState -> Nat -> Nat
       nextFloor Stopped f = f
       nextFloor Up f = f + 1
       nextFloor Down f =
         if f > 0
           then f - 1
           else 0

       nextMoveState :: MoveState -> Nat -> MoveState
       nextMoveState Stopped _ = Stopped
       nextMoveState Up _ = Up
       nextMoveState Down f =
         if f <= 1
           then Stopped
           else Down
       |]
 )

infixr 2 :>>

type Elevator = (DoorState, MoveState, Nat)

type family Moving (m :: MoveState) :: Constraint where
  Moving Up = ()
  Moving Down = ()

data Action (s :: Elevator) (s' :: Elevator) :: Type where
  Open ::
    Action
      '(Closed, Stopped, f)
      '(Opened, Stopped, f)
  Close ::
    Action
      '(Opened, Stopped, f)
      '(Closed, Stopped, f)
  Move ::
    (Moving m, (m == Down && f == 0) ~ False) => -- changed for Problems.hs #4
    Sing (m :: MoveState) ->
    Action
      '(Closed, Stopped, f)
      '(Closed, NextMoveState m f, NextFloor m f)
  Stop ::
    Action
      '(Closed, m, f)
      '(Closed, Stopped, f)
  Wait ::
    Action
      '(d, m, f)
      '(d, NextMoveState m f, NextFloor m f)
  (:>>) :: Action s1 s2 -> Action s2 s3 -> Action s1 s3

deriving instance Show (Action s1 s2)

type ElevatorProgram f f' =
  Action '(Opened, Stopped, f) '(Opened, Stopped, f')

program :: ElevatorProgram 0 0
program =
  Close
    :>> Move SUp
    :>> Wait
    :>> Wait
    :>> Stop
    :>> Open
    :>> Close
    :>> Move SDown
    :>> Wait
    :>> Wait
    :>> Stop
    :>> Open

-- badElevator :: ElevatorProgram 0 1
-- badElevator =
--   Close
--     :>> Move SUp
--     -- :>> Stop
--     :>> Open

printElevator :: Action s s' -> IO ()
printElevator Open = putStrLn "open"
printElevator Close = putStrLn "close"
printElevator (Move m) = case m of
  SUp -> putStrLn "up"
  _ -> putStrLn "down"
printElevator Stop = putStrLn "stop"
printElevator Wait = putStrLn "wait"
printElevator (a :>> prog) = printElevator a >> printElevator prog

instance {-# OVERLAPS #-} Show (SomeSing Elevator) where
  show (SomeSing s) = show (fromSing s)

data SomeAction where
  SomeAction :: Action s s' -> Sing s -> Sing s' -> SomeAction

deriving instance Show SomeAction

finalState :: SomeSing Elevator -> SomeAction -> Maybe (SomeSing Elevator)
finalState (SomeSing s1) (SomeAction _ s1' s2) =
  decideEquality s1 s1' >> Just (SomeSing s2)

actionFromString :: String -> SomeSing Elevator -> Maybe SomeAction
actionFromString name (SomeSing st) = action name st
  where
    act :: Action s s' -> Sing s -> Sing s' -> Maybe SomeAction
    act a s1 s2 = Just $ SomeAction a s1 s2
    action :: String -> Sing (s :: Elevator) -> Maybe SomeAction
    action "open" s@(STuple3 SClosed SStopped f) =
      act Open s (STuple3 SOpened SStopped f)
    action "close" s@(STuple3 SOpened SStopped f) =
      act Close s (STuple3 SClosed SStopped f)
    action "up" s@(STuple3 SClosed SStopped f) =
      act (Move SUp) s (STuple3 SClosed SUp (sNextFloor SUp f))
    action "down" s@(STuple3 SClosed SStopped f) =
      case f %== SNat @0 of -- changed for Problems.hs #4
        SFalse -> act (Move SDown) s (STuple3 SClosed (sNextMoveState SDown f) (sNextFloor SDown f))
        STrue -> Nothing
    action "stop" s@(STuple3 SClosed _ f) =
      act Stop s (STuple3 SClosed SStopped f)
    action "wait" s@(STuple3 d m f) =
      act Wait s (STuple3 d (sNextMoveState m f) (sNextFloor m f))
    action _ _ = Nothing
