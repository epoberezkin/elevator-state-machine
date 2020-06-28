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
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits

$( singletons
     [d|
       data DoorState = Opened | Closed
         deriving (Show, Read, Eq)

       data MoveState = Stopped | GoingUp | GoingDown
         deriving (Show, Read, Eq)
       |]
 )

$( singletonsOnly
     [d|
       nextFloor :: MoveState -> Nat -> Nat
       nextFloor Stopped f = f
       nextFloor GoingUp f = f + 1
       nextFloor GoingDown f =
         if f > 0
           then f - 1
           else 0

       nextMoveState :: MoveState -> Nat -> MoveState
       nextMoveState Stopped _ = Stopped
       nextMoveState GoingUp _ = GoingUp
       nextMoveState GoingDown f =
         if f == 0
           then Stopped
           else GoingDown
       |]
 )

infixr 5 :>>

type Elevator = (DoorState, MoveState, Nat)

data Action (s :: Elevator) (s' :: Elevator) :: Type where
  Open :: Action '(Closed, Stopped, f) '(Opened, Stopped, f)
  Close :: Action '(Opened, Stopped, f) '(Closed, Stopped, f)
  Up :: Action '(Closed, Stopped, f) '(Closed, GoingUp, f + 1)
  Down :: Action '(Closed, Stopped, f) '(Closed, GoingDown, NextFloor GoingDown f)
  Stop :: Action '(Closed, m, f) '(Closed, Stopped, f)
  Wait :: Action '(d, m, f) '(d, NextMoveState m f, NextFloor m f)
  Skip :: Action s s
  (:>>) :: Action s1 s2 -> Action s2 s3 -> Action s1 s3

type ElevatorProgram f f' = Action '(Opened, Stopped, f) '(Opened, Stopped, f')

program :: ElevatorProgram 0 0
program =
  Close
    :>> Up
    :>> Wait
    :>> Wait
    :>> Stop
    :>> Open
    :>> Close
    :>> Down
    :>> Wait
    :>> Wait
    :>> Stop
    :>> Open

-- badElevator :: ElevatorProgram 0 1
-- badElevator =
--   Close
--     :>> Up
--     :>> Open

printElevator :: Action s s' -> IO ()
printElevator Open = putStrLn "open"
printElevator Close = putStrLn "close"
printElevator Up = putStrLn "up"
printElevator Down = putStrLn "down"
printElevator Stop = putStrLn "stop"
printElevator Wait = putStrLn "wait"
printElevator Skip = return ()
printElevator (a :>> prog) = printElevator a >> printElevator prog

instance {-# OVERLAPS #-} Show (SomeSing Elevator) where
  show (SomeSing s) = show (fromSing s)

data SomeAction where
  SomeAction :: Action s s' -> Sing s -> Sing s' -> SomeAction

finalState :: SomeSing Elevator -> SomeAction -> Maybe (SomeSing Elevator)
finalState (SomeSing s1) (SomeAction _ s2 s3) =
  if fromSing s1 == fromSing s2
    then Just $ SomeSing s3
    else Nothing

actionFromString :: String -> SomeSing Elevator -> Maybe SomeAction
actionFromString name (SomeSing st) = action name st
  where
    act a s1 s2 = Just $ SomeAction a s1 s2
    action :: String -> Sing (s :: Elevator) -> Maybe SomeAction
    action "open" s@(STuple3 SClosed SStopped f) =
      act Open s (STuple3 SOpened SStopped f)
    action "close" s@(STuple3 SOpened SStopped f) =
      act Close s (STuple3 SClosed SStopped f)
    action "up" s@(STuple3 SClosed SStopped f) =
      act Up s (STuple3 SClosed SGoingUp (sNextFloor SGoingUp f))
    action "down" s@(STuple3 SClosed SStopped f) =
      act Down s (STuple3 SClosed SGoingDown (sNextFloor SGoingDown f))
    action "stop" s@(STuple3 SClosed _ f) =
      act Stop s (STuple3 SClosed SStopped f)
    action "wait" s@(STuple3 d m f) =
      act Wait s (STuple3 d (sNextMoveState m f) (sNextFloor m f))
    action _ _ = Nothing
