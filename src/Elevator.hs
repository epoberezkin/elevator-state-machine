{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE ConstraintKinds                #-}
{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE DeriveAnyClass                 #-}
{-# LANGUAGE EmptyCase                      #-}
{-# LANGUAGE FlexibleContexts               #-}
{-# LANGUAGE FlexibleInstances              #-}
{-# LANGUAGE FunctionalDependencies         #-}
{-# LANGUAGE GADTs                          #-}
{-# LANGUAGE InstanceSigs                   #-}
{-# LANGUAGE LambdaCase                     #-}
{-# LANGUAGE PartialTypeSignatures          #-}
{-# LANGUAGE PolyKinds                      #-}
{-# LANGUAGE RankNTypes                     #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE StandaloneDeriving             #-}
{-# LANGUAGE TemplateHaskell                #-}
{-# LANGUAGE TypeApplications               #-}
{-# LANGUAGE TypeFamilies                   #-}
{-# LANGUAGE TypeOperators                  #-}
{-# LANGUAGE UndecidableInstances           #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Elevator where

import Data.Kind
import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TH
import Data.Singletons.TypeLits
import Numeric.Natural

$(singletons [d|
  data DoorState = Opened | Closed
    deriving (Show, Read, Eq)
  data MoveState = Stopped | GoingUp | GoingDown
    deriving (Show, Read, Eq)

  nextFloor :: MoveState -> Nat -> Nat
  nextFloor Stopped   f = f
  nextFloor GoingUp   f = f + 1
  nextFloor GoingDown f = max (f - 1) 1
  |])


infixr 5 :>
data Action (d :: DoorState) (m :: MoveState) (f :: Nat)
            (d':: DoorState) (m':: MoveState) (f':: Nat) :: Type where
  Open   :: Action
              Closed Stopped f
              Opened Stopped f

  Close  :: Action
              Opened Stopped f
              Closed Stopped f

  Up     :: Action
              Closed Stopped f
              Closed GoingUp (NextFloor GoingUp f)

  Down   :: Action
              Closed Stopped f
              Closed GoingDown (NextFloor GoingDown f)

  Stop   :: Action  
              Closed m f
              Closed Stopped f

  Wait   :: Action
              d m f
              d m (NextFloor m f)

  (:>)   :: Action d1 m1 f1 d2 m2 f2
         -> Action d2 m2 f2 d3 m3 f3
         -> Action d1 m1 f1 d3 m3 f3

type ElevatorProgram f f' = Action Opened Stopped f Opened Stopped f'

program :: ElevatorProgram 1 1
program =
  Close :> Up
  :> Wait :> Wait
  :> Stop :> Open
  :> Close :> Down
  :> Wait :> Wait
  :> Stop :> Open

-- badProg :: ElevatorSeq d m f d' m' f' =
--   Close :> Up :> Open

printElevator :: Action d m f d' m' f' -> IO ()
printElevator Open  = putStrLn "Open"
printElevator Close = putStrLn "Close"
printElevator Up    = putStrLn "Up"
printElevator Down  = putStrLn "Down"
printElevator Stop  = putStrLn "Stop"
printElevator Wait  = putStrLn "Wait"
printElevator (a :> prog) = printElevator a >> printElevator prog


type State d m f = ( Sing (d :: DoorState)
                   , Sing (m :: MoveState)
                   , Sing (f :: Nat) )

fromState :: State d m f -> (DoorState, MoveState, Natural)
fromState (d,m,f) = (fromSing d, fromSing m, fromSing f)

toState :: (DoorState, MoveState, Natural)
        -> (SomeSing DoorState, SomeSing MoveState, SomeSing Nat)
toState (d,m,f) = (toSing d, toSing m, toSing f)

data SomeState = forall d m f. SomeState (State d m f)

instance Show SomeState where
  show (SomeState s) = show (fromState s)

data SomeAction where
  SomeAction :: State d m f
             -> State d' m' f'
             -> Action d m f d' m' f'
             -> SomeAction

mkSomeState :: DoorState -> MoveState -> Natural -> SomeState
mkSomeState (FromSing d) (FromSing m) (FromSing f) = SomeState (d,m,f)

nextState :: SomeState -> SomeAction -> Maybe SomeState
nextState (SomeState s1) (SomeAction s2 s3 _) =
  if fromState s1 == fromState s2
    then Just $ SomeState s3
    else Nothing


actionFromName :: SomeState -> String -> Maybe SomeAction
actionFromName (SomeState st) = action st
  where
    someA a s s' = Just $ SomeAction a s s'

    action :: State d m f -> String -> Maybe SomeAction
    action s@(SClosed, SStopped, f) "open" =
      someA s (SOpened, SStopped, f) Open

    action s@(SOpened, SStopped, f) "close" =
      someA s (SClosed, SStopped, f) Close

    action s@(SClosed, SStopped, f) "up" =
      someA s (SClosed, SGoingUp, sNextFloor SGoingUp f) Up

    action s@(SClosed, SStopped, f) "down" =
      someA s (SClosed, SGoingDown, sNextFloor SGoingDown f) Down

    action s@(SClosed, _, f) "stop" =
      someA s (SClosed, SStopped, f) Stop

    action s@(d, m, f) "wait" =
      someA s (d, m, sNextFloor m f) Wait

    action _ _ = Nothing
