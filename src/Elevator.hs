{-# OPTIONS_GHC -Wall                       #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
{-# LANGUAGE ConstraintKinds                #-}
{-# LANGUAGE DataKinds                      #-}
{-# LANGUAGE DeriveAnyClass                 #-}
{-# LANGUAGE EmptyCase                      #-}
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
  |])

type family NextFloor (m :: MoveState) (f :: Nat) where
  NextFloor Stopped   f = f
  NextFloor GoingUp   f = f + 1
  NextFloor GoingDown f = f - 1

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
              Closed GoingUp f

  Down   :: Action
              Closed Stopped f
              Closed GoingDown f

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


data EState (d :: DoorState) (m :: MoveState) (f :: Nat) :: Type where
  EState :: EState d m f

type ESing d m f = (Sing (d :: DoorState), Sing (m :: MoveState), Sing (f :: Nat))
type ESingI d m f = (SingI (d :: DoorState), SingI (m :: MoveState), SingI (f :: Nat))
type ESomeSing = (SomeSing DoorState, SomeSing MoveState, SomeSing Nat)

fromESing :: ESing d m f -> (DoorState, MoveState, Natural)
fromESing (d,m,f) = (fromSing d, fromSing m, fromSing f)

toESing :: (DoorState, MoveState, Natural) -> ESomeSing
toESing (d,m,f) = (toSing d, toSing m, toSing f)

esing :: ESingI d m f => ESing d m f
esing = (sing, sing, sing)

mkES :: ESing d m f -> EState d m f
mkES _ = EState

data SomeState where
  SomeState :: ESing d m f -> EState d m f -> SomeState

data SomeAction where
  SomeAction :: ESing d m f -> ESing d' m' f' -> Action d m f d' m' f' -> SomeAction

someState :: ESing d m f -> SomeState
someState s = SomeState s (mkES s)

nextState :: SomeState -> SomeAction -> Maybe SomeState
nextState (SomeState s1 _) (SomeAction s2 s3 _) =
  if fromESing s1 == fromESing s2
    then Just $ someState s3
    else Nothing

someAction :: (ESingI d m f, ESingI d' m' f')
           => Action d m f d' m' f'
           -> SomeAction
someAction = SomeAction esing esing

actionFromString :: SomeState -> String -> Maybe SomeAction
actionFromString (SomeState (d,m,f) _) name =
  withKnownNat f $ action (d,m,f) name
  where
    action :: KnownNat f => ESing d m f -> String -> Maybe SomeAction
    action (SClosed, SStopped, f') "open" = Just $ someAction $ open f'
      where
        open :: KnownNat f => Sing f -> Action Closed Stopped f Opened Stopped f
        open _ = Open

    action _ _ = Nothing
