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


type ESing d m f = ( Sing (d :: DoorState)
                   , Sing (m :: MoveState)
                   , Sing (f :: Nat) )
type ESingI d m f = ( SingI (d :: DoorState)
                    , SingI (m :: MoveState)
                    , SingI (f :: Nat) )
type StateI d m f = ( ESingI d m f
                    , KnownNat f )
type ESomeSing = (SomeSing DoorState, SomeSing MoveState, SomeSing Nat)

fromESing :: ESing d m f -> (DoorState, MoveState, Natural)
fromESing (d,m,f) = (fromSing d, fromSing m, fromSing f)

toESing :: (DoorState, MoveState, Natural) -> ESomeSing
toESing (d,m,f) = (toSing d, toSing m, toSing f)

esing :: StateI d m f => ESing d m f
esing = (sing, sing, sing)

withState :: ESing d m f -> ((StateI d m f, KnownNat (NextFloor m f)) => r) -> r
withState (Sing, m@Sing, f@Sing) func =
  withKnownNat f $ withKnownNat (sNextFloor m f) func

data SomeState where
  SomeState :: ESing d m f -> SomeState

instance Show SomeState where
  show (SomeState s) = show (fromESing s)

data SomeAction where
  SomeAction :: ESing d m f
             -> ESing d' m' f'
             -> Action d m f d' m' f'
             -> SomeAction

mkSomeState :: (DoorState, MoveState, Natural) -> SomeState
mkSomeState st = case toESing st of
  (SomeSing d, SomeSing m, SomeSing f) -> SomeState (d,m,f)

nextState :: SomeState -> SomeAction -> Maybe SomeState
nextState (SomeState s1) (SomeAction s2 s3 _) =
  if fromESing s1 == fromESing s2
    then Just $ SomeState s3
    else Nothing

someAction :: (StateI d m f, StateI d' m' f')
           => Action d m f d' m' f'
           -> SomeAction
someAction = SomeAction esing esing

actionFromString :: SomeState -> String -> Maybe SomeAction
actionFromString (SomeState s) name = withState s $ action s name
  where
    action :: (StateI d m f, KnownNat (NextFloor m f)) => ESing d m f -> String -> Maybe SomeAction
    action (SClosed, SStopped, f) "open" = Just $ someAction $ open f
      where
        open :: KnownNat f => Sing f -> Action Closed Stopped f Opened Stopped f
        open _ = Open

    action (SOpened, SStopped, f) "close" = Just $ someAction $ close f
      where
        close :: KnownNat f => Sing f -> Action Opened Stopped f Closed Stopped f
        close _ = Close

    action (SClosed, SStopped, f) "up" = Just $ someAction $ up f
      where
        up :: KnownNat f => Sing f -> Action Closed Stopped f Closed GoingUp f
        up _ = Up

    action (SClosed, SStopped, f) "down" = Just $ someAction $ down f
      where
        down :: KnownNat f => Sing f -> Action Closed Stopped f Closed GoingDown f
        down _ = Down

    action (SClosed, m, f) "stop" = Just $ someAction $ stop (m,f)
      where
        stop :: KnownNat f => (Sing m, Sing f) -> Action Closed m f Closed Stopped f
        stop _ = Stop

    action (d,m,f) "wait" = Just $ someAction $ wait (d,m,f) (sNextFloor m f)
      where
        wait :: (KnownNat f, KnownNat (NextFloor m f))
             => ESing d m f -> Sing (NextFloor m f)
             -> Action d m f d m (NextFloor m f)
        wait _ _ = Wait

    action _ _ = Nothing
