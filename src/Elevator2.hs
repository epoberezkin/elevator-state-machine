{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Elevator2 where

infix 5 :::
data ElevatorState (m :: MovementState) (d :: DoorState) where
  ElevatorState :: ElevatorState m d
type (:::) m d = ElevatorState m d

data DoorState = Opened | Closed | Opening | Closing

data MovementState = Stopped | MovingUp | MovingDown

infix 4 >>>
type family (>>>) action state where
  OpenA       >>> Stopped ::: Closed  = Stopped ::: Opening
  CloseA      >>> Stopped ::: Opened  = Stopped ::: Closing
  DoorSensorA >>> Stopped ::: Opening = Stopped ::: Opened
  DoorSensorA >>> Stopped ::: Closing = Stopped ::: Closed

data ActionType = OpenA | CloseA | DoorSensorA

data Action (action :: ActionType) state state' where
  Open       :: Action
                  OpenA
                  (Stopped ::: Closed)
                  (OpenA >>> (Stopped ::: Closed))
  Close      :: Action
                  CloseA
                  (Stopped ::: Opened)
                  (CloseA >>> (Stopped ::: Opened))
  DoorSensor :: Action
                  DoorSensorA
                  (Stopped ::: s)
                  (DoorSensorA >>> (Stopped ::: s))