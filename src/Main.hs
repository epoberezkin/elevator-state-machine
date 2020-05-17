{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Elevator
import Numeric.Natural

main :: IO ()
main = do
  -- d :: DoorState <- getData "door state:"
  -- m :: MoveState <- getData "move state:"
  -- f :: Natural <- getData "floor:"
  let state = mkSomeState (Opened, Stopped, 1)
  print state
  runElevator state
  return ()

-- getData :: Read a => String -> IO a
-- getData s = read <$> (putStrLn s >> getLine)

runElevator :: SomeState -> IO ()
runElevator state = do
  putStrLn "action: " 
  name <- getLine
  let maybeAction = actionFromString state name
  case maybeAction >>= nextState state of
    Just state' -> do
      print state'
      runElevator state'
    Nothing -> do
      putStrLn $ "action " ++ name ++ " not allowed, state did not change"
      runElevator state
