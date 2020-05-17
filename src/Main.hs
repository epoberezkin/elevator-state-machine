{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Elevator
import Numeric.Natural
import System.IO

main :: IO ()
main = do
  -- d :: DoorState <- getData "door state:"
  -- m :: MoveState <- getData "move state:"
  -- f :: Natural <- getData "floor:"
  -- let state = mkSomeState d m f
  let state = mkSomeState Opened Stopped 1
  runElevator state
  return ()

-- getData :: Read a => String -> IO a
-- getData s = read <$> (putStrLn s >> getLine)

runElevator :: SomeState -> IO ()
runElevator st = do
  putStr $ show st ++ ": "
  hFlush stdout
  name <- getLine
  let maybeAction = actionFromName st name
  case maybeAction >>= nextState st of
    Just st' -> runElevator st'
    Nothing -> do
      putStrLn $ "action " ++ name ++ " not allowed"
      runElevator st
