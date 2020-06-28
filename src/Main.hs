{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Singletons
import Elevator
-- import Numeric.Natural
import System.IO
import System.IO.Interact

main :: IO ()
main = do
  let elevator = toSing (Opened, Stopped, 0)
  print elevator
  replState runElevator elevator

runElevator :: String -> SomeSing Elevator -> (String, SomeSing Elevator)
runElevator act st =
  case actionFromString act st >>= finalState st of
    Just st' -> (show st', st')
    Nothing -> ("action " ++ act ++ " not allowed", st)
--
-- main :: IO ()
-- main = do
--   let elevator = toDep (Opened, Stopped, 0)
--   runElevator elevator

-- runElevator :: SomeElevator -> IO ()
-- runElevator st = do
--   putStrLn $ show st ++ "\n> "
--   hFlush stdout
--   act <- getLine
--   case actionFromString act st >>= finalState st of
--     Just st' -> runElevator st'
--     Nothing -> do
--       putStrLn $ "action " ++ act ++ " not allowed"
--       runElevator st
