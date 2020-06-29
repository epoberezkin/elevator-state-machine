module Main where

import Data.List
import Data.Singletons
import Elevator
import Problems
import System.IO
import System.IO.Interact
import Text.Read

main :: IO ()
main = do
  let elevator = toSing (Opened, Stopped, 0)
  putStrLn "Enter: open, close, up, down, wait, stop or floor <n>"
  putStrLn $ show' elevator
  replState runElevator elevator

runElevator :: String -> SomeSing Elevator -> (String, SomeSing Elevator)
runElevator act st =
  let action = case stripPrefix "floor " act of
        Just f -> readMaybe f >>= elevatorToFloor st -- from Problems.hs #4
        Nothing -> actionFromString act st
   in case action >>= finalState st of
        Just st' -> (show' st', st')
        Nothing -> ("action " ++ act ++ " not allowed", st)

show' :: SomeSing Elevator -> String
show' (SomeSing s) = show (fromSing s)
