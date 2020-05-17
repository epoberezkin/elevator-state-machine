{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.TypeLits
import Elevator
import Numeric.Natural

main :: IO ()
main = do
  d :: DoorState <- getData "door state:"
  m :: MoveState <- getData "move state:"
  f :: Natural <- getData "floor:"
  print $ toESing (d,m,f)
  return ()

getData :: Read a => String -> IO a
getData s = read <$> (putStrLn s >> getLine)
