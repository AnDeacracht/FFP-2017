{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module Utils where

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe
import System.Random
import System.IO.Unsafe

import Colour
import Player

type DiceRoll = Int
type FieldId = String

rollDie :: Int 
rollDie = unsafePerformIO $ randomRIO (1, 6)

toInt :: String -> Int
toInt str = read str + 0

getPlayerByIndex :: Int -> Map.Map Colour Player -> Player
getPlayerByIndex 1 playerMap = fromJust $ Map.lookup Red playerMap
getPlayerByIndex 2 playerMap = fromJust $ Map.lookup Blue playerMap
getPlayerByIndex 3 playerMap = fromJust $ Map.lookup Green playerMap
getPlayerByIndex _ playerMap = fromJust $ Map.lookup Yellow playerMap

readColour :: String -> Colour
readColour "red" = Red
readColour "blue" = Blue
readColour "green" = Green
readColour _ = Yellow


makeGoalCellNumber :: Player -> Int -> FieldId
makeGoalCellNumber player nr = "goal-" ++ show nr ++ "-" ++ colour player

extractGoalCellNumber :: FieldId -> Int
extractGoalCellNumber fieldId = toInt (T.unpack (T.splitOn "-" (T.pack fieldId) !! 1))

-- removes first and last element from list

coreList :: [a] -> [a]
coreList [] = []
coreList [_] = []
coreList xs = tail (init xs)

removeItem :: (Eq a) => a -> [a] -> [a]
removeItem _ [] = []
removeItem item (x:xs) = if item == x then xs else x : removeItem item xs 

nothingOnBoard :: Player -> Bool
nothingOnBoard player = null $ occupiedFields player

nothingInHouse :: Player -> Bool
nothingInHouse player = inHouse player <= 0

nothingInGoal :: Player -> Bool
nothingInGoal player = inGoal player <= 0
