{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}

module Player where

import GHC.Generics
import Data.Aeson

data Player = 
    Player 
    { colour :: String -- string rep for easier JSON access
    , inHouse :: Int 
    , inGoal :: Int 
    , occupiedFields :: [String]
    , startField :: String
    , finalField :: String
    , mustLeaveStart :: Bool
    } deriving (Generic)

instance Show Player where
    show player = c ++ i ++ g ++ f ++ m ++ sp
        where
            sp = "\t------------\n" 
            c = "\tColour: " ++ show (colour player) ++ "\n" 
            i = "\tIn house: " ++ show (inHouse player) ++ "\n"
            g = "\tIn goal: " ++ show (inGoal player) ++ "\n"
            f = "\tOccupies fields: " ++ show (occupiedFields player) ++ "\n"
            --s = "\tStarts from: " ++ show (startField player) ++ "\n" 
            --ff = "\tFinal field: " ++ show (finalField player) ++ "\n"
            m = "\tMust leave start: " ++ show (mustLeaveStart player) ++ "\n"

instance ToJSON Player
--instance FromJSON Player

setInHouse :: Int -> Player -> Player
setInHouse newInHouse player =
    Player 
    { colour = colour player
    , inHouse = newInHouse
    , inGoal = inGoal player 
    , occupiedFields = occupiedFields player
    , startField = startField player
    , finalField = finalField player
    , mustLeaveStart = mustLeaveStart player
    }

setInGoal :: Int -> Player -> Player
setInGoal newInGoal player =
    Player 
    { colour = colour player
    , inHouse = inHouse player
    , inGoal = newInGoal
    , occupiedFields = occupiedFields player
    , startField = startField player
    , finalField = finalField player
    , mustLeaveStart = mustLeaveStart player
    }

setOccupiedFields :: [String] -> Player -> Player
setOccupiedFields newOccupiedFields player =
    Player 
    { colour = colour player
    , inHouse = inHouse player
    , inGoal = inGoal player
    , occupiedFields = newOccupiedFields
    , startField = startField player
    , finalField = finalField player
    , mustLeaveStart = mustLeaveStart player
    }

setMustLeaveStart :: Bool -> Player -> Player
setMustLeaveStart newMustLeaveStart player =
    Player 
    { colour = colour player
    , inHouse = inHouse player
    , inGoal = inGoal player
    , occupiedFields = occupiedFields player
    , startField = startField player
    , finalField = finalField player
    , mustLeaveStart = newMustLeaveStart
    }