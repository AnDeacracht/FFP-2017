{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}

module GameState where

import GHC.Generics
import Data.Aeson
import qualified Data.Map as Map
import Data.Maybe

import Player
import Colour
import Utils

data GameState = 
    GameState 
    { players :: Map.Map Colour Player
    , activePlayer :: Player -- the player whose turn it is
    , rollsLeft :: Int
    , currentRoll :: Int
    , waitingForMove :: Bool
    } deriving (Generic)

instance Show GameState where
    show state = p ++ p1 ++ p2 ++ p3 ++ p4 ++ c ++ ra ++ r ++ w
        where
            p = "\nPlayers:\n"
            p1 = show $ fromJust $ Map.lookup Red $ players state
            p2 = show $ fromJust $ Map.lookup Blue $ players state
            p3 = show $ fromJust $ Map.lookup Green $ players state
            p4 = show $ fromJust $ Map.lookup Yellow $ players state
            c = "Currently active: " ++  show (colour (activePlayer state)) ++ "\n" 
            ra = "Rolls left: " ++ show (rollsLeft state) ++ "\n"
            r = "Current roll: " ++ show (currentRoll state) ++ "\n"
            w = "Waiting for move: " ++ show (waitingForMove state) ++ "\n"

instance ToJSON GameState
--instance FromJSON GameState

setPlayers :: Map.Map Colour Player -> GameState -> GameState
setPlayers newPlayers state = 
    GameState 
    { players = newPlayers
    , activePlayer = activePlayer state
    , rollsLeft = rollsLeft state
    , currentRoll = currentRoll state
    , waitingForMove = waitingForMove state
    }

setSinglePlayer :: Player -> GameState -> GameState
setSinglePlayer player state = setPlayers (Map.adjust (\_ -> player) (readColour (colour player)) (players state)) state


setActivePlayer :: Player -> GameState -> GameState
setActivePlayer newActivePlayer state = 
    GameState 
    { players = players state
    , activePlayer = newActivePlayer
    , rollsLeft = rollsLeft state
    , currentRoll = currentRoll state
    , waitingForMove = waitingForMove state
    }

setRollsLeft :: Int -> GameState -> GameState
setRollsLeft newRollsLeft state = 
    GameState 
    { players = players state
    , activePlayer = activePlayer state
    , rollsLeft = newRollsLeft
    , currentRoll = currentRoll state
    , waitingForMove = waitingForMove state
    }

setCurrentRoll :: Int -> GameState -> GameState
setCurrentRoll newCurrentRoll state = 
    GameState 
    { players = players state
    , activePlayer = activePlayer state
    , rollsLeft = rollsLeft state
    , currentRoll = newCurrentRoll
    , waitingForMove = waitingForMove state
    }

setWaitingForMove :: Bool -> GameState -> GameState
setWaitingForMove newWaitingForMove state = 
    GameState 
    { players = players state
    , activePlayer = activePlayer state
    , rollsLeft = rollsLeft state
    , currentRoll = currentRoll state
    , waitingForMove = newWaitingForMove
    }