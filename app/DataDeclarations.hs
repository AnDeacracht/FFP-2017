{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}

module DataDeclarations where

import Yesod
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Control.Concurrent.MVar
import qualified Data.Map as Map

data NaBiodhFeargOrt = NaBiodhFeargOrt { gameState :: MVar GameState }

data Alignment  = Horizontal | Vertical
data FieldPart  = Top | Bottom
data Goal       = TopGoal | BottomGoal | LeftGoal | RightGoal
data Colour     = Red | Blue | Green | Yellow deriving (Ord, Eq, Generic)

instance Show Colour where
    show c = case c of
        Red -> "red"
        Blue -> "blue"
        Green -> "green"
        Yellow -> "yellow"

instance Enum Colour where
    
    succ Red = Blue
    succ Blue = Green
    succ Green = Yellow
    succ Yellow = Red

    toEnum 1 = Red
    toEnum 2 = Blue
    toEnum 3 = Green
    toEnum _ = Yellow

    fromEnum Red = 1
    fromEnum Blue = 2
    fromEnum Green = 3
    fromEnum _ = 4

instance ToJSON Colour
--instance FromJSON Colour
instance ToJSONKey Colour
--instance FromJSONKey Colour

type DiceRoll = Int
type FieldId = String

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

data GameState = 
    GameState 
    { players :: Map.Map Colour Player
    , activePlayer :: Player -- the player whose turn it is
    , rollsAllowed :: Int
    , roll :: Int
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
            ra = "Rolls allowed: " ++ show (rollsAllowed state) ++ "\n"
            r = "Current roll: " ++ show (roll state) ++ "\n"
            w = "Waiting for move: " ++ show (waitingForMove state) ++ "\n"

instance ToJSON Player
--instance FromJSON Player

instance ToJSON GameState
--instance FromJSON GameState

