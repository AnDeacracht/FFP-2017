{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}

module DataDeclarations where

import Yesod
import Text.Hamlet
import Text.Lucius
import Text.Julius
import qualified Data.Text as T
import GHC.Generics
import Data.List
import Data.Aeson
import Data.Maybe
import Debug.Trace
import Control.Concurrent.MVar
import System.Random
import System.IO.Unsafe
import qualified Data.Map as Map

data NaBiodhFeargOrt = NaBiodhFeargOrt { gameState :: MVar GameState }

data Alignment  = Horizontal | Vertical
data FieldPart  = Top | Bottom
data Goal       = TopGoal | BottomGoal | LeftGoal | RightGoal
data Colour     = Red | Blue | Green | Yellow deriving (Ord, Eq, Generic, Enum, Bounded)

instance Show Colour where
    show c = case c of
        Red -> "red"
        Blue -> "blue"
        Green -> "green"
        Yellow -> "yellow"

instance ToJSON Colour
--instance FromJSON Colour
instance ToJSONKey Colour
--instance FromJSONKey Colour

type DiceRoll = Int


data Player = Player {
    colour :: String, -- string rep for easier JSON access
    inHouse :: Int, 
    inGoal :: Int, 
    occupiedFields :: [String],
    startField :: String,
    mustLeaveStart :: Bool
} deriving (Generic)

instance Show Player where
    show player = c ++ i ++ g ++ s ++ m ++ sp
        where
            sp = "\t------------\n" 
            c = "\tColour: " ++ show (colour player) ++ "\n" 
            i = "\tIn house: " ++ show (inHouse player) ++ "\n"
            g = "\tIn goal: " ++ show (inGoal player) ++ "\n"
            s = "\tStarts from: " ++ show (startField player) ++ "\n" 
            m = "\tMust leave start: " ++ show (mustLeaveStart player) ++ "\n"

data GameState = GameState {
    players :: Map.Map Colour Player,
    turn :: Colour, -- the player whose turn it is
    rollsAllowed :: Int
} deriving (Generic)

instance Show GameState where
    show state = p ++ p1 ++ p2 ++ p3 ++ p4 ++ c ++ r
        where
            p = "Players:\n"
            p1 = show $ fromJust $ Map.lookup Red $ players state
            p2 = show $ fromJust $ Map.lookup Blue $ players state
            p3 = show $ fromJust $ Map.lookup Green $ players state
            p4 = show $ fromJust $ Map.lookup Yellow $ players state
            c = "Currently active: " ++  show (turn state) ++ "\n" 
            r = "Rolls allowed: " ++ show (rollsAllowed state) ++ "\n"

instance ToJSON Player
--instance FromJSON Player

instance ToJSON GameState
--instance FromJSON GameState

