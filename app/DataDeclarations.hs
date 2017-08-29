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
data Colour     = Red | Blue | Green | Yellow deriving (Enum, Ord, Eq, Generic)

instance Show Colour where
    show c = case c of
        Red -> "red"
        Blue -> "blue"
        Green -> "green"
        Yellow -> "yellow"

instance ToJSON Colour
instance FromJSON Colour
instance ToJSONKey Colour
instance FromJSONKey Colour

type DiceRoll = Int


data Player = Player { 
    inHouse :: Int, 
    inGoal :: Int, 
    occupiedFields :: [String],
    startField :: String,
    mustLeaveStart :: Bool
} deriving (Show, Generic)

data GameState = GameState {
    players :: Map.Map Colour Player,
    turn :: Colour, -- the player whose turn it is
    rollsAllowed :: Int
} deriving (Show, Generic)

instance ToJSON Player
instance FromJSON Player

instance ToJSON GameState
instance FromJSON GameState

