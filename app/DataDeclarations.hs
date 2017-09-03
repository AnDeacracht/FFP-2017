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

import GameState

data NaBiodhFeargOrt = NaBiodhFeargOrt { gameState :: MVar GameState }

data Alignment  = Horizontal | Vertical
data FieldPart  = Top | Bottom
data Goal       = TopGoal | BottomGoal | LeftGoal | RightGoal

data MoveType = 
        FieldMove       { rollResult :: Int }
    |   GoalMove        { rollResult :: Int } 
    |   EnterGoalMove   { rollResult :: Int } 
    |   InvalidMove     { message :: String } 
    deriving (Show)


type DiceRoll = Int
type FieldId = String
