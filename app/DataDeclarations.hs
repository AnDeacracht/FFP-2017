{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}

module DataDeclarations where

import Control.Concurrent.MVar

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

isInvalid :: MoveType -> Bool
isInvalid moveType = case moveType of
    InvalidMove _ -> True
    _             -> False

