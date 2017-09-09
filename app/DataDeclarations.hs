{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module DataDeclarations where

import Control.Concurrent.MVar
import Database.Persist.TH
import Database.Persist.Sqlite as DB
import Data.List

import GameState

data NaBiodhFeargOrt = NaBiodhFeargOrt { gameState :: MVar GameState, persistence :: ConnectionPool }

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

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
QuizQuestion json
    questionText String
    correctAnswer String
    answer1 String
    answer2 String
    answer3 String
    answer4 String
    deriving Show
|]

quizQuestions :: [QuizQuestion]
quizQuestions = 
    [ QuizQuestion "Tá ocras..." "orm" "orm" "agam" "dom" "fúm" 
    , QuizQuestion "Tá grá agam..." "duit" "duit" "agat" "ort" "díot"
    , QuizQuestion "Tá mé ag..." "ól" "óladh" "ólaim" "ól" "ólta"
    ]