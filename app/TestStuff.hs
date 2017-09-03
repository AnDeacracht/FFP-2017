{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module TestStuff (boardingTest, capturingTest, captureOnMoveTest) where

import qualified Data.Map as Map

import DataDeclarations
import GameStateFunctions
import Colour
import Utils
import Player
import GameState
import MoveFunctions

red :: Player
red = Player "red" 3 0 ["38"] "1" "40" False

blue :: Player
blue = Player "blue" 3 0 ["1"] "11" "10" False

green :: Player
green = Player "green" 3 0 ["14"] "21" "20" False

yellow :: Player
yellow = Player "yellow" 3 0 ["2"] "31" "30" False

houseEntryTester :: Player
houseEntryTester = red

getCapturedTester :: Player
getCapturedTester = blue

boardingTester :: Player
boardingTester = green

getCapturedOnMoveTester :: Player
getCapturedOnMoveTester = yellow

testPlayers :: Map.Map Colour Player
testPlayers =
    Map.fromList 
    [ (Red, houseEntryTester) -- tests house entry
    , (Blue, getCapturedTester) -- tests capturing on boarding
    , (Green, boardingTester) -- tests going aboard
    , (Yellow, getCapturedOnMoveTester) -- tests capturing on move
    ]

testCapturingState :: GameState
testCapturingState = 
    GameState 
    { players = testPlayers
    , activePlayer = red
    , rollsLeft = 3
    , currentRoll = 0
    , waitingForMove = False
    }

testHouseEntryState :: GameState 
testHouseEntryState = 
    GameState 
    { players = testPlayers
    , activePlayer = red
    , rollsLeft = 1
    , currentRoll = 0
    , waitingForMove = False
    }

testBoardingState :: GameState 
testBoardingState = GameState
    { players = testPlayers
    , activePlayer = green
    , rollsLeft = 1
    , currentRoll = 0
    , waitingForMove = False
    }

testCaptureOnMoveState :: GameState 
testCaptureOnMoveState = GameState
    { players = testPlayers
    , activePlayer = blue
    , rollsLeft = 1
    , currentRoll = 0
    , waitingForMove = False
    }

-- blue is on 1, red goes aboard, captures blue. 
-- Blue: 4 in house, occupied []
-- Red: 2 in house, occupied: [1 38]
capturingTest :: GameState 
capturingTest = putPieceOnBoard testCapturingState

-- green boards.
-- Green: 2 in house, occupied: [21 14]
boardingTest :: GameState
boardingTest = putPieceOnBoard testBoardingState 

-- yellow is on 2, blue is on 1, captures yellow.
-- Yellow: 4 in house, occupied: []
-- Blue: 3 in house, occupied: [2]

captureOnMoveTest :: GameState 
captureOnMoveTest = handleMoveRequest testCaptureOnMoveState "1" 1

-- red is on 38, goes into house with a 4
-- Red: 3 in house, 1 in goal, occupied: ["goal-3-red"]
-- houseEntryTest :: GameState 