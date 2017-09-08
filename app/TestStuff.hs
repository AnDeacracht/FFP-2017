{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module TestStuff where

import qualified Data.Map as Map

import DataDeclarations
import GameStateFunctions
import Colour
import Utils
import Player
import GameState
import MoveFunctions


beforeHouseRed = Player "red" 3 0 ["40"] "1" "40" False
beforeHouseBlue = Player "blue" 3 0 ["10"] "11" "10" False
beforeHouseGreen = Player "green" 3 0 ["20"] "21" "20" False
beforeHouseYellow = Player "yellow" 3 0 ["30"] "31" "30" False


red :: Player
red = Player "red" 2 1 ["38", "goal-red-4"] "1" "40" False

blue :: Player
blue = Player "blue" 3 0 ["1"] "11" "10" False

green :: Player
green = Player "green" 3 0 ["14"] "21" "20" False

yellow :: Player
yellow = Player "yellow" 3 0 ["2"] "31" "30" False

red2 :: Player -- tests leaving house as last piece
red2 = Player "red" 1 0 ["38", "12", "20"] "1" "40" False 

blue2 :: Player -- tests rolling 6's with empty house
blue2 = Player "blue" 0 0 ["37", "16", "17", "18"] "11" "10" False

red3 :: Player 
red3 = Player "red" 1 0 ["13", "3", "9"] "1" "40" False 

red4 :: Player
red4 = Player "red" 2 0 ["5", "18"] "1" "40" False 

blue4 :: Player
blue4 = Player "blue" 3 0 ["4"] "11" "10" False

green4 :: Player
green4 = Player "green" 3 0 ["27"] "21" "20" False  

yellow4 :: Player 
yellow4 = Player "yellow" 1 0 ["6", "33", "36"] "31" "30" False

yellow3 :: Player 
yellow3 = Player "yellow" 2 0 ["6", "39"] "31" "30" False

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

testPlayers2 :: Map.Map Colour Player
testPlayers2 =
    Map.fromList 
    [ (Red, red2)
    , (Blue, blue2) 
    , (Green, boardingTester) -- tests going aboard
    , (Yellow, getCapturedOnMoveTester) -- tests capturing on move
    ]

testPlayers3 :: Map.Map Colour Player
testPlayers3 =
    Map.fromList 
    [ (Red, red3)
    , (Blue, blue2) 
    , (Green, boardingTester) -- tests going aboard
    , (Yellow, yellow3) -- tests capturing on move
    ]

testPlayers4 :: Map.Map Colour Player
testPlayers4 =
    Map.fromList 
    [ (Red, red4)
    , (Blue, blue4) 
    , (Green, green4) -- tests going aboard
    , (Yellow, yellow4) -- tests capturing on move
    ]

{--

yellow moves 6 to 9 with a 3, kills red there
red moves from 3 to 6 with 3
BUG : red also appears on 9, sharing the field with yellow

--}

testBullshitState :: GameState
testBullshitState = 
    GameState 
    { players = testPlayers3
    , activePlayer = yellow3
    , rollsLeft = 0
    , currentRoll = 3
    , waitingForMove = True
    }

testAnusState :: GameState
testAnusState =
    GameState
    { players = testPlayers4
    , activePlayer = red4
    , rollsLeft = 0
    , currentRoll = 5
    , waitingForMove = True
    }

testCrapState :: GameState 
testCrapState =
    GameState
    { players = Map.fromList
        [ (Red, redP)
        , (Blue, Player "blue" 3 0 ["10"] "11" "10" False)
        , (Green, greenP)
        , (Yellow, Player "yellow" 2 0 ["37", "33"] "31" "30" False)
        ]
    , activePlayer = greenP
    , rollsLeft = 0
    , currentRoll = 2
    , waitingForMove = True
    }
    where
        redP = Player "red" 3 0 ["24"] "1" "40" False
        greenP = Player "green" 3 0 ["32"] "21" "20" False
{--
    Players:
    Colour: "red"
    In house: 3
    In goal: 0
    Occupies fields: ["24"]
    Must leave start: False
    ------------
    Colour: "blue"
    In house: 3
    In goal: 0
    Occupies fields: ["10"]
    Must leave start: False
    ------------
    Colour: "green"
    In house: 3
    In goal: 0
    Occupies fields: ["32"]
    Must leave start: False
    ------------
    Colour: "yellow"
    In house: 2
    In goal: 0
    Occupies fields: ["37","33"]
    Must leave start: False
    ------------
Currently active: "green"
Rolls left: 1
Current roll: 0
Waiting for move: False



--}

testSixesState :: GameState
testSixesState = 
    GameState 
    { players = testPlayers2
    , activePlayer = red2
    , rollsLeft = 1
    , currentRoll = 0
    , waitingForMove = False
    }

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