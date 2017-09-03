{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}

module GameStateFunctions where

import System.Random
import System.IO.Unsafe
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe
import Data.List

import DataDeclarations
import GameState
import Player
import MoveFunctions
import Colour
import Utils

{-- INITIAL STATE --}

allPlayers :: Map.Map Colour Player
allPlayers = 
    Map.fromList 
    [ (Red, Player "red" 4 0 [] "1" "40" False)
    , (Blue, Player "blue" 4 0 [] "11" "10" False)
    , (Green, Player "green" 4 0 [] "21" "20" False)
    , (Yellow, Player "yellow" 4 0 [] "31" "30" False)
    ]

initialState :: GameState
initialState =  
    GameState 
    { players = allPlayers
    , activePlayer = getPlayerByIndex (unsafePerformIO $ randomRIO (1, 4)) allPlayers
    , rollsLeft = 3
    , currentRoll = 0
    , waitingForMove = False
    }

{-- THE ROLL HANDLER --}

-- Goes active once the player rolls. If the player rolls a 6, action needs to be taken immediately
-- as the player may get no choice which piece they want to move.

handleRoll :: GameState -> DiceRoll -> GameState
handleRoll state 6
    | nothingOnBoard activeP = putPieceOnBoard state -- nothing on board, put piece on it
    | nothingInHouse activeP = -- nothing left in house, reroll
        GameState
        { players = players state
        , activePlayer = activeP
        , rollsLeft = 1 -- you must reroll if you have a 6
        , currentRoll = currentRoll state + 6
        , waitingForMove = False
        }
    | otherwise = checkStartField
    where
        activeP = activePlayer state
        start = startField activeP
        checkStartField = -- check start field - if something is there, it needs to move on
            if start `elem` (occupiedFields activeP) 
                then -- check to see if you can move away immediately
                    case (determineMoveType activeP start 6) of
                        FieldMove roll -> fieldMove state start 6 -- move away immediately
                        InvalidMove _ -> GameState -- TODO propagate the forced move until all pieces have been checked. Monad instance for MoveType?
                        -- for now we just make the player give up their turn
                            { players = players state
                            , activePlayer = nextPlayer state activeP
                            , rollsLeft = determineRolls state
                            , currentRoll = currentRoll state + 6
                            , waitingForMove = False
                            }
                else putPieceOnBoard state -- put new piece on board

-- if the player rolls anything else
handleRoll state rollResult
    | nothingOnBoard activeP = checkRollCount
    | mustLeaveStart activeP = -- move immediately, you get no choice
        case (determineMoveType activeP start rollResult) of 
            FieldMove roll -> fieldMove state (startField activeP) rollResult 
            InvalidMove _ -> GameState 
                { players = players state
                , activePlayer = nextPlayer state activeP
                , rollsLeft = determineRolls state
                , currentRoll = currentRoll state + rollResult
                , waitingForMove = False
                }
    | otherwise = -- if you needn't vacate the start field, wait for user input
        if cannotMove (activePlayer state) rollResult -- check here if the player has rolled too high to move etc
            then GameState 
                { players = players state
                , activePlayer = nextPlayer state activeP
                , rollsLeft = determineRolls state
                , currentRoll = currentRoll state + rollResult
                , waitingForMove = False
                }
            else GameState 
                { players = players state
                , activePlayer = activeP
                , rollsLeft = 0 -- no reroll
                , currentRoll = currentRoll state + rollResult
                , waitingForMove = True -- wait for move command
                }
    where
        activeP = activePlayer state
        start = startField activeP 
        checkRollCount =
            -- still more than one roll left
            if (rollsLeft state) > 1
                then GameState 
                { players = players state
                , activePlayer = activeP
                , rollsLeft = (rollsLeft state) - 1
                , currentRoll = rollResult
                , waitingForMove = False
                }
                -- all rolls used up, move on
                else GameState 
                { players = players state
                , activePlayer = nextPlayer state activeP
                , rollsLeft = 3
                , currentRoll = rollResult
                , waitingForMove = False
                }

{-- MOVE FUNCTIONS --}

handleMoveRequest :: GameState -> FieldId -> GameState
handleMoveRequest state fromField = move state moveType fromField
    where
        moveType = determineMoveType (activePlayer state) fromField (currentRoll state)

move :: GameState -> MoveType -> FieldId -> GameState
move state moveType fromField = case moveType of
    FieldMove roll -> fieldMove state fromField roll
    EnterGoalMove roll -> enterGoalMove state fromField roll
    GoalMove roll -> goalMove state fromField roll
    InvalidMove _  -> state

-- if it's a GoalMove

goalMove :: GameState -> FieldId -> DiceRoll -> GameState
goalMove state fromField roll = GameState 
    { players = handleCapture state activeP fromField
    , activePlayer = nextPlayer state activeP
    , rollsLeft = determineRolls state
    , currentRoll = roll
    , waitingForMove = False
    }
    where
        activeP = activePlayer state
        targetField = makeGoalMove activeP fromField roll
        newOccupiedFields = targetField : delete fromField (occupiedFields activeP)
        updatedPlayer = setOccupiedFields newOccupiedFields activeP

-- if it's a FieldMove

fieldMove :: GameState -> FieldId -> DiceRoll -> GameState
fieldMove state fromField roll = GameState 
    { players = handleCapture state activeP fromField
    , activePlayer = nextPlayer state activeP
    , rollsLeft = determineRolls state
    , currentRoll = roll
    , waitingForMove = False
    }
    where 
        activeP = activePlayer state -- the player that will move
        targetField = makeFieldMove fromField roll -- where we will end up
        newOccupiedFields = targetField : delete fromField (occupiedFields activeP) -- delete the old field from the player's field list
        updatedPlayer = setOccupiedFields newOccupiedFields activeP-- update player information

-- if it's an EnterGoal move

enterGoalMove :: GameState -> FieldId -> DiceRoll -> GameState
enterGoalMove state fromField roll = GameState 
    { players = handleCapture state activeP fromField
    , activePlayer = nextPlayer state activeP
    , rollsLeft = determineRolls state
    , currentRoll = roll
    , waitingForMove = False
    }
    where
       activeP = activePlayer state -- the player that will move
       targetField = makeEnterGoalMove activeP fromField roll
       newOccupiedFields = targetField : delete fromField (occupiedFields activeP)
       updatedPlayer = (setInGoal $ inGoal activeP + 1) . (setOccupiedFields newOccupiedFields) $ activeP 

putPieceOnBoard :: GameState -> GameState
putPieceOnBoard state
    | arePiecesInHouse = modifiedState
    | otherwise = state -- should not happen since handleRoll takes care of this
    where
        activeP = activePlayer state
        arePiecesInHouse = (inHouse activeP) > 0
        updatedPlayer = Player 
            { colour = colour activeP
            , inHouse = inHouse activeP - 1
            , inGoal = inGoal activeP
            , occupiedFields = (startField activeP) : (occupiedFields activeP)
            , startField = startField activeP
            , finalField = finalField activeP
            , mustLeaveStart = True 
            }
        modifiedState = GameState 
            { players = handleCapture state updatedPlayer (startField updatedPlayer)
            , activePlayer = updatedPlayer
            , rollsLeft = 1
            , currentRoll = 6 -- always a six that makes you go aboard
            , waitingForMove = False
            }

{-- UTILITY FUNCTIONS --}

handleCapture :: GameState -> Player -> FieldId -> Map.Map Colour Player
handleCapture state capturer fieldId = case capturedOccupier of 
    Just occupier ->
        let m1 = Map.adjust (\_ -> occupier) (readColour (colour occupier)) (players state)
        in Map.adjust (\_ -> capturer) (readColour (colour capturer)) m1
    Nothing -> Map.adjust (\_ -> capturer) (readColour (colour capturer)) (players state)
    where 
        fieldOccupier = fieldOccupiedBy state fieldId
        capturedOccupier = case fieldOccupier of
            Just player ->  Just $ (setInHouse $ inHouse player + 1) . (setOccupiedFields $ delete fieldId (occupiedFields player)) $ player
            Nothing -> Nothing
        
fieldOccupiedBy :: GameState -> FieldId -> Maybe Player
fieldOccupiedBy state fieldId = case occupier of
    [p] -> Just p
    _  -> Nothing
    where occupier = filter (\p -> fieldId `elem` (occupiedFields p)) $ Map.elems (players state)


determineRolls :: GameState -> Int -- three rolls if house empty and board empty
determineRolls state
    | (boardEmpty) && (goalEmpty) = 3
    | otherwise = 1
    --TODO half-empty goal with gaps
    where 
        nextUp = nextPlayer state (activePlayer state)
        boardEmpty = nothingOnBoard nextUp
        goalEmpty = nothingInGoal nextUp

nextPlayer :: GameState -> Player -> Player
nextPlayer state player = fromJust $ Map.lookup nextCol $ players state
    where nextCol = succ $ readColour $ colour player

