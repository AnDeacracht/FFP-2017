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
    , rollsAllowed = 3
    , roll = 0
    , waitingForMove = False
    }

{-- THE ROLL HANDLER --}

handleRoll :: GameState -> DiceRoll -> GameState
-- if the player rolls a 6, take action immediately.
handleRoll state 6
    | nothingOnBoard activeP = putPieceOnBoard state-- nothing on board, put piece on it
    | nothingInHouse activeP = -- nothing left in house, reroll
        GameState
        { players = players state
        , activePlayer = activeP
        , rollsAllowed = 1 -- you must reroll if you have a 6
        , roll = roll state + 6
        , waitingForMove = False
        }
    | otherwise = checkStartField
    where
        activeP = activePlayer state
        checkStartField = -- check start field - if something is there, is needs to move on
            if (startField activeP) `elem` (occupiedFields activeP) 
                then movePlayer state 6 (startField activeP) -- move away immediately
                else putPieceOnBoard state -- put new piece on board

-- if the player rolls anything else
handleRoll state rollResult
    | nothingOnBoard activeP = checkRollCount
    | mustLeaveStart activeP = -- move immediately, you get no choice
        movePlayer state rollResult (startField activeP) 
    | otherwise = -- if you needn't vacate the start field, wait for user input
        GameState 
        { players = players state
        , activePlayer = activeP
        , rollsAllowed = 0 -- no reroll
        , roll = roll state + rollResult
        , waitingForMove = True -- wait for move command
        }
    where
        activeP = activePlayer state
        checkRollCount =
            -- still more than one roll allowed
            if (rollsAllowed state) > 1
                then GameState 
                { players = players state
                , activePlayer = activeP
                , rollsAllowed = (rollsAllowed state) - 1
                , roll = rollResult
                , waitingForMove = False
                }
                -- all rolls used up, move on
                else GameState 
                { players = players state
                , activePlayer = nextPlayer state activeP
                , rollsAllowed = 3
                , roll = rollResult
                , waitingForMove = False
                }

{-- MOVE FUNCTIONS --}

movePlayer :: GameState -> DiceRoll -> String -> GameState
movePlayer state rollResult fromField = 
    GameState 
    { players = capturePiece state updatedPlayer target
    , activePlayer = nextPlayer state activeP
    , rollsAllowed = determineRolls state
    , roll = rollResult
    , waitingForMove = False
    }
    where
        activeP = activePlayer state
        target = newField activeP fromField rollResult
        updatedPlayer = 
            Player 
            { colour = colour activeP
            , inHouse = inHouse activeP
            , inGoal = inGoal activeP
            , occupiedFields = move fromField target (occupiedFields activeP)
            , startField = startField activeP
            , finalField = finalField activeP
            , mustLeaveStart = False 
            }

handleMoveRequest :: GameState -> FieldId -> GameState
handleMoveRequest state fieldId = movePlayer state (roll state) fieldId

putPieceOnBoard :: GameState -> GameState
putPieceOnBoard state
    | arePiecesInHouse = modifiedState
    | otherwise = state
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
            { players = capturePiece state updatedPlayer (startField updatedPlayer)
            , activePlayer = updatedPlayer
            , rollsAllowed = 1
            , roll = 6 -- always a six that makes you go aboard
            , waitingForMove = False
            }

{-- UTILITY FUNCTIONS --}

capturePiece :: GameState -> Player -> FieldId -> Map.Map Colour Player
capturePiece state capturer fieldId = case capturedOccupier of 
    Just occupier ->
        let m1 = Map.adjust (\_ -> occupier) (readColour (colour occupier)) (players state)
        in Map.adjust (\_ -> capturer) (readColour (colour capturer)) m1
    Nothing -> Map.adjust (\_ -> capturer) (readColour (colour capturer)) (players state)
    where 
        fieldOccupier = fieldOccupiedBy state fieldId
        capturedOccupier = case fieldOccupier of
            Just player ->  Just $ Player
                { colour = colour player
                , inHouse = inHouse player + 1
                , inGoal = inGoal player
                , occupiedFields = delete fieldId (occupiedFields player)
                , startField = startField player
                , finalField = finalField player
                , mustLeaveStart = mustLeaveStart player
                }
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

nothingOnBoard :: Player -> Bool
nothingOnBoard player = null $ occupiedFields player

nothingInHouse :: Player -> Bool
nothingInHouse player = inHouse player <= 0

nothingInGoal :: Player -> Bool
nothingInGoal player = inGoal player <= 0

readColour :: String -> Colour
readColour "red" = Red
readColour "blue" = Blue
readColour "green" = Green
readColour _ = Yellow

nextPlayer :: GameState -> Player -> Player
nextPlayer state player = fromJust $ Map.lookup nextCol $ players state
    where nextCol = succ $ readColour $ colour player

newField :: Player -> FieldId -> Int -> FieldId
newField player fieldId steps 
    | isPastFinalField player fieldId = fieldId
    | otherwise = show $ (read fieldId + steps) `mod` 40 -- modulo for wraparound

move :: FieldId -> FieldId -> [FieldId] -> [FieldId]
move fromField toField fieldList = toField : delete fromField fieldList

isPastFinalField :: Player -> FieldId -> Bool
isPastFinalField player fieldId = idNr >= finalFieldNr -- TODO fix this, ugly
    where
        idNr = toInt fieldId
        finalFieldNr = toInt $ finalField player

toInt :: String -> Int
toInt str = read str + 0

exceedsGoal :: Player -> FieldId -> Int -> Bool
exceedsGoal player fieldId occupiedGoalFields = idNr > finalFieldNr + 4 - occupiedGoalFields
    where
        idNr = read fieldId
        finalFieldNr = read $ finalField player

determineGoalOccupation :: Player -> Int
determineGoalOccupation player = foldl1 max goalCells
    where
        goalFields = map (T.pack) $ filter (\fieldId -> "goal" `isInfixOf` fieldId) (occupiedFields player)
        goalCells = map (\s -> read (T.unpack (T.splitOn "-" s !! 1))) (goalFields)

rollDie :: Int 
rollDie = unsafePerformIO $ randomRIO (1, 6)

getPlayerByIndex :: Int -> Map.Map Colour Player -> Player
getPlayerByIndex 1 playerMap = fromJust $ Map.lookup Red $ playerMap
getPlayerByIndex 2 playerMap = fromJust $ Map.lookup Blue $ playerMap
getPlayerByIndex 3 playerMap = fromJust $ Map.lookup Green $ playerMap
getPlayerByIndex _ playerMap = fromJust $ Map.lookup Yellow $ playerMap

