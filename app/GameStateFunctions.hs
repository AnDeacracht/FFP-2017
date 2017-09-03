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
import MoveFunctions
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
    , rollsAllowed = 3
    , roll = 0
    , waitingForMove = False
    }

{-- THE ROLL HANDLER --}

-- Goes active once the player rolls. If the player rolls a 6, action needs to be taken immediately
-- as the player may get no choice which piece they want to move.

handleRoll :: GameState -> DiceRoll -> GameState
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
        if cannotMove (activePlayer state) rollResult -- check here if the player has rolled too high to move etc
            then GameState 
                { players = players state
                , activePlayer = nextPlayer state activeP
                , rollsAllowed = determineRolls state
                , roll = roll state + rollResult
                , waitingForMove = False
                }
            else GameState 
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

-- deprec 
newField :: Player -> FieldId -> Int -> Maybe FieldId
newField player fieldId steps 
    | isPastFinalField player targetId = Nothing
    | isInvalidEnterGoalMove player targetId = Nothing
    | fieldOccupiedBySelf player targetId = Nothing
    | otherwise = Just $ show $ target `mod` 40 -- modulo for wraparound
    where
        target = toInt fieldId + steps
        targetId = show target
--deprec end




movePlayer :: GameState -> DiceRoll -> String -> GameState
movePlayer state rollResult startFieldId
    | isGoalField startFieldId = GameState 
        { players = Map.adjust (\_ -> updatedPlayerInGoal) (readColour (colour activeP)) (players state)
        , activePlayer = nextPlayer state activeP
        , rollsAllowed = determineRolls state
        , roll = rollResult
        , waitingForMove = False
        }
    | otherwise = GameState 
        { players = targetCapture
        , activePlayer = nextPlayer state activeP
        , rollsAllowed = determineRolls state
        , roll = rollResult
        , waitingForMove = False
        }
    where
        activeP = activePlayer state
        target = newField activeP startFieldId rollResult
        targetCapture = case target of
            Just t -> capturePiece state updatedPlayer t
            Nothing -> players state
        targetMove = move activeP startFieldId target
        finalFields = case targetMove of 
            Just fields -> fields
            Nothing -> occupiedFields activeP 
        updatedPlayer = 
            Player 
            { colour = colour activeP
            , inHouse = inHouse activeP
            , inGoal = inGoal activeP
            , occupiedFields = finalFields
            , startField = startField activeP
            , finalField = finalField activeP
            , mustLeaveStart = False 
            }

        -- move inside goal
            
        goalTarget = newGoalField activeP startFieldId rollResult
        goalTargetMove = move activeP startFieldId goalTarget
        finalGoalFields = case goalTargetMove of
            Just fields -> fields
            Nothing -> occupiedFields activeP
        updatedPlayerInGoal =
            Player
            { colour = colour activeP
            , inHouse = inHouse activeP
            , inGoal = inGoal activeP
            , occupiedFields = finalGoalFields
            , startField = startField activeP
            , finalField = finalField activeP
            , mustLeaveStart = False 
            }

handleMoveRequest :: GameState -> FieldId -> GameState
handleMoveRequest state fieldId
    | isNothing (newField activeP fieldId rollResult) = state  -- ignore the move command and let the player issue another one - client can complain
    | otherwise = movePlayer state rollResult fieldId
    where
        rollResult = roll state
        activeP = activePlayer state

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



-- for moving inside the goal, where skipping pieces is not allowed
newGoalField :: Player -> FieldId -> Int -> Maybe FieldId
newGoalField player fieldId steps 
    | targetFieldExceeds = Nothing
    | targetFieldOccupied = Nothing
    | isSkipNeeded player fieldId $ show targetField = Nothing
    | otherwise = Just $ makeGoalCellNumber player targetField
    where 
        startField = extractGoalCellNumber fieldId
        targetField = startField - steps -- since goal cells are numbered from 4 to 1
        targetFieldExceeds = targetField < 1
        targetFieldAsGoalCell = makeGoalCellNumber player targetField
        targetFieldOccupied = targetFieldAsGoalCell `elem` (occupiedFields player)


move :: Player -> FieldId -> Maybe FieldId -> Maybe [FieldId]
move player fromField maybeToField = do
    target <- maybeToField
    return $ target : delete fromField (occupiedFields player)



cannotMove :: Player -> Int -> Bool
cannotMove player rollResult = all isNothing possibleMovers
    where
        fields = occupiedFields player
        possibleMovers = map (\field -> newField player field rollResult) fields


-- Goal fields are numbered 4 to 1, starting with the field closest to the entrance.
-- To determine how far into the goal a player gets, subtract the steps they have left at the entrance from 5.
-- Thus, if you have 4 steps left, you'll get to the top field, while with only 1 step you'll make the first field. 

goIntoGoal :: Player -> DiceRoll -> FieldId -> Player
goIntoGoal player rollResult fromField = 
    Player 
    { colour = colour player
    , inHouse = inHouse player
    , inGoal = inGoal player + 1 
    , occupiedFields = occupiedGoalField : (occupiedFields player)
    , startField = startField player
    , finalField = finalField player
    , mustLeaveStart = mustLeaveStart player
    }
    where
        (finalFieldAsInt, fieldIdAsInt) = ((toInt $ finalField player), toInt fromField)
        -- final field is 40, you're at 38 and rolled a 4 ---> 4 - 2 = 2 steps left inside the goal
        stepsLeftAtGoal = rollResult - (finalFieldAsInt - fieldIdAsInt)
        arrivalField = show $ 5 - stepsLeftAtGoal
        occupiedGoalField = "goal-" ++ arrivalField ++ "-" ++ (colour player) 
