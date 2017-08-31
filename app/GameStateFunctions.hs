module GameStateFunctions where

import System.Random
import System.IO.Unsafe
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import DataDeclarations

{-- INITIAL STATE --}

allPlayers = 
    Map.fromList 
    [ (Red, Player "red" 4 0 [] "1" "40" False)
    , (Blue, Player "blue" 4 0 [] "11" "10" False)
    , (Green, Player "green" 4 0 [] "21" "20" False)
    , (Yellow, Player "yellow" 4 0 [] "31" "30" False)
    ]

initialState =  
    GameState 
    { players = allPlayers
    , turn = getPlayerByIndex $ unsafePerformIO $ randomRIO (1, 4)
    , rollsAllowed = 3
    , roll = 0
    , waitingForMove = False
    }

testPlayers =
    Map.fromList 
    [ (Red, Player "red" 4 0 [] "1" "40" False)
    , (Blue, Player "blue" 3 0 ["1"] "11" "10" False)
    , (Green, Player "green" 3 0 ["40"] "21" "20" False)
    , (Yellow, Player "yellow" 4 0 [] "31" "30" False)
    ]

testState = 
    GameState 
    { players = testPlayers
    , turn = getPlayerByIndex 3
    , rollsAllowed = 3
    , roll = 0
    , waitingForMove = False
    }
{-- THE ROLL HANDLER --}

handleRoll :: GameState -> DiceRoll -> GameState
-- if the player rolls a 6, take action immediately.
handleRoll state 6
    | nothingOnBoard activePlayer = putPieceOnBoard state activePlayer -- nothing on board, put piece on it
    | nothingInHouse activePlayer = -- nothing left in house, reroll
        GameState
        { players = players state
        , turn = turn state
        , rollsAllowed = 1 -- you must reroll if you have a 6
        , roll = roll state + 6
        , waitingForMove = False
        }
    | otherwise = checkStartField
    where
        activePlayer = turn state
        checkStartField = -- check start field - if something is there, is needs to move on
            if (startField activePlayer) `elem` (occupiedFields activePlayer) 
                then movePlayer state activePlayer 6 (startField activePlayer) -- move away immediately
                else putPieceOnBoard state activePlayer -- put new piece on board

-- if the player rolls anything else
handleRoll state rollResult
    | nothingOnBoard activePlayer = checkRollCount
    | mustLeaveStart activePlayer = -- move immediately, you get no choice
        movePlayer state activePlayer rollResult (startField activePlayer) 
    | otherwise = -- if you needn't vacate the start field, wait for user input
        GameState 
        { players = players state
        , turn = turn state
        , rollsAllowed = 0 -- no reroll
        , roll = roll state + rollResult
        , waitingForMove = True -- wait for move command
        }
    where
        activePlayer = turn state
        checkRollCount =
            -- still more than one roll allowed
            if (rollsAllowed state) > 1
                then GameState 
                { players = players state
                , turn = activePlayer
                , rollsAllowed = (rollsAllowed state) - 1
                , roll = rollResult
                , waitingForMove = False
                }
                -- all rolls used up, move on
                else GameState 
                { players = players state
                , turn = nextPlayer state activePlayer
                , rollsAllowed = 3
                , roll = rollResult
                , waitingForMove = False
                }

{-- MOVE FUNCTIONS --}

movePlayer :: GameState -> Player -> DiceRoll -> String -> GameState
movePlayer state playerToModify rollResult fromField = 
    GameState 
    { players = capturePiece state updatedPlayer target
    , turn = nextPlayer state (turn state)
    , rollsAllowed = determineRolls state
    , roll = rollResult
    , waitingForMove = False
    }
    where
        currCol = readColour $ colour $ turn state
        target = newField fromField rollResult
        updatedPlayer = 
            Player 
            { colour = colour playerToModify
            , inHouse = inHouse playerToModify
            , inGoal = inGoal playerToModify
            , occupiedFields = move fromField target (occupiedFields playerToModify)
            , startField = startField playerToModify
            , finalField = finalField playerToModify
            , mustLeaveStart = False 
            }

handleMoveRequest :: GameState -> FieldId -> GameState
handleMoveRequest state fieldId = movePlayer state (turn state) (roll state) fieldId

putPieceOnBoard :: GameState -> Player -> GameState
putPieceOnBoard state playerToModify
    | arePiecesInHouse = modifiedState
    | otherwise = state
    where
        currCol = readColour $ colour $ turn state
        sField = startField playerToModify
        arePiecesInHouse = (inHouse playerToModify) > 0
        startFieldOccupier = fieldOccupiedBy state sField
        updatedPlayer = Player 
            { colour = colour playerToModify
            , inHouse = inHouse playerToModify - 1
            , inGoal = inGoal playerToModify
            , occupiedFields = (startField playerToModify) : (occupiedFields playerToModify)
            , startField = startField playerToModify
            , finalField = finalField playerToModify
            , mustLeaveStart = True 
            }
        modifiedState = GameState 
            { players = capturePiece state updatedPlayer (startField updatedPlayer)
            , turn = updatedPlayer
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
fieldOccupiedBy state id = case occupier of
    []  -> Nothing
    [p] -> Just p
    where occupier = filter (\p -> id `elem` (occupiedFields p)) $ Map.elems (players state)

determineRolls :: GameState -> Int -- three rolls if house empty and board empty
determineRolls state
    | (boardEmpty) && (goalEmpty) = 3
    | otherwise = 1
    --TODO half-empty goal with gaps
    where 
        nextUp = nextPlayer state (turn state)
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
readColour "yellow" = Yellow

nextPlayer :: GameState -> Player -> Player
nextPlayer state player = fromJust $ Map.lookup nextCol $ players state
    where nextCol = succ $ readColour $ colour player

newField :: FieldId -> Int -> FieldId
newField fieldId steps = show $ (read fieldId + steps) `mod` 40 -- TODO fails with goal fields for now

move :: FieldId -> FieldId -> [FieldId] -> [FieldId]
move fromField toField fieldList = toField : delete fromField fieldList

rollDie :: Int 
rollDie = unsafePerformIO $ randomRIO (1, 6)

getPlayerByIndex :: Int -> Player
getPlayerByIndex 1 = fromJust $ Map.lookup Red $ allPlayers
getPlayerByIndex 2 = fromJust $ Map.lookup Blue $ allPlayers
getPlayerByIndex 3 = fromJust $ Map.lookup Green $ allPlayers
getPlayerByIndex 4 = fromJust $ Map.lookup Yellow $ allPlayers

