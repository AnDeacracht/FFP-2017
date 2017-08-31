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
    [ (Red, Player "red" 4 0 [] "1" False)
    , (Blue, Player "blue" 4 0 [] "11" False)
    , (Green, Player "green" 4 0 [] "21" False)
    , (Yellow, Player "yellow" 4 0 [] "31" False)
    ]

initialState =  
    GameState 
    { players = allPlayers
    , turn = getRandomPlayer $ unsafePerformIO $ randomRIO (1, 4)
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
    { players = Map.adjust (\_ -> updatedPlayer) currCol (players state)
    , turn = nextPlayer state (turn state)
    , rollsAllowed = determineRolls state
    , roll = rollResult
    , waitingForMove = False
    }
    where
        currCol = readColour $ colour $ turn state
        updatedPlayer = 
            Player 
            { colour = colour playerToModify
            , inHouse = inHouse playerToModify
            , inGoal = inGoal playerToModify
            , occupiedFields = move fromField (newField fromField rollResult) (occupiedFields playerToModify)
            , startField = startField playerToModify
            , mustLeaveStart = False 
            }

handleMoveRequest :: GameState -> FieldId -> GameState
handleMoveRequest state fieldId = movePlayer state (turn state) (roll state) fieldId

putPieceOnBoard :: GameState -> Player -> GameState
putPieceOnBoard state playerToModify = if arePiecesInHouse then modifiedState else state
    where
        currCol = readColour $ colour $ turn state
        arePiecesInHouse = (inHouse playerToModify) > 0
        updatedPlayer = Player 
            { colour = colour playerToModify
            , inHouse = (inHouse playerToModify) - 1
            , inGoal = inGoal playerToModify
            , occupiedFields = (startField playerToModify) : (occupiedFields playerToModify)
            , startField = startField playerToModify
            , mustLeaveStart = True 
            }
        modifiedState = GameState 
            { players = Map.adjust (\_ -> updatedPlayer) currCol (players state)
            , turn = updatedPlayer
            , rollsAllowed = 1
            , roll = 6 -- always a six that makes you go aboard
            , waitingForMove = False
            }

{-- UTILITY FUNCTIONS --}

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
newField fieldId steps = show $ (read fieldId) + steps -- TODO fails with goal fields for now

move :: FieldId -> FieldId -> [FieldId] -> [FieldId]
move fromField toField fieldList = toField : delete fromField fieldList

rollDie :: Int 
rollDie = unsafePerformIO $ randomRIO (1, 6)

getRandomPlayer :: Int -> Player
getRandomPlayer 1 = fromJust $ Map.lookup Red $ allPlayers
getRandomPlayer 2 = fromJust $ Map.lookup Blue $ allPlayers
getRandomPlayer 3 = fromJust $ Map.lookup Green $ allPlayers
getRandomPlayer 4 = fromJust $ Map.lookup Yellow $ allPlayers

