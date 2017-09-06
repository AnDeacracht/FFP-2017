{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE DeriveGeneric         #-}

module Main where

import Yesod
import Text.Hamlet
import Text.Lucius
import Text.Julius
import qualified Data.Text as T
import GHC.Generics
import Data.List
import Data.Aeson
import Data.Maybe
import Debug.Trace
import Control.Concurrent.MVar
import System.Random
import System.IO.Unsafe

import GameStateFunctions
import GameState
import DataDeclarations
import Widgets

mkYesod "NaBiodhFeargOrt" [parseRoutes|
/               HomeR      GET
/roll           RollR      GET
/init           InitR      GET
/move/#String   MoveR      GET
|]

instance Yesod NaBiodhFeargOrt

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Ná bíodh fearg ort!"
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
    toWidget $(juliusFileReload "../Game.julius")
    toWidget $(luciusFileReload "../Game.lucius")
    let topSide = fieldPartWrapper $ topLeft >> topGoal >> topRight
    let middle = fieldPartWrapper $ leftGoal >> centre >> rightGoal
    let bottomSide = fieldPartWrapper $ bottomLeft >> bottomGoal >> bottomRight
    let mainField = sectionWrapper $ topSide >> middle >> bottomSide
    let sidebarField = sectionWrapper sidebar
    playingField $ mainField >> sidebarField

getMoveR :: String -> Handler Value
getMoveR fromField = do
    foundation <- getYesod
    let gameStateVar = gameState foundation -- MVar GameState
    gameState <- liftIO $ readMVar $ gameStateVar -- GameState
    let newState = handleMoveRequest gameState fromField (currentRoll gameState)
    lift $ putStrLn "NEW STATE AFTER MOVE"
    lift $ print newState
    _ <- lift $ swapMVar gameStateVar newState
    returnJson $ toJSON newState

getRollR :: Handler Value
getRollR = do
    foundation <- getYesod
    let gameStateVar = gameState foundation -- MVar GameState
    gameState <- liftIO $ readMVar $ gameStateVar -- GameState
    rollResult <- lift $ randomRIO (1,6)
    let newState = handleRoll gameState rollResult
    lift $ print rollResult
    lift $ putStrLn "NEW STATE AFTER ROLL"
    lift $ print newState
    _ <- lift $ swapMVar gameStateVar newState
    returnJson $ toJSON newState

getInitR :: Handler Value
getInitR = do
    foundation <- getYesod
    gameState <- liftIO $ readMVar $ gameState foundation -- GameState
    returnJson $ toJSON gameState

main :: IO ()
main = do 
    startState <- newMVar $ initialState
    warp 3000 NaBiodhFeargOrt { gameState = startState }
    