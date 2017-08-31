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
import DataDeclarations
import Widgets

mkYesod "NaBiodhFeargOrt" [parseRoutes|
/        HomeR      GET
/roll    RollR      GET
/turn    TurnR      GET
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

{--getRollR :: Handler Html
getRollR = do
    rollRequest <- getRequest
    let roll = head $ reqGetParams rollRequest
    let rollValue = (\x -> T.unpack (snd x)) roll
    defaultLayout $ do
        toWidget [hamlet|<h1 .roll-value>#{rollValue}|]
--}
getRollR :: Handler Value
getRollR = do
    foundation <- getYesod
    let gameStateVar = gameState foundation -- MVar GameState
    gameState <- liftIO $ readMVar $ gameStateVar -- GameState
    rollResult <- lift $ randomRIO (1,6)
    lift $ print rollResult
    let newState = handleRoll gameState rollResult
    lift $ print newState
    _ <- lift $ swapMVar gameStateVar newState
    returnJson $ toJSON newState

getTurnR :: Handler T.Text
getTurnR = do
    foundation <- getYesod
    gameState <- liftIO $ readMVar $ gameState foundation -- GameState
    let currentTurn = turn gameState
    return $ T.pack $ show currentTurn


main :: IO ()
main = do 
    startState <- newMVar $ initialState
    --stateData <- readMVar initialState
    --print stateData
    warp 3000 NaBiodhFeargOrt { gameState = startState }
    