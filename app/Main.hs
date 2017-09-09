{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

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
import Database.Persist
import Database.Persist.TH
import qualified Database.Persist.Sqlite as DB
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)


import GameStateFunctions
import GameState
import DataDeclarations
import Widgets
import TestStuff


mkYesod "NaBiodhFeargOrt" [parseRoutes|
/                           HomeR       GET
/roll                       RollR       GET
/init                       InitR       GET
/move/#String               MoveR       GET
/question/#QuestionId       QuestionR   GET
|]

instance Yesod NaBiodhFeargOrt
instance YesodPersist NaBiodhFeargOrt where
    type YesodPersistBackend NaBiodhFeargOrt = DB.SqlBackend
    runDB action = do
        NaBiodhFeargOrt _ pool <- getYesod
        DB.runSqlPool action pool

-- der Dialog zur Userinteraktion konnte nicht mehr vollendet werden... die Persistenz funktioniert allerdings

getQuestionR :: QuestionId -> Handler Html
getQuestionR questionId = do
    q <- runDB $ selectList [QuestionId ==. questionId] []
    defaultLayout [whamlet|
        $forall Entity qid qu <- q
            <h1 .question>#{questionQuestionText qu}
            <div .answers-container>
                <div .answer>
                    <p .answertext>#{questionAnswer1 qu}
                <div .answer>
                    <p .answertext>#{questionAnswer2 qu}
                <div .answer>
                    <p .answertext>#{questionAnswer3 qu}
                <div .answer>
                    <p .answertext>#{questionAnswer4 qu}
    |]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Ná bíodh fearg ort!"
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
    addScriptRemote "https://code.jquery.com/ui/1.12.1/jquery-ui.min.js"
    addStylesheetRemote "https://code.jquery.com/ui/1.12.1/themes/base/jquery-ui.css"
    toWidget $(juliusFile "../Game.julius")
    toWidget $(luciusFileReload "../Game.lucius")
    let topSide = fieldPartWrapper $ topLeft >> topGoal >> topRight
    let middle = fieldPartWrapper $ leftGoal >> centre >> rightGoal
    let bottomSide = fieldPartWrapper $ bottomLeft >> bottomGoal >> bottomRight
    let mainField = sectionWrapper $ topSide >> middle >> bottomSide
    let sidebarField = sectionWrapper sidebar
    playingField $ mainField >> sidebarField >> dialogue

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
    runStderrLoggingT $ DB.withSqlitePool "ceisteanna.db3" 5 $ \pool -> liftIO $ do
        runResourceT $ flip DB.runSqlPool pool $ do
            DB.runMigration migrateAll 
            mapM (DB.insert) questions -- QuizQuestion "Tá ocras..." "orm" "agam" "dom" "fúm"
        warp 3000 NaBiodhFeargOrt { gameState = startState, persistence = pool }
    