{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import Yesod
import Control.Monad.State
import Text.Hamlet
import Text.Lucius
import Text.Julius
import Data.List
import Data.Aeson

data NaBiodhFeargOrt = NaBiodhFeargOrt

mkYesod "NaBiodhFeargOrt" [parseRoutes|
/ HomeR GET
|]

instance Yesod NaBiodhFeargOrt

data Alignment = Horizontal | Vertical
data FieldPart = Upper | Lower


cell :: String -> Widget 
cell x = 
    toWidget [hamlet|
        <div .cell ##{x}> 
    |]

houseCell :: Widget 
houseCell =
    toWidget [hamlet|
        <div .house-cell .cell> 
    |]

cellLink :: Alignment -> Widget
cellLink Horizontal = 
    toWidget [hamlet| 
        <div .cell-link-horizontal>
            <div .cell-link-upper-div>
            <div .cell-link-lower-div> 
    |]
    
cellLink Vertical = 
    toWidget [hamlet| 
        <div .cell-link-vertical>
                <div .cell-link-left-div>
                <div .cell-link-right-div> 
    |]

verticalLink :: Widget
verticalLink = cellLink Vertical

horizontalLink :: Widget
horizontalLink = cellLink Horizontal

twinlinkCell :: String -> Alignment -> Widget
twinlinkCell id alignment = case alignment of
    Horizontal -> cellRow twinlinked 
    Vertical -> cellColumn twinlinked
    where
        twinlinked = do
            cellLink alignment
            cell id
            cellLink alignment

cellContainer :: Alignment -> Widget -> Widget
cellContainer Horizontal content = 
    toWidget [whamlet|
        <section .cell-row-wrapper>
            ^{content}
    |]
cellContainer Vertical content =
    toWidget [whamlet|
        <section .cell-column-wrapper>
            ^{content}
    |]

cellRow :: Widget -> Widget
cellRow content = cellContainer Horizontal content

cellColumn :: Widget -> Widget 
cellColumn content = cellContainer Vertical content


flexContainer :: Widget -> String -> Widget
flexContainer content classes =
    toWidget [whamlet|
        <section class="#{classes}">
            ^{content}
    |]

cellgroup :: [String] -> Alignment -> Maybe Widget -> Maybe Widget -> Widget
cellgroup ids linkdir upperLink lowerLink = case (linkdir, upperLink, lowerLink) of
    (Horizontal, Just upl, Just lowl) -> cellRow $ upl >> linkedCells >> lowl
    (Horizontal, Just upl, Nothing) -> cellRow $ upl >> linkedCells
    (Horizontal, Nothing, Just lowl) -> cellRow $ linkedCells >> lowl
    (Horizontal, Nothing, Nothing) -> cellRow linkedCells
    (Vertical, Just upl, Just lowl) -> cellColumn $ upl >> linkedCells >> lowl
    (Vertical, Just upl, Nothing) -> cellColumn $ upl >> linkedCells
    (Vertical, Nothing, Just lowl) -> cellColumn $ linkedCells >> lowl
    (Vertical, Nothing, Nothing) -> cellColumn linkedCells
    where   
        cellsAndIds = intersperse (cellLink linkdir) $ map (\id -> cell id) ids
        linkedCells = foldl1 (>>) cellsAndIds
        

isolatedCells :: Int -> Widget
isolatedCells nr = foldl1 (>>) cells
    where
        cells = map (\_ -> houseCell) [1..nr]

fieldPart :: Widget -> Widget
fieldPart content = flexContainer content "field-part"

fieldPartWrapper :: Widget -> Widget
fieldPartWrapper content = flexContainer content "field-part-wrapper"

partRow :: Widget -> Widget
partRow content = flexContainer content "part-row"

goalRow :: Widget -> Widget 
goalRow content = flexContainer content "part-row goal-row"

house :: FieldPart -> Widget
house fieldpart = case fieldpart of
    Upper -> fieldPart upperHouseContainer 
    Lower -> fieldPart lowerHouseContainer
    where
        houseRow = partRow $ isolatedCells 2
        upperHouseContainer = flexContainer (houseRow >> houseRow) "house house-upper"
        lowerHouseContainer = flexContainer (houseRow >> houseRow) "house house-lower"

playingField :: Widget -> Widget
playingField content = flexContainer content "playing-field"

slogan :: Widget -> Widget
slogan s = flexContainer s "slogan"

upperLeft :: Widget 
upperLeft = do
    let h = house Upper
    let s = slogan [whamlet|<h1>Ná|]
    let horizcells = partRow $ cellgroup ["1", "2", "3", "4", "5"] Horizontal Nothing Nothing
    let vertcells = cellgroup ["6", "7", "8", "9"] Vertical Nothing (Just verticalLink)
    let row1 = partRow $ h >> s >> vertcells
    fieldPart $ row1 >> horizcells

upperGoal :: Widget
upperGoal = do
    let link = partRow $ twinlinkCell "10" Horizontal
    let goal = goalRow $ partRow $ cellgroup ["H-5", "H-6", "H-7", "H-8"] Vertical (Just verticalLink) Nothing
    fieldPart $ link >> goal


upperRight :: Widget
upperRight = do
    let h = house Upper
    let s = slogan [whamlet|<h1>bíodh|]
    let horizcells = partRow $ cellgroup ["15", "16", "17", "18", "19"] Horizontal Nothing Nothing
    let vertcells = cellgroup ["11", "12", "13", "14"] Vertical Nothing (Just verticalLink)
    let row1 = partRow $ vertcells >> s >> h
    fieldPart $ row1 >> horizcells

lowerRight :: Widget
lowerRight = do
    let h = house Lower
    let s = slogan [whamlet|<h1>ort|]
    let horizcells = partRow $ cellgroup ["21", "22", "23", "24", "25"] Horizontal Nothing Nothing
    let vertcells = cellgroup ["26", "27", "28", "29"] Vertical (Just verticalLink) Nothing
    let row1 = partRow $ vertcells >> s >> h
    fieldPart $ horizcells >> row1

lowerGoal :: Widget
lowerGoal = do
    let link = partRow $ twinlinkCell "30" Horizontal
    let goal = goalRow $ partRow $ cellgroup ["H-13", "H-14", "H-15", "H-16"] Vertical Nothing (Just verticalLink)
    fieldPart $ goal >> link


lowerLeft :: Widget
lowerLeft = do
    let h = house Lower
    let s = slogan [whamlet|<h1>fearg|]
    let horizcells = partRow $ cellgroup ["35", "36", "37", "38", "39"] Horizontal Nothing Nothing
    let vertcells = cellgroup ["31", "32", "33", "34"] Vertical (Just verticalLink) Nothing
    let row1 = partRow $ h >> s >> vertcells
    fieldPart $ horizcells >> row1

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    toWidget $(luciusFile "./widgets.lucius")
    let upperSide = fieldPartWrapper $ upperLeft >> upperGoal >> upperRight
    let lowerSide = fieldPartWrapper $ lowerLeft >> lowerGoal >> lowerRight
    playingField $ upperSide >> lowerSide
{--
    let xxx = cellgroup
    let name = "GURK" :: [Char]
    setTitle "SACKFRESSE"
    addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"
    let delta = toJSON name
    toWidget
        [julius|

            var gamestate = {
                red : 0,
                blue: 0
            }

            $(function() {

                $(".fieldcell").each(function(index) {
                    $(this).on("click", function() {
                        $("#teitel").html($(this).attr("id"))
                        gamestate.red += 1
                        gamestate.blue -= 1
                        console.log(gamestate)
                    });
                });

            });
        |]
    [whamlet| ^{widschet1}|]

--}

main :: IO ()
main = warp 3000 NaBiodhFeargOrt



{--------------------- NONYESOD SHITE ----------------------------}

type Stack = [Int]

pop :: State Stack Int  
pop = state $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = state $ \xs -> ((),a:xs)  


data StateThing = StateThing {
    shits :: Int,
    poops :: String,
    love :: Bool
} deriving (Show)

initState = StateThing 0 "" False

manipShit :: Int -> StateThing -> ((), StateThing)
manipShit newval statething = ((), StateThing newval (poops statething) (love statething))

manipShit2 :: Int -> State StateThing ()
manipShit2 newval = state $ \statething -> ((), StateThing newval (poops statething) (love statething))

manipPoops2 :: String -> State StateThing ()
manipPoops2 newval = state $ \statething -> ((), StateThing (shits statething) newval (love statething))

manipLove2 :: Bool -> State StateThing ()
manipLove2 newval = state $ \statething -> ((), StateThing (shits statething) (poops statething) newval)

dickery = do
    manipLove2 True
    manipShit2 1000
    manipPoops2 "I am bongo"

