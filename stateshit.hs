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

data Alignment  = Horizontal | Vertical
data FieldPart  = Top | Bottom
data Colour     = Red | Blue | Green | Yellow
data Goal       = TopGoal | BottomGoal | LeftGoal | RightGoal

instance Show Colour where
    show c = case c of
        Red -> "red"
        Blue -> "blue"
        Green -> "green"
        Yellow -> "yellow"


cell :: String -> String ->  Widget 
cell id classes = 
    toWidget [hamlet|
        <div class="cell #{classes}" id="#{id}""> 
    |]

colouredCell :: String -> Colour -> String -> Widget
colouredCell id colour classes = cell id $ (show colour) ++ " " ++ classes

houseCell :: String -> Colour -> Widget
houseCell id colour = 
    colouredCell ("house-" ++ (idString id colour)) colour classes
    where
        classes = "house-cell " ++ (show colour) ++ "-house-cell"

idString :: String -> Colour -> String
idString x colour = (show colour) ++ "-" ++ x

twinlinkCell :: String -> String -> Alignment -> Widget
twinlinkCell id classes alignment = case alignment of
    Horizontal  -> cellRow twinlinked 
    Vertical    -> cellColumn twinlinked
    where
        twinlinked = do
            cellLink alignment
            cell id classes
            cellLink alignment

cellLink :: Alignment -> Widget
cellLink Horizontal = 
    toWidget [hamlet| 
        <div .cell-link-horizontal>
            <div .cell-link-top-div>
            <div .cell-link-bottom-div> 
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

sidebar :: Widget
sidebar = do
    let button = partRow $ toWidget [whamlet|
        <button .rollbutton>Roll the die!
    |]
    let die = partRow $ toWidget [whamlet|
        <div .die>
    |]
    let turn = partRow $ toWidget [whamlet|
        <div .turn>
    |]
    sidebarPart $ turn >> button >> die


cellgroup :: [String] -> String -> Alignment -> Maybe Widget -> Maybe Widget -> Widget
cellgroup ids classes linkdir topLink bottomLink = case (linkdir, topLink, bottomLink) of
    (Horizontal, Just topL, Just bottomL)   -> cellRow $ topL >> linkedCells >> bottomL
    (Horizontal, Just topL, Nothing)        -> cellRow $ topL >> linkedCells
    (Horizontal, Nothing,   Just bottomL)   -> cellRow $ linkedCells >> bottomL
    (Horizontal, Nothing,   Nothing)        -> cellRow linkedCells
    (Vertical,   Just topL, Just bottomL)   -> cellColumn $ topL >> linkedCells >> bottomL
    (Vertical,   Just topL, Nothing)        -> cellColumn $ topL >> linkedCells
    (Vertical,   Nothing,   Just bottomL)   -> cellColumn $ linkedCells >> bottomL
    (Vertical,   Nothing,   Nothing)        -> cellColumn linkedCells
    where   
        cellsAndIds = intersperse (cellLink linkdir) $ map (\id -> cell id classes) ids
        linkedCells = foldl1 (>>) cellsAndIds

fieldPart :: Widget -> Widget
fieldPart content = flexContainer content "field-part"

fieldPartHorizontal :: Widget -> Widget
fieldPartHorizontal content = flexContainer content "field-part-horizontal"

fieldPartWrapper :: Widget -> Widget
fieldPartWrapper content = flexContainer content "field-part-wrapper"

sectionWrapper :: Widget -> Widget
sectionWrapper content = flexContainer content "section-wrapper"

sidebarPart :: Widget -> Widget
sidebarPart content = flexContainer content "field-part sidebar" 

partRow :: Widget -> Widget
partRow content = flexContainer content "part-row"

goalRow :: Widget -> Widget 
goalRow content = flexContainer content "part-row goal-row"

house :: Int -> Colour -> FieldPart -> Widget
house id colour fieldpart = case fieldpart of
    Top -> fieldPart topHouseContainer 
    Bottom -> fieldPart bottomHouseContainer
    where
        toprow = partRow $ houseCell (show id) colour >> houseCell (show (id + 1)) colour
        botrow = partRow $ houseCell (show (id + 2)) colour >> houseCell (show (id + 3)) colour
        topHouseContainer = flexContainer (toprow >> botrow) "house house-top"
        bottomHouseContainer = flexContainer (toprow >> botrow) "house house-bottom"

playingField :: Widget -> Widget
playingField content = flexContainer content "playing-field"

slogan :: Widget -> Widget
slogan s = flexContainer s "slogan"

goal :: Int -> Colour -> Goal -> Widget
goal id colour goalType = case goalType of
    TopGoal      -> goalRow $ partRow $ partialCellgroup Vertical (Just verticalLink) Nothing
    BottomGoal   -> goalRow $ partRow $ partialCellgroup Vertical Nothing (Just verticalLink)
    LeftGoal     -> goalRow $ partRow $ partialCellgroup Horizontal (Just horizontalLink) Nothing
    RightGoal    -> goalRow $ partRow $ partialCellgroup Horizontal Nothing (Just horizontalLink)
    where
        id1 = "goal-" ++ idString (show id) colour
        id2 = "goal-" ++ idString (show (id + 1)) colour
        id3 = "goal-" ++ idString (show (id + 2)) colour
        id4 = "goal-" ++ idString (show (id + 3)) colour
        partialCellgroup = cellgroup [id1, id2, id3, id4] $ (show colour) ++ "-goal-cell"


topGoal :: Widget
topGoal = do
    let link = partRow $ twinlinkCell "10" "" Horizontal
    let g = goal 5 Blue TopGoal
    fieldPart $ link >> g

bottomGoal :: Widget
bottomGoal = do
    let link = partRow $ twinlinkCell "30" "" Horizontal
    let g = goal 13 Yellow BottomGoal
    fieldPart $ g >> link


leftGoal :: Widget
leftGoal = do
    let link = partRow $ twinlinkCell "40" "" Vertical
    let g = goal 1 Red LeftGoal
    fieldPartHorizontal $ link >> g

rightGoal :: Widget
rightGoal = do
    let link = partRow $ twinlinkCell "20" "" Vertical
    let g = goal 9 Green RightGoal
    fieldPartHorizontal $ g >> link

centre :: Widget
centre = toWidget [whamlet|
    <div .centre>
|]

topLeft :: Widget 
topLeft = do
    let h = house 1 Red Top
    let s = slogan [whamlet|<h1>Ná|]
    let horizcells = partRow $ cellgroup ["1", "2", "3", "4", "5"] "" Horizontal Nothing Nothing
    let vertcells = cellgroup ["6", "7", "8", "9"] "" Vertical Nothing (Just verticalLink)
    let row1 = partRow $ h >> s >> vertcells
    fieldPart $ row1 >> horizcells


topRight :: Widget
topRight = do
    let h = house 5 Blue Top
    let s = slogan [whamlet|<h1>bíodh|]
    let horizcells = partRow $ cellgroup ["15", "16", "17", "18", "19"] "" Horizontal Nothing Nothing
    let vertcells = cellgroup ["11", "12", "13", "14"] "" Vertical Nothing (Just verticalLink)
    let row1 = partRow $ vertcells >> s >> h
    fieldPart $ row1 >> horizcells


bottomRight :: Widget
bottomRight = do
    let h = house 9 Green Bottom
    let s = slogan [whamlet|<h1>ort!|]
    let horizcells = partRow $ cellgroup ["21", "22", "23", "24", "25"] "" Horizontal Nothing Nothing
    let vertcells = cellgroup ["26", "27", "28", "29"] "" Vertical (Just verticalLink) Nothing
    let row1 = partRow $ vertcells >> s >> h
    fieldPart $ horizcells >> row1

bottomLeft :: Widget
bottomLeft = do
    let h = house 13 Yellow Bottom
    let s = slogan [whamlet|<h1>fearg|]
    let horizcells = partRow $ cellgroup ["35", "36", "37", "38", "39"] "" Horizontal Nothing Nothing
    let vertcells = cellgroup ["31", "32", "33", "34"] "" Vertical (Just verticalLink) Nothing
    let row1 = partRow $ h >> s >> vertcells
    fieldPart $ horizcells >> row1




getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
    toWidget $(luciusFile "widgets.lucius")
    let topSide = fieldPartWrapper $ topLeft >> topGoal >> topRight
    let middle = fieldPartWrapper $ leftGoal >> centre >> rightGoal
    let bottomSide = fieldPartWrapper $ bottomLeft >> bottomGoal >> bottomRight
    let mainField = sectionWrapper $ topSide >> middle >> bottomSide
    let sidebarField = sectionWrapper sidebar
    playingField $ mainField >> sidebarField
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

