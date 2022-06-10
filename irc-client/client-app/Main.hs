{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
module Main
    ( main
    ) where

import Brick.Main as Main
import Brick.Widgets.Core as C
import Brick.Types as T
import Brick.AttrMap as A
import Brick.Widgets.Border as B
import Brick.Util as U
import Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Brick.Focus as F

import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Control.Lens.Lens
import Control.Lens.TH
import Control.Lens.Setter
import Control.Lens.Getter

import Control.Monad as M
import Data.Text as Text
import qualified Data.Vector as V
import Data.Void (Void)
--------------------------------------------------------------------------------
--import           Control.Concurrent  (forkIO)
--import           Control.Monad       (forever, unless)
--import           Control.Monad.Trans (liftIO)
--import           Network.Socket      (withSocketsDo)
--import           Data.Text           (Text)
--import qualified Data.Text           as T
--import qualified Data.Text.IO        as T
--import qualified Network.WebSockets  as WS
--import           Requests
--import           Data.Aeson


------------------------------------------------------------------------------
--app :: WS.ClientApp ()
--app conn = do
    --putStrLn "Connected!"

    --let connect = encode $ Login "Scoot"
    --WS.sendTextData conn connect

    -- Fork a thread that writes WS data to stdout
    --_ <- forkIO $ forever $ do
        --msg <- WS.receiveData conn
        --liftIO $ T.putStrLn msg

    --Read from stdin and write to WS
    --let loop = do
            --line <- T.getLine
            --case line of
                --"1" -> let createRoomJson = encode $ CreateRoom "Room1" in
                       --WS.sendTextData conn createRoomJson >> loop
                --_ -> loop
    --         unless (T.null line) $ WS.sendTextData conn line >> loop

    --loop
    --WS.sendClose conn ("Bye!" :: Text)


------------------------------------------------------------------------------
--main :: IO ()
--main = withSocketsDo $ WS.runClient "127.0.0.1" 8765 "/" app

data ResourceNames = ChatWindow | InputText | UserList
  deriving (Eq, Ord, Show)

chatWindowList :: UIState -> Widget ResourceNames
chatWindowList state =
  let
    gList = L.list ChatWindow (V.fromList $ chatWindowTxt state) 1
  in
    B.border $ L.renderList
                  (\b e -> txt e) False gList

inputEditor :: UIState -> E.Editor Text ResourceNames
inputEditor state = E.editorText InputText (Just 1) (inputTxt state)

inputWidget :: UIState -> Widget ResourceNames
inputWidget state = B.border $ E.renderEditor (txt . mconcat) True $ inputEditor state

usersWidget :: UIState -> Widget ResourceNames
usersWidget state = 
  let
    uList = L.list UserList (V.fromList $ memberListTxt state) 1
  in
    B.border $ L.renderList (\b e -> txt e) False uList

drawUI :: UIState -> [T.Widget ResourceNames]
drawUI state = return $ ((chatWindowList state) <=> (inputWidget state)) <+> (usersWidget state)

handleEvent :: UIState -> T.BrickEvent ResourceNames Void -> T.EventM ResourceNames (T.Next UIState)
handleEvent s (VtyEvent (EvKey (KChar x) [])) = Main.continue $ s { inputTxt = Text.snoc (inputTxt s) x }
handleEvent s (VtyEvent (EvKey KEnter [])) = Main.continue $ s { inputTxt = mempty, chatWindowTxt = (inputTxt s):(chatWindowTxt s) }
handleEvent s (VtyEvent (EvKey KEsc [])) = Main.halt s
handleEvent s _ = Main.continue s

theMap :: A.AttrMap
theMap = A.attrMap defAttr []

chooseCursor :: UIState -> [CursorLocation ResourceNames] -> Maybe (CursorLocation ResourceNames)
chooseCursor s [x] = Just x
chooseCursor s _ = Nothing

app :: Main.App UIState Void ResourceNames
app = 
  App { appDraw = drawUI
      , appChooseCursor = chooseCursor
      , appHandleEvent = handleEvent
      , appStartEvent = return
      , appAttrMap = const theMap
      }

data UIState = UIState { 
     inputTxt :: Text
    ,chatWindowTxt :: [Text]
    ,memberListTxt :: [Text]
  }

myState = UIState {
     inputTxt = mempty
    ,chatWindowTxt = []
    ,memberListTxt = []
  }

main :: IO ()
main = do
  void $ Main.defaultMain app myState
