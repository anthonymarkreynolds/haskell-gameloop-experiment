{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Algebra.Graph.Undirected
import           Control.Lens
import           Control.Monad.State      (StateT, evalStateT, forM_, liftIO,
                                           unless)
import           System.Console.ANSI      (clearScreen, hideCursor, showCursor)
import           System.IO                (BufferMode (LineBuffering, NoBuffering),
                                           hSetBuffering, hSetEcho, stdin)
import           Text.Read                (readMaybe)

getCharNoEcho :: IO Char
getCharNoEcho = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  return c

data Message
  = Info String
  | Alert String
  | Plain String
  deriving (Show)

renderMessage :: Message -> String
renderMessage (Info str)  = "🛈 " ++ str ++ "  "
renderMessage (Alert str) = "⚠ " ++ str ++ "  "
renderMessage (Plain str) = str

newtype Action = Action { _action :: StateT AppState IO () }

data MenuItem = MenuItem {
  _itemLabel  :: String,
  _nextMenu   :: Maybe Menu,
  _itemAction :: Maybe Action
}

data Menu = Menu {
  _menuName  :: String,
  _menuItems :: [MenuItem]
}

type Room = String
data Game = Game {
  _playerName :: String,
  _playerAge  :: Int,
  _roomGraph  :: Graph Room
}

data AppState = AppState {
  _notice      :: Message,
  _currentMenu :: Menu,
  _currentGame :: Game,
  _quit        :: Bool
}
makeLenses ''Menu
makeLenses ''MenuItem
makeLenses ''Game
makeLenses ''AppState
makeLenses ''Action

quitMenuItem :: MenuItem
quitMenuItem = MenuItem "Quit" Nothing (Just (Action (quit .= True)))

testAction :: Action
testAction = Action $ currentGame . playerAge += 1

mainMenu :: Menu
mainMenu = Menu "Main"
  [ MenuItem "Help Menu"     (Just helpMenu) Nothing
  , MenuItem "Increment age" Nothing         (Just testAction)
  , quitMenuItem ]

helpMenu :: Menu
helpMenu = Menu "Help" [ MenuItem "Main Menu" (Just mainMenu) Nothing ]

runGameLoop :: StateT AppState IO ()
runGameLoop = do
  quitFlag <- use quit
  unless quitFlag $ do
    menu <- use currentMenu
    flash <- use notice
    game <- use currentGame
    let items = menu ^. menuItems

    -- Render the view
    liftIO $ do
      clearScreen
      putStrLn $ "Age: " ++ show (game ^. playerAge)
      putStrLn $ renderMessage flash
      putStrLn $ "Menu: " ++ menu ^. menuName
      putStr $ ifoldMap (\i item -> show (i + 1) ++ ". " ++ (item ^. itemLabel) ++ "\n") items

    c <- liftIO getCharNoEcho
    case readMaybe [c] :: Maybe Int of
      Just i -> do
        case items ^? ix (i - 1) :: Maybe MenuItem of
          Just selectedItem -> do
            forM_ (selectedItem ^. nextMenu) (currentMenu .=)
            forM_ (selectedItem ^. itemAction) _action
            notice .=  Plain ("You selected " ++ show i ++ ". " ++ selectedItem ^. itemLabel)
          Nothing -> notice .= Info ("No menu item for " ++ show i)
      Nothing -> notice .= Alert "Invalid input"

    runGameLoop

main :: IO ()
main = do
  hideCursor
  evalStateT runGameLoop $ AppState (Info "Welcome")  mainMenu (Game "noname" 30 (vertex "test")) False
  showCursor
  clearScreen
  putStrLn "Thanks for playing, goodbye!"
