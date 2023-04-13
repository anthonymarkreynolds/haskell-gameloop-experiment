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
  _textArea    :: Maybe String,
  _currentMenu :: Menu,
  _currentGame :: Game,
  _quit        :: Bool
}
makeLenses ''Menu
makeLenses ''MenuItem
makeLenses ''Game
makeLenses ''AppState
makeLenses ''Action

backMenu :: Menu -> Menu -> Menu
backMenu currentMenu' nextMenu' = nextMenu' & menuItems %~ (++ [prevMenuItem])
  where
    prevMenuItem = MenuItem ("Back to: " ++ (currentMenu' ^. menuName)) (Just currentMenu') Nothing

newGame :: Game
newGame = Game "noname" 30 (vertex "Starting Room")

quitMenuItem :: MenuItem
quitMenuItem = MenuItem "Quit" Nothing (Just (Action (quit .= True)))

renderHelp :: Action
renderHelp = Action $ textArea .= Just "This is the help text, should be rendering if the Help Menu was selected"

menuInfo :: String
menuInfo = "Menus are simple to use, all you need to do is press the corresponding numbers on your keyboard."

multilineStr :: String
multilineStr =
  "This is a test of multiline strings\nThis should be on line 2\nand this on line 3"

helpMenu :: Menu
helpMenu = Menu "Help"
  [ MenuItem "How to use menus" Nothing (Just (Action (textArea .= Just menuInfo)))
  , MenuItem "How to quit"      Nothing (Just (Action (textArea .= Just "Quitting is easy! Just go the main menu and select 'Quit'")))
  , MenuItem "Multiline String Test" Nothing (Just (Action (textArea .= Just multilineStr)))]

testAction :: Action
testAction = Action $ currentGame . playerAge += 1

getAge :: Action
getAge = Action $ do
  game <- use currentGame
  textArea .= Just ("Age: " ++ show (game ^. playerAge))

statsMenu :: Menu
statsMenu = Menu "Stats"
  [ MenuItem "Inventory" Nothing Nothing
  , MenuItem "Player Stats" Nothing (Just getAge)]

mainMenu :: Menu
mainMenu = Menu "Main"
  [ MenuItem "Help"          (Just $ backMenu mainMenu helpMenu) (Just renderHelp)
  , MenuItem "New Game"      Nothing        (Just (Action $ currentGame .= newGame))
  , MenuItem "Increment age" Nothing        (Just testAction)
  , MenuItem "Stats"         (Just $ backMenu mainMenu statsMenu) Nothing
  , quitMenuItem ]


runGameLoop :: StateT AppState IO ()
runGameLoop = do
  quitFlag <- use quit
  unless quitFlag $ do

    -- Get state
    displayText <- use textArea
    menu <- use currentMenu
    flash <- use notice
    game <- use currentGame
    let items = menu ^. menuItems

    -- Render the view
    liftIO $ do
      clearScreen
      putStrLn $ renderMessage flash
      putStrLn "------------------------"
      forM_ displayText $ \str -> do
        putStrLn str
        putStrLn "------------------------"
      -- putStrLn $ "Age: " ++ show ((currentGame . playerAge) ^. appState)
      putStrLn $ "Menu: " ++ menu ^. menuName
      putStr $ ifoldMap (\i item -> show (i + 1) ++ ". " ++ (item ^. itemLabel) ++ "\n") items


    -- Handle input / run action
    c <- liftIO getCharNoEcho
    case readMaybe [c] :: Maybe Int of
      Just i -> do
        case items ^? ix (i - 1) :: Maybe MenuItem of
          Just selectedItem -> do
            textArea .= Nothing
            forM_ (selectedItem ^. nextMenu) (currentMenu .=)
            forM_ (selectedItem ^. itemAction) _action
            notice .= Plain ("You selected " ++ show i ++ ". " ++ selectedItem ^. itemLabel)
          Nothing -> notice .= Info ("No menu item for " ++ show i)
      Nothing -> notice .= Alert "Invalid input"

    runGameLoop

initAppState :: AppState
initAppState = AppState (Info "Welcome") Nothing  mainMenu newGame False

main :: IO ()
main = do
  hideCursor
  evalStateT runGameLoop initAppState
  showCursor
  clearScreen
  putStrLn "Thanks for playing, goodbye!"
