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
  _itemAction :: Action
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

type MenuM = StateT AppState IO
type MenuWithBack = MenuM ()
-- type ConfirmAction = Action -> MenuM ()

modifyMenu :: (Menu -> Menu) -> MenuM ()
modifyMenu f = currentMenu %= f

addMenuItem :: String -> Action -> MenuM ()
addMenuItem label action' = do
  modifyMenu $ \menu ->
    menu & menuItems %~ (++ [MenuItem label action'])

addBackMenuM :: Menu -> MenuM Menu
addBackMenuM previousMenu = do
  let backLabel = "Back to: " ++ (previousMenu ^. menuName)
  addMenuItem backLabel (Action (currentMenu .= previousMenu))
  use currentMenu

menuWithBack :: MenuM Menu -> Menu -> MenuWithBack
menuWithBack createMenu previousMenu = do
  _ <- createMenu
  _ <- addBackMenuM previousMenu
  menu <- use currentMenu
  currentMenu .= menu

confirmAction :: String -> Action -> Menu -> MenuWithBack
confirmAction message originalAction = menuWithBack confirmMenu
  where
    confirmMenu :: MenuM Menu
    confirmMenu = do
      textArea .= Just message
      let confirmMenu' = Menu "Confirm" [ MenuItem "Yes" originalAction]
      currentMenu .= confirmMenu'
      return confirmMenu'

menuItemWithBack :: String -> MenuM Menu -> Menu -> MenuItem
menuItemWithBack label createMenu previousMenu =
  MenuItem label (Action (menuWithBack createMenu previousMenu))

menuItemWithConfirm :: String -> Action -> Menu -> MenuItem
menuItemWithConfirm label action' previousMenu =
  MenuItem label (Action (confirmAction message action' previousMenu))
    where message = "Are you sure you want to select " ++ label ++ "?"

-- Action sequencer
infixl 4 >->
(>->) :: Action -> Action -> Action
a1 >-> a2 = Action $ do
  _action a1
  _action a2

newGame :: Game
newGame = Game "noname" 30 (vertex "Starting Room")

-- helper function to set main text
displayText :: String -> Action
displayText text = Action $ textArea .= Just text

setMenu :: Menu -> Action
setMenu menu = Action (currentMenu .= menu)

helpMenu :: MenuM Menu
helpMenu = do
  textArea .= Just "This is the help text, should be rendering if the Help Menu was selected"
  let menu = Menu "Help"
             [ MenuItem "How to use menus"      (displayText "Menus are simple to use, all you need to do is press the corresponding numbers on your keyboard.")
             , MenuItem "How to quit"           (displayText "Quitting is easy! Just go the main menu and select 'Quit'")
             , MenuItem "Multiline String Test" (displayText "This is a test of multiline strings\nThis should be on line 2\nand this on line 3") ]
  currentMenu .= menu
  return menu

testAction :: Action
testAction = Action $ currentGame . playerAge += 1

getAge :: Action
getAge = Action $ do
  game <- use currentGame
  textArea .= Just ("Age: " ++ show (game ^. playerAge))

statsMenu :: MenuM Menu
statsMenu = do
  let menu = Menu "Stats" [ MenuItem "Player Stats" getAge]
  currentMenu .= menu
  return menu

initNewGame :: Action
initNewGame = Action (currentGame .= newGame)

mainMenu :: Menu
mainMenu = Menu "Main"
  [ menuItemWithBack "Help" helpMenu mainMenu
  , menuItemWithConfirm "New Game" (initNewGame >-> setMenu mainMenu >-> displayText "A new game has been created") mainMenu
  , MenuItem "Increment age" (testAction >-> displayText "Age Incremented by 1")
  , menuItemWithBack "Stats" statsMenu mainMenu
  , menuItemWithConfirm "Quit"(Action $ quit .= True) mainMenu ]
  -- , MenuItem "Quit"          (Action $ confirmAction "Are you sure you want to quit?" (Action $ quit .= True) mainMenu) ]

runGameLoop :: StateT AppState IO ()
runGameLoop = do
  quitFlag <- use quit
  unless quitFlag $ do

    -- Get state
    flash <- use notice
    textArea' <- use textArea
    menu <- use currentMenu
    let items = menu ^. menuItems

    -- Render the view
    liftIO $ do
      clearScreen
      putStrLn $ renderMessage flash
      putStrLn "------------------------"
      forM_ textArea' $ \str -> do
        putStrLn str
        putStrLn "------------------------"
      putStrLn $ "Menu: " ++ menu ^. menuName
      putStr $ ifoldMap (\i item -> show (i + 1) ++ ". " ++ (item ^. itemLabel) ++ "\n") items

    -- Handle input / run action
    c <- liftIO getCharNoEcho
    case readMaybe [c] :: Maybe Int of
      Just i -> do
        case items ^? ix (i - 1) :: Maybe MenuItem of
          Just selectedItem -> do
            textArea .= Nothing
            _action (selectedItem ^. itemAction)
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
