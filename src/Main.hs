{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Algebra.Graph.Undirected
import           Control.Lens
import           Control.Monad.State      (StateT, evalStateT, liftIO)
import           System.Console.ANSI      (clearScreen, hideCursor, showCursor)
import           System.IO                (BufferMode (LineBuffering, NoBuffering),
                                           hSetBuffering, hSetEcho, stdin)
import           Text.Read                (readEither, readMaybe)

getCharNoEcho :: IO Char
getCharNoEcho = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  hSetBuffering stdin LineBuffering
  hSetEcho stdin True
  return c

data Message = Info String | Alert String | Plain String deriving (Show)

newtype MenuAction = MenuAction { _action :: StateT AppState IO () }

type MenuItem = (Menu, Maybe MenuAction)

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
  _currentGame :: Game
}
makeLenses ''Menu
makeLenses ''Game
makeLenses ''AppState
makeLenses ''MenuAction


mainMenu :: Menu
mainMenu = Menu "Main" [ (helpMenu, Nothing), (mainMenu, Nothing) ]

helpMenu :: Menu
helpMenu = Menu "Help" [ (mainMenu, Nothing) ]

-- parseInput :: String -> Either String Int
-- parseInput s = case readEither s of
--   Just i -> Right i
--   Nothing -> Left "Invalid input"

renderMessage :: Message -> String
renderMessage = \case
  Info  str -> "🛈 " ++ str ++ "  "
  Alert str -> "⚠ " ++ str ++ "  "
  Plain str -> str

runGameLoop :: StateT AppState IO ()
runGameLoop = do
  menu <- use currentMenu
  flash <- use notice
  game <- use currentGame
  let items = menu ^. menuItems

  -- Render the view
  liftIO $ do
    clearScreen
    putStrLn $ renderMessage flash
    putStrLn $ "Menu: " ++ menu ^. menuName
    putStr $ ifoldMap (\i item -> show (i + 1) ++ ". " ++ (_menuName . fst) item ++ "\n") items

  c <- liftIO getCharNoEcho
  case readMaybe [c] :: Maybe Int of
    Just i -> do
      case items ^? ix (i - 1) of
        Just (menu', Nothing) -> do
          currentMenu .= menu'
          notice .= Plain ("You selected " ++ show i ++ ". " ++ menu' ^. menuName )
        Just (menu', Just action) -> return ()
          -- not sure how to run a menu action

        Nothing -> notice .= Info ("No menu item for " ++ show i)
    Nothing -> notice .= Alert "Invalid input"

  runGameLoop

main :: IO ()
main = do
  hideCursor
  evalStateT runGameLoop $ AppState (Info "Welcome") mainMenu $ Game "noname" 30 $ vertex "test"
  showCursor
