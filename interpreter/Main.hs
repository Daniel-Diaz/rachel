
module Main (
    main
  ) where

import Rachel.Types
import Rachel.Inference
import Rachel.Evaluation
import Rachel.Compilation
import Rachel.Doc
import Rachel.Parser

import qualified Data.Map as Map
import Text.Parsec
import Control.Monad (void,unless,when)
import System.Environment (getArgs)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class (lift)
import Control.Applicative

import System.Console.Haskeline
import System.Console.Haskeline.History (addHistoryRemovingAllDupes,historyLines)
import qualified System.Console.Haskeline.MonadException as E

type RacheliM = InputT RachelM

inputSettings :: Settings RachelM
inputSettings = defaultSettings { autoAddHistory = False }

runRacheliM :: RacheliM a -> RachelM a
runRacheliM = runInputT inputSettings

racheliCatch :: (String -> RacheliM a) -> RacheliM a -> RacheliM a
racheliCatch f m = do
  h <- getHistory
  let g e = putHistory h >> f e
  lift $ rachelCatch (runRacheliM . g) (runRacheliM $ putHistory h >> m)

-- Commands

data Command =
    ImportModule FilePath
  | ShowEnvironment
  | ShowIdentity Id
  | UnloadIdentity Id
  | LoadIdentity Id Exp
  | Quit

-- | Perform a 'Command'.
perform :: Command -> RachelM ()
perform (ImportModule fp) = do
  loadFile fp
  compile
  rachelIO $ putStrLn "OK. Module compiled."
perform ShowEnvironment = printEnvironment
perform (ShowIdentity x) = do
  e <- getEnv
  case Map.lookup x e of
    Nothing -> rachelIO $ putStrLn $ "Identity `" ++ x ++ "` is not defined."
    Just (t,f,v) -> rachelIO $ putStr $ showEntity $ Entity x t f v
perform (UnloadIdentity x) = do
  e <- getEnv
  case Map.lookup x e of
    Nothing -> rachelIO $ putStrLn $ "Identity `" ++ x ++ "` is not defined."
    Just _  -> do modifyEnv $ Map.delete x
                  rachelIO $ putStrLn $ "Identity `" ++ x ++ "` deleted."
perform Quit = rachelError quitMessage
perform (LoadIdentity x e) = loadDec $ FunDec x e
  
quitMessage :: String
quitMessage = "Quit."

-- Command parsing

p_Command :: RachelP Command
p_Command = choice
  [ p_ImportModule , p_ShowEnvironment , p_ShowIdentity
  , p_UnloadIdentity , p_Quit , p_LoadIdentity
    ]

p_ImportModule :: RachelP Command
p_ImportModule = do
  _ <- char 'i'
  spaces1
  ImportModule <$> many1 anyChar

p_ShowEnvironment :: RachelP Command
p_ShowEnvironment = char 'e' >> return ShowEnvironment

p_ShowIdentity :: RachelP Command
p_ShowIdentity = do
  char 's' >> spaces1
  ShowIdentity <$> p_Ident

p_UnloadIdentity :: RachelP Command
p_UnloadIdentity = do
  _ <- char 'u'
  spaces1
  UnloadIdentity <$> p_Ident

p_Quit :: RachelP Command
p_Quit = char 'q' >> return Quit

p_LoadIdentity :: RachelP Command
p_LoadIdentity = do
  char 'l' >> spaces1
  FunDec i e <- p_FunDec
  return $ LoadIdentity i e

-- MAIN

initialize :: [FilePath] -> RachelM ()
initialize xs = do
  setVerbosity 1
  rachelIO $ putStrLn "Loading primitives..."
  loadPrimitives
  rachelIO $ putStrLn "Loading prelude..."
  loadFile "res/prelude.rach"
  compile
  unless (null xs) $
    mapM_ (\x -> do rachelIO $ putStrLn $ "Loading file `" ++ x ++ "`..."
                    loadFile x
                    compile) xs

rachelInput :: RacheliM String
rachelInput = maybe ":e" id <$> getInputLine "*> "

rachelCatch :: (String -> RachelM a) -> RachelM a -> RachelM a
rachelCatch f m = do
  s0 <- get
  (a,s) <- lift $ runStateT m s0 `catchError` (\e -> runStateT (f e) s0)
  put s
  return a

infixr 0 <?

(<?) :: RachelM a -> (String -> RachelM a) -> RachelM a
(<?) = flip rachelCatch

processInput :: String -> RachelM ()
processInput str
  | all (==' ') str = return ()
  | head str == ':' = void $ runRachelP (witheof p_Command) (tail str) >>= perform
  | otherwise = do
      x <- runRachelP (witheof p_Exp) str
      perform $ LoadIdentity "_it" x
      me <- lookForId "_it"
      case me of
        Nothing -> rachelError "For some reason, the `it` variable could not be found."
        Just (Entity _ t f v) -> do
          rachelIO $ putStrLn $ pretty v ++ " : " ++ pretty t
          modifyEnv $ Map.delete "_it"
          loadEntity $ Entity "it" t f v

rachelLoop :: RacheliM ()
rachelLoop = do
  str <- rachelInput
  modifyHistory $ addHistoryRemovingAllDupes str
  b <- racheliCatch (\e -> do
    let b = e == quitMessage
    unless b $ outputStrLn e
    return b
    ) $ lift (processInput str) >> return False
  unless b rachelLoop

main :: IO ()
main = do
  welcomeMessage
  xs <- getArgs
  r <- runRachelM $ runRacheliM $ do 
         lift $ initialize xs
         rachelLoop
  case r of
    Left err -> putStrLn err
    Right _ -> putStrLn "racheli closed."

welcomeMessage :: IO ()
welcomeMessage = putStr $ unlines [
    "Welcome to racheli, the Rachel interpreter."
  ]
