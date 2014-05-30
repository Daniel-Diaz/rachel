
import Rachel.Types
import Rachel.Compilation
import Rachel.Evaluation

import Data.Sound
import Data.Sound.WAVE

import Control.Applicative ((<$>))
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import System.Environment (getArgs)

import qualified Data.Map as M

data ArgMode = InputA | VerbosityA | OutputA
               deriving (Eq,Enum)

argName :: ArgMode -> String
argName InputA = "i"
argName OutputA = "o"
argName VerbosityA = "v"

data CompState = CompState
  { outputFile :: FilePath
  , inputFiles :: [FilePath]
  , verbosityArg :: Int
  , currentMode :: ArgMode
    }

type Args = StateT CompState IO

processArgs :: [String] -> Args ()
processArgs = mapM_ processArg

processArg :: String -> Args ()
processArg ('-':str) =
  case filter ((==str) . argName) [InputA ..] of
    []  -> liftIO $ putStrLn $ "Unrecognized flag: " ++ str
    [m] -> modify $ \st -> st { currentMode = m }
    _   -> liftIO $ putStrLn $ "BUG! Flag has more than one meaning: " ++ str
processArg str = do
  m <- currentMode <$> get
  case m of
    InputA ->
      modify $ \st -> st { inputFiles = inputFiles st ++ [str] }
    VerbosityA ->
      modify $ \st -> st { verbosityArg = read str , currentMode = InputA }
    OutputA ->
      modify $ \st -> st { outputFile = str , currentMode = InputA }

main :: IO ()
main = do
  st <- execStateT (liftIO getArgs >>= processArgs) $
          -- Initial compilation state
          CompState "default.wav" [] 0 InputA
  r <- runRachelM $ do
          -- Rachel compilation
          setVerbosity $ verbosityArg st
          loadPrimitives
          loadFile "res/prelude.rach"
          mapM_ loadFile $ inputFiles st
          compile
  case r of
    Left err -> putStrLn err
    Right (_,rst) ->
      case M.lookup "main" $ rachelEnv rst of
        Nothing -> putStrLn "Entity 'main' is not defined."
        Just (t,_,v) ->
          case t of
            TSound ->
              case fromValue v of
                Nothing -> putStrLn "Entity 'main' contains a bottom value."
                Just (RSound s) -> encodeFile (outputFile st) $ fromSound 16 s
            _ -> putStrLn $ "Entity 'main' has incorrect type: " ++ pretty t
