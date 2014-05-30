
module Rachel.Evaluation (
    -- * Global Rachel State
    RachelState (..)
  , CodeLine (..)
  , initialRachelState
  , RachelM
  , runRachelM
  , rachelError
  , rachelIO
  , rachelTalk
  , modifyEnv, getEnv
  , getSource, setSource
  , setVerbosity
  , lookForId
    -- * Expression evaluation
  , rachelEval
    -- * Loading
  , loadEntity
  , loadPrimitives
  , loadDec
  ) where

import Rachel.Types
import Rachel.Primitive
import Rachel.Inference
import Rachel.Transformation

import Control.Applicative
import Control.Monad (when)
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Error
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map
import System.IO.Unsafe

data CodeLine =
  CodeLine { clCode :: String   -- ^ Code in this line.
           , clFile :: FilePath -- ^ File from where the code comes from.
           , clLine :: Int      -- ^ Line number.
             }

data RachelState = RachelSt
  { -- | The 'Environment' with the current state of the compilation.
    rachelEnv :: Environment
    -- | The remainding lines of source code.
  , sourceCode :: [CodeLine]
    -- | Level of verbosity. A value of /0/ indicates silence.
  , verbosityLevel :: Int
    }

initialRachelState :: RachelState
initialRachelState = RachelSt { rachelEnv = Map.empty , sourceCode = [] , verbosityLevel = 0 }

type RachelM = StateT RachelState (ErrorT String IO)

runRachelM :: RachelM a -> IO (Either String (a,RachelState))
runRachelM r = runErrorT $ runStateT r initialRachelState

rachelError :: String -> RachelM a
rachelError = lift . throwError

rachelIO :: IO a -> RachelM a
rachelIO = lift . lift

-- | Print a 'String' in the screen (using 'putStrLn') if the verbosity level is
--   equal or greater than the given number.
rachelTalk :: Int -> String -> RachelM ()
rachelTalk n str = do
  vl <- verbosityLevel <$> get
  when (vl >= n) $ rachelIO $ putStrLn str

modifyEnv :: (Environment -> Environment) -> RachelM ()
modifyEnv f = modify $ \st -> st { rachelEnv = f $ rachelEnv st }

getEnv :: RachelM Environment
getEnv = rachelEnv <$> get

getSource :: RachelM [CodeLine]
getSource = sourceCode <$> get

setSource :: [CodeLine] -> RachelM ()
setSource ls = modify $ \st -> st { sourceCode = ls }

setVerbosity :: Int -> RachelM ()
setVerbosity n = modify $ \st -> st { verbosityLevel = n }

lookForId :: Id -> RachelM (Maybe Entity)
lookForId x = do
  e <- getEnv
  return $ case Map.lookup x e of
    Nothing -> Nothing
    Just (t,f,v) -> Just $ Entity x t f v

-- | Evaluates a expression within the Rachel scope.
--   It doesn't modify the Rachel state.
rachelEval :: Exp -> RachelM Value
rachelEval (EReal r) = return $ VReal r
rachelEval (EInteger i) = return $ VInteger i
rachelEval (EVar x) = do
  vm <- rachelEnv <$> get
  case Map.lookup x vm of
    Nothing -> rachelError $ "Undefined variable `" ++ x ++ "`."
    Just v -> return $ (\(_,_,a) -> a) v
rachelEval (EProd x y) = VProd <$> rachelEval x <*> rachelEval y
rachelEval ESumL = return $ VFun VSumL
rachelEval ESumR = return $ VFun VSumR
rachelEval (EApp f x) = do
  vf <- rachelEval f
  case vf of
    VBottom str -> return $ VBottom str
    VFun g -> g <$> rachelEval x
    _ -> rachelError $ "Unexpected value `" ++ pretty f ++ "`. Expected function."
rachelEval (ELambda x e) = do
  st <- get
  return $ VFun $ \vx ->
   -- Locally assign the value vx to x.
   -- The type is not used during evaluation, so anything works.
   let r = unsafePerformIO $ runErrorT $ evalStateT (loadEntity (Entity x undefined defaultFixity vx) >> rachelEval e) st
   in  case r of
         Left err -> VBottom err
         Right v -> v
rachelEval (ELet x e1 e2) = do
  v1 <- rachelEval e1
  st <- get
  -- Locally assign the value v1 to x.
  -- The type is not used during evaluation, so anything works.
  let r = unsafePerformIO $ runErrorT $ evalStateT (loadEntity (Entity x undefined defaultFixity v1) >> rachelEval e2) st
  case r of
    Left err -> return $ VBottom err
    Right v -> return v

loadEntity :: Entity -> RachelM ()
loadEntity = modifyEnv . insertEntity

loadPrimitives :: RachelM ()
loadPrimitives = mapM_ loadEntity allPrimitives

-- | Load a declaration. Compiling a Rachel program is nothing else but apply 'loadDec' to
--   every parsed declaration. The behavior of each declaration is as follows.
--
--   * 'TypeDec': If the identity is undefined, it creates one with the given type, 'defaultFixity',
--                and 'VBottom' value. If it is defined, it is overrided only keeping the fixity.
--
--   * 'FunDec': If the expression does not type-check, an error is thrown. Otherwise, if the
--               identity is undefined, it's added with the type and value of the given expression and
--               'defaultFixity'. If the identity /is/ defined but it has 'VBottom' value, this means
--               that its type has been declared and the value will be overrided if and only if the
--               type of the expression and the type declared unify. However, if the identity /is/
--               defined but it already has a non-bottom value, its definition will be overrided
--               and the type and value of the given expression will be applied. Although the fixity
--               will be kept.
--
--  * 'InfixDec': If the identity is undefined, it is defined with type 'TAny', 'VBottom' value and
--                the given fixity. Otherwise, it will just change the fixity of the identity.
loadDec :: Dec -> RachelM ()
loadDec (TypeDec i t) = do
  e <- getEnv
  case Map.lookup i e of
    Nothing -> loadEntity $ Entity i t defaultFixity $ VBottom $ "Identity `" ++ i ++ "` does not have a value yet."
    -- Declaring a type of an already defined fuction removes its value.
    -- However, it inherits the fixity.
    Just (_,f,_) -> loadEntity $ Entity i t f $ VBottom $ "Identity `" ++ i ++ "` does not have a value yet."
loadDec (FunDec i ex0) = do
  e <- getEnv
  let c = contextFromEnv e
      ex = asFixPoint i ex0
  case typeOf c ex of
    Left err -> rachelError $ "Expression `" ++ pretty ex ++ "` does not type check. " ++ err
    Right t -> do
      v <- rachelEval ex
      case Map.lookup i e of
        Nothing -> loadEntity $ Entity i t defaultFixity v
        Just (t0,f,v0) ->
          case v0 of
            VBottom _ ->
              case unify t t0 of
                Left _ -> rachelError $ "Identity `" ++ i ++ "` was declared to have type `" ++ pretty t0
                                     ++ "`, but its definition has type `" ++ pretty t ++ "`."
                Right _ -> loadEntity $ Entity i t0 f v
            _ -> loadEntity $ Entity i t f v
loadDec (InfixDec i f) = do
  e <- getEnv
  case Map.lookup i e of
    Nothing -> loadEntity $ Entity i (TVar "a") f $ VBottom $ "Identity `" ++ i ++ "` does not have value yet."
    Just (t,_,v) -> loadEntity $ Entity i t f v
loadDec (PatDec i n xs) =
  loadDec $ FunDec i $
     -- q_ = realDiv patternLength n
     ELet "q_" (EVar "realDiv" `EApp` EVar "patternLength" `EApp` EReal (RReal $ fromIntegral n)) $
       foldr1 (\r1 r2 -> EVar "soundAdd" `EApp` r1 `EApp` r2)
         $ fmap (foldr (\(c,s0) s ->  EVar "addAt"
                              `EApp` (EVar "realProd" `EApp` EVar "q_" `EApp` EReal (RReal $ fromIntegral c))
                              `EApp` s0
                              `EApp` s
                         ) $ EVar "zeroSound" `EApp` EVar "patternLength"
                  ) $ fmap (zip [0..] . fmap EVar) xs
