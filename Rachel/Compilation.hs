
module Rachel.Compilation (
    getDec
  , loadFile
  , compile
  ) where

import Rachel.Types
import Rachel.Evaluation
import Rachel.Parser
import Rachel.Transformation

import Data.Char (isAlpha)
import Control.Applicative
import Control.Monad (when)
import System.Directory

isDecHead :: String -> Bool
isDecHead str = not (null str) && (isAlpha (head str) || head str == '(' || head str == '#')

isNegligible :: String -> Bool
isNegligible = all (==' ')

-- | Get a new declaration string from the source code, if there is any.
getDecStr :: RachelM (Maybe String)
getDecStr = do
  ls <- getSource
  let g (x:xs)
        | isNegligible (clCode x) = setSource xs >> return []
        | isDecHead (clCode x) = setSource (x:xs) >> return []
        | otherwise = (clCode x:) <$> g xs
      g [] = setSource [] >> return []
      f (x:xs)
        | isNegligible (clCode x) = f xs
        | isDecHead (clCode x) = Just . (clCode x:) <$> g xs
        | otherwise = rachelError $ show (clLine x) ++ "@" ++ clFile x ++ ": Declaration header expected "
                                 ++ "but a meaningless line was found."
      f [] = return Nothing
  fmap unwords <$> f ls

-- | Get a new declaration from the source code, if there is any.
--   Otherwise, 'getDec' returns 'Nothing'.
getDec :: RachelM (Maybe Dec)
getDec = do
  mstr <- getDecStr
  case mstr of
    Nothing -> return Nothing
    Just str -> Just <$> runRachelP p_Dec str

-- | Perform a compilation step, loading the next declaration.
--   If a declaration was found, 'True' is returned.
stepCompilation :: RachelM Bool
stepCompilation = do
  md <- getDec
  case md of
    Nothing -> return False
    Just d -> loadDec d >> return True

loadFile :: FilePath -> RachelM ()
loadFile fp = do
  b <- rachelIO $ doesFileExist fp
  if b then do str <- rachelIO $ removeComments <$> readFile fp
               let ls = (\(c,n) -> CodeLine c fp n) <$> zip (lines str) [1..]
               getSource >>= setSource . (++ls)
       else rachelError $ "File `" ++ fp ++ "` does not exist."
  
-- | Compile the currently available source code.
compile :: RachelM ()
compile = do
  b <- stepCompilation
  when b compile
