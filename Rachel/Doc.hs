
module Rachel.Doc (
    primitivesReport
  , showEntity
  , printEnvironment
  ) where

import Rachel.Types
import Rachel.Primitive
import Rachel.Evaluation

import qualified Data.Map as Map
import Data.Char (isLetter)

primitivesReport :: String
primitivesReport = unlines $ fmap showEntity allPrimitives

showEntity :: Entity -> String
showEntity (Entity i t f v) = unlines
  [ m i ++ " : " ++ pretty t
  , m i ++ " = " ++ pretty v
  , pretty f
    ]
  where
    m str
      | null str  = "#no identifier#"
      | otherwise = let x = head str
                    in  if isLetter x || x == '_' 
                           then i
                           else "(" ++ i ++ ")"

printEnvironment :: RachelM ()
printEnvironment = do
  e <- getEnv
  let pr (i,(t,f,v)) = rachelIO $ putStrLn $ showEntity $ Entity i t f v
  mapM_ pr $ Map.toList e

