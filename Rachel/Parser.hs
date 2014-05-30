
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Rachel.Parser (
    -- * Parser type
    RachelP
  , OpTable
  , runRachelP
    -- * Parsers
  , p_Ident
  , p_Type
  , p_TypeDec
  , p_Exp
  , p_FunDec
  , p_InfixDec
  , p_Dec
    -- * Utils
  , spaces1
  , witheof
  ) where

import Rachel.Types
import Rachel.Evaluation

import Text.Parsec
import Text.Parsec.Expr

import Control.Applicative hiding (many,(<|>),optional)
import Control.Monad (replicateM,void)

import Data.Char (isLower,isSpace)
import Data.Map (Map)
import qualified Data.Map as Map

type OpTable = Map Int [(Id,OpAssoc)]

createOpTable :: RachelM OpTable
createOpTable = do
  e <- getEnv
  return $ foldr (uncurry $ Map.insertWith (++)) Map.empty
         $ (\(i,(_,Fixity a p,_)) -> (p,[(i,a)])) <$> Map.toList e

type RachelP = ParsecT String OpTable RachelM

addToOpTable :: Id -> Fixity -> RachelP ()
addToOpTable i (Fixity a p) = modifyState $ Map.insertWith (++) p [(i,a)]

readOpTable :: RachelP [[Operator String OpTable RachelM Exp]]
readOpTable = do
  m <- getState
  let g i = let t = if isOp i then id else withAccents
            in  t (string i) >> return (\x y -> EApp (EApp (EVar i) x) y)
      f (i,a) = Infix (try $ spaced $ g i <* notFollowedBy p_Op)
              $ if a == LeftA then AssocLeft else AssocRight
  return $ reverse $ ([Infix (try $ spaced $ fmap (\i x y -> EApp (EApp (EVar i) x) y) p_Op) AssocRight]:)
                   $ (\(_,as) -> fmap f as) <$> Map.toList m

runRachelP :: RachelP a -> String -> RachelM a
runRachelP p str = do
  t <- createOpTable
  r <- runParserT p t "source" str
  case r of
    Left err -> rachelError $ show err
    Right v -> return v

opSymbols :: String
opSymbols = "+-*/@#?&%$|^.<>="

resWords :: [String]
resWords = ["let","in"]

isOp :: Id -> Bool
isOp = not . isLower . head

p_AIdent :: RachelP Id
p_AIdent = do
  str <- (:) <$> satisfy isLower <*> many alphaNum
  if str `elem` resWords
     then fail $ str ++ " is a reserved word."
     else return str

p_OIdent :: RachelP Id
p_OIdent = many1 $ oneOf opSymbols

p_Ident :: RachelP Id
p_Ident = p_AIdent <|> withParens p_OIdent

p_Op :: RachelP Id
p_Op =  (char '`' *> p_AIdent <* char '`')
    <|> p_OIdent
    <?> "operator symbol"

spaced :: RachelP a -> RachelP a
spaced p = spaces *> p <* spaces

withParens :: RachelP a -> RachelP a
withParens p = char '(' *> p <* char ')'

withAccents :: RachelP a -> RachelP a
withAccents p = char '`' *> p <* char '`'

spaces1 :: RachelP String
spaces1 = many1 $ satisfy isSpace

spacedList1 :: RachelP a -> RachelP [a]
spacedList1 p = do
  x  <- p
  xs <- many $ try $ spaces1 >> p
  return $ x:xs

spacedList :: RachelP a -> RachelP [a]
spacedList p = try (spacedList1 p) <|> return []

typeOpTable :: [[Operator String OpTable RachelM Type]]
typeOpTable =
  [ [ Infix (try $ spaced $ char '*' >> return TProd) AssocRight ] -- a*b*c = a*(b*c)
  , [ Infix (try $ spaced $ char '+' >> return TSum) AssocRight ] -- a+b+c = a+(b+c)
  , [ Infix (try $ spaced $ string "->" >> return TFun) AssocRight ]
    ]

p_TypeUnit :: RachelP Type
p_TypeUnit = choice
 [ string "Real" >> return TReal
 , string "Integer" >> return TInteger
 , string "Bool" >> return TBool
 , string "Sound" >> return TSound
 , TVar <$> p_AIdent
 , withParens p_Type
   ]

p_Type :: RachelP Type
p_Type = buildExpressionParser typeOpTable p_TypeUnit

p_TypeDec :: RachelP Dec
p_TypeDec = do
  i <- p_Ident
  spaced $ char ':'
  t <- p_Type
  return $ TypeDec i t

p_Let :: RachelP Exp
p_Let = do
  string "let"
  spaces1
  i <- p_Ident
  spaces1
  as <- spacedList p_AIdent
  spaced $ char '='
  e1 <- p_Exp
  spaces1
  string "in"
  spaces1
  addToOpTable i defaultFixity
  e2 <- p_Exp
  return $ ELet i (foldr ELambda e1 as) e2

p_Lambda :: RachelP Exp
p_Lambda = do
  char '\\'
  spaces
  as <- spacedList p_AIdent
  spaced $ char '.'
  e <- p_Exp
  return $ foldr ELambda e as

p_Real :: RachelP RReal
p_Real = do
  d1 <- many1 digit
  char '.'
  d2 <- many1 digit
  return $ RReal $ read $ d1 ++ "." ++ d2

p_Integer :: RachelP RInteger
p_Integer = RInteger . read <$> many1 digit

p_Prod :: RachelP Exp
p_Prod = do
  char '('
  e1 <- p_Exp
  char ','
  e2 <- p_Exp
  char ')'
  return $ EProd e1 e2

p_Sum :: RachelP Exp
p_Sum = do
  c <- char 'L' <|> char 'R'
  case c of
    'L' -> return ESumL
    'R' -> return ESumR
    _ -> fail "This is impossible!"

p_Var :: RachelP Exp
p_Var = EVar <$> (p_AIdent <|> withParens p_OIdent)

p_ExpUnit :: RachelP Exp
p_ExpUnit = choice [
    EReal <$> try p_Real
  , EInteger <$> p_Integer
  , try p_Var
  , p_Let
  , p_Lambda
  , p_Sum
  , try p_Prod
  , withParens p_Exp
    ]

p_App :: RachelP Exp
p_App = foldl1 EApp <$> spacedList1 p_ExpUnit

p_Exp :: RachelP Exp
p_Exp = do
  t <- readOpTable
  buildExpressionParser t p_App

p_FunDec :: RachelP Dec
p_FunDec = do
  i <- p_Ident
  spaces1
  as <- spacedList p_AIdent
  spaced $ char '='
  e <- p_Exp
  return $ FunDec i $ foldr ELambda e as

p_InfixDec :: RachelP Dec
p_InfixDec = do
  c <- try $ do
    string "infix"
    (char 'l' <|> char 'r') <* spaces1
  let a = if c == 'l' then LeftA else RightA
  i <- p_Op
  spaces1
  p <- read <$> many1 digit
  return $ InfixDec i $ Fixity a p

p_PatDec :: RachelP Dec
p_PatDec = do
  char '#'
  spaces
  i <- p_AIdent
  spaced $ char '|'
  x <- sepBy1 p_AIdent spaces1
  let n = length x
  spaces
  xs <- many $ do
    char '|'
    i1 <- p_AIdent
    y <- replicateM (n-1) $ spaces1 *> p_AIdent
    spaces
    return $ i1 : y
  return $ PatDec i n $ x : xs

-- | Parse a top-level declaration.
p_Dec :: RachelP Dec
p_Dec = choice [ p_InfixDec, p_PatDec , try p_TypeDec, p_FunDec ]

witheof :: RachelP a -> RachelP a
witheof p = do
  x <- p
  spaces >> eof
  return x
