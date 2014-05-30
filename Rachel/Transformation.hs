
module Rachel.Transformation (
    -- * Raw
    removeComments
    -- * Expressions
  , expReplace
  , asFixPoint
    -- * Types
  , renameTypeVars
  ) where

import Rachel.Types

import Control.Applicative ((<$>))
import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as M
import qualified Data.Set as S

-- Comments

p_IComment :: Parser ()
p_IComment = void $ string "--" >> manyTill anyChar (void newline <|> eof)

p_MComment :: Parser ()
p_MComment = do
  _ <- string "{-"
  void $ manyTill anyChar $ try $ string "-}"

p_Commented :: Parser String
p_Commented =
  let p :: Parser (String -> String)
      p = fmap (foldr (.) id)
        $ many
        $ choice [ const id <$> try p_MComment
                 , const ('\n':) <$> try p_IComment
                 , (:) <$> anyChar ]
  in ($ []) <$> p

removeComments :: String -> String
removeComments str =
  let Right str' = parse p_Commented "source" str
  in  tail str'

-- Expression transformation

expReplace :: Exp -- ^ Expression to substitute @o@.
           -> Exp -- ^ Result of the substitution @t@.
           -> Exp -- ^ Original expression @e@.
           -> Exp -- ^ Output expression: same as expression @e@ but with
                  --   occurrences of @o@ substitued by @t@.
expReplace o t = go
 where
  f e e' = if o == e then t else e'
  --
  go e@(EProd x y)   = f e $ EProd     (go x) (go y)
  go e@(EApp x y)    = f e $ EApp      (go x) (go y)
  go e@(ELambda i x) = f e $ ELambda i (go x)
  go e@(ELet i x y)  = f e $ ELet i    (go x) (go y)
  go e = f e e

asFixPoint :: Id -> Exp -> Exp
asFixPoint x e = ELet e' (ELambda f $ expReplace (EVar x) (EVar f) e) $ EApp (EVar "fix") (EVar e')
  where
    f  = "_f"
    e' = "_e"

-- Variable renaming

-- | Apply a renaming to all type variables using @t0@, @t1@, @t2@, ... and so on for the new names.
renameTypeVars :: Type -> Type
renameTypeVars t = mapTypeVars (m M.!) t
  where
    (m,_) = S.foldr (\v (vm,i) -> (M.insert v ("t" ++ show i) vm , i+1)) (M.empty,0 :: Int) $ typeVars t
