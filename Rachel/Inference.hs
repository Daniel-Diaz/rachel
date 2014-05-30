
module Rachel.Inference (
    -- * Unification
    unify
  , applySubstitutions
    -- * Type inference
  , typeOf
  ) where

import Rachel.Types
import Rachel.Transformation
--
import qualified Data.Map as Map
import Data.Sequence (Seq,(|>),(<|))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
--
import Control.Applicative
import Control.Monad.Trans.State
import Control.Monad.Trans.Class (lift)
import qualified Data.Foldable as F
import Data.Maybe (isJust)

-- Unification algorithm

substitute :: Id -> Type -> Type -> Type
substitute x t (TVar v) = if x == v then t else TVar v
substitute x t (TFun f a) = TFun (substitute x t f) (substitute x t a)
substitute x t (TProd a b) = TProd (substitute x t a) (substitute x t b)
substitute x t (TSum a b) = TSum (substitute x t a) (substitute x t b)
substitute _ _ t0 = t0

-- Performs a unification step. Returns 'True' if a change has been made.
unificationStep :: Seq (Type,Type) -> Either String (Seq (Type,Type),Bool)
unificationStep tseq = if Seq.null tseq then Right (tseq,False) else
  let (t1,t2) Seq.:< ts = Seq.viewl tseq
      failUnify = Left $ "Types `" ++ pretty t1 ++ "` and `" ++ pretty t2 ++ "` don't unify."
  in  if t1 == t2
         then Right (ts,True)
         else case (t1,t2) of
                -- Decompose
                (TFun  f x,TFun  f' x') -> Right ((f,f') <| (x,x') <| ts,True)
                (TProd x y,TProd x' y') -> Right ((x,x') <| (y,y') <| ts,True)
                (TSum  x y,TSum  x' y') -> Right ((x,x') <| (y,y') <| ts,True)
                -- Check and eliminate
                (TVar v,t) ->
                  if Set.member v (typeVars t)
                     then Left $ "Unification is impossible because `" ++ v ++ "` appears in `" ++ pretty t ++ "`."
                     else if Set.notMember v (typeVars t)
                                 && isJust   (Seq.findIndexL (\(x,y) -> Set.member v (typeVars x)
                                                                     || Set.member v (typeVars y)) ts)
                             then Right $ ((TVar v, t) <| fmap (\(x,y) -> (substitute v t x,substitute v t y)) ts,True)
                             else Right (ts |> (t1,t2),False)
                -- Swap
                (t,TVar v) -> Right ((TVar v,t) <| ts,True)
                -- Otherwise
                _ -> failUnify

unification :: Seq (Type,Type) -> Int -> Either String (Seq (Type,Type))
unification tseq i =
  if i == Seq.length tseq
     then Right tseq
     else case unificationStep tseq of
            Left err -> Left err
            Right (tseq',b) -> unification tseq' $ if b then 0 else i+1

-- | Check if two types unify. If they don't, a 'String' with an error description is returned.
--   If they do, it returns a 'Seq'uence of substitutions that makes both types equal.
--   To apply these substitutions to a given type, use 'applySubstitutions'.
unify :: Type -> Type -> Either String (Seq (Id,Type))
unify t1 t2 = fmap (\(TVar x,t) -> (x,t)) <$> unification (Seq.singleton (t1,t2)) 0

applySubstitutions :: Seq (Id,Type) -> Type -> Type
applySubstitutions tseq t0 = F.foldr (\(v,s) t -> substitute v s t) t0 tseq

-- Type inference algorithm

data TIState = TIS { tiContext :: Context , tiNumber :: Int , tiCounter :: Int }

type TIM = StateT TIState (Either String)

newTypeVar :: TIM Type
newTypeVar = do
  st <- get
  let n = tiNumber st
  put $ st { tiNumber = n + 1 }
  return $ TVar $ "t" ++ show n ++ "#local"

makeExtern :: Type -> TIM Type
makeExtern t = do
  st <- get
  let n = tiCounter st
  put $ st { tiCounter = n + 1 }
  return $ sufixType ("#e" ++ show n) t

tiUpdate :: Seq (Id,Type) -> TIM ()
tiUpdate s = modify $ \st -> st { tiContext = fmap (applySubstitutions s) $ tiContext st }

typeOfM :: Exp -> TIM Type
typeOfM (EReal _) = return TReal
typeOfM (EInteger _) = return TInteger
typeOfM (EVar x) = do
  ct <- tiContext <$> get
  case Map.lookup x ct of
    Nothing -> lift $ Left $ "Unbound variable `" ++ x ++ "`."
    Just t -> if hasHash t then return t
                           else makeExtern t
typeOfM (EProd e1 e2) = TProd <$> typeOfM e1 <*> typeOfM e2
typeOfM ESumL = do
  t <- newTypeVar
  fmap (TFun t) $ TSum <$> return t <*> newTypeVar
typeOfM ESumR = do
  t <- newTypeVar
  fmap (TFun t) $ TSum <$> newTypeVar <*> return t
typeOfM (ELambda x e) = do
  tx <- newTypeVar
  modify $ \st -> st { tiContext = Map.insert x tx $ tiContext st }
  te <- typeOfM e
  ct <- tiContext <$> get
  return $ TFun (ct Map.! x) te
typeOfM (ELet x e1 e2) = do
  tx <- typeOfM e1
  modify $ \st -> st { tiContext = Map.insert x tx $ tiContext st }
  typeOfM e2
typeOfM (EApp f x) = do
  tf <- typeOfM f
  tx <- typeOfM x
  ty <- newTypeVar
  case unify (TFun tx ty) tf of
    Left err -> lift $ Left $ err
             ++ "\n  When matching `" ++ pretty x ++ " : " ++ pretty tx
             ++ "` as an argument of `" ++ pretty f ++ " : " ++ pretty tf ++ "`."
    Right s -> do tiUpdate s
                  return $ applySubstitutions s ty

-- typeOf :: Context -> Exp -> Either String (Type,Context)
-- typeOf ct e = fmap (second tiContext) $ runStateT (typeOfM e) $ TIS ct 0 0

typeOf :: Context -> Exp -> Either String Type
typeOf ct e = fmap renameTypeVars $ evalStateT (typeOfM e) $ TIS ct 0 0
