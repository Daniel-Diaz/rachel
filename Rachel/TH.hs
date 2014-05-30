
module Rachel.TH (
    -- * Primitive list
    genEntList
    -- * prim functions
  , genPrim
  ) where

import Language.Haskell.Exts (ParseResult (..), Module (..), Decl (..), prettyPrint)
import Language.Haskell.Exts.Parser (parseModuleWithMode,defaultParseMode,ParseMode(..))
import Language.Haskell.Exts.Extension
import Language.Haskell.TH
import Data.List (isPrefixOf)

genEntList :: Q [Dec]
genEntList = do
  t <- runIO $ readFile "Rachel/Primitive.hs"
  let m = defaultParseMode { extensions =
            [ EnableExtension TemplateHaskell
            , EnableExtension MultiParamTypeClasses
              ] }
  case parseModuleWithMode m t of
    ParseFailed _ _ -> fail "Couldn't parse module of primitives."
    ParseOk (Module _ _ _ _ _ _ xs) ->
      let l = ListE $ concat $ fmap genEnt xs
          n = mkName "allPrimitives"
          b = NormalB l
      in  return $ 
            [ SigD n $ AppT ListT (ConT $ mkName "Entity")
            , FunD n [ Clause [] b [] ] ]

genEnt :: Decl -> [Exp]
genEnt (TypeSig _ ns _) =
  let xs = fmap prettyPrint ns
  in  fmap (VarE . mkName) $ filter (isPrefixOf "e_") xs
genEnt _ = []

infixr 3 ->>

(->>) :: Type -> Type -> Type
t1 ->> t2 = AppT (AppT ArrowT t1) t2

genPrim :: Int -> Q [Dec]
genPrim = return . genPrimPure

genPrimPure :: Int -> [Dec]
genPrimPure n =
  [ SigD (mkName $ "prim" ++ show n) $
    -- Type
      ForallT (concat $ fmap (\i -> [ PlainTV $ mkName $ "p" ++ show i
                                    , PlainTV $ mkName $ "h" ++ show i ]) [0..n])
              -- Context
              (fmap (\i -> ClassP (mkName "PrimitiveType") [ VarT $ mkName $ "p" ++ show i
                                                           , VarT $ mkName $ "h" ++ show i]) [0..n])
              -- Type
              $    ConT (mkName "String")
              ->> (foldr1 (->>) $ fmap (\i -> VarT $ mkName $ "h" ++ show i) [0..n])
              ->>  ConT (mkName "Entity")
  , FunD (mkName $ "prim" ++ show n) [genPrimClause n]
    ]

genPrimClause :: Int -> Clause
genPrimClause n = Clause [VarP $ mkName "str" , VarP $ mkName "f"] (genPrimBody n) (genPrimWhere n)

appList :: [Exp] -> Exp
appList = foldl1 AppE

infixr 1 $$

($$) :: Exp -> Exp -> Exp
e1 $$ e2 = UInfixE e1 (VarE $ mkName "$") e2

genPrimBody :: Int -> Body
genPrimBody n = NormalB $ appList
   [ ConE $ mkName "Entity"
   , VarE $ mkName "str"
   , foldr1 (\t1 t2 -> UInfixE t1 (VarE $ mkName "->>") t2)
       $ fmap (\i -> VarE (mkName "toType") `AppE` VarE (mkName $ "p" ++ show i)) [0..n]
   , VarE $ mkName "defaultFixity"
   ] $$ lambdar n
  where
    lambdar 0 = VarE (mkName "toValue") $$ (appList $ VarE (mkName "f'") : fmap (\i -> VarE $ mkName $ "x" ++ show i) [1..n])
    lambdar i =
      let i' = n - i + 1
      in  (ConE (mkName "VFun") $$) $ (\l -> UInfixE l (VarE $ mkName ".") $ VarE $ mkName "fromValue") 
             $ LamE [VarP $ mkName $ "mx" ++ show i']
             $ CaseE (VarE $ mkName $ "mx" ++ show i')
                 [ Match (ConP (mkName "Nothing") []) (NormalB $ ConE (mkName "VBottom") `AppE` LitE (StringL "bottom")) []
                 , Match (ConP (mkName "Just") [VarP $ mkName $ "x" ++ show i']) (NormalB $ lambdar $ i - 1) []
                  ]

genPrimWhere :: Int -> [Dec]
genPrimWhere n = ValD hs (NormalB $ dec n) [] : FunD (mkName "f'") [fprime] : ps
  where
    tOp p1 p2 = TupP [p1,p2]
    hs = foldr1 tOp $ fmap (\i -> VarP $ mkName $ "h" ++ show i) [0..n]
    dec 0 = VarE $ mkName "f"
    dec i =
      let t = foldr (\s e -> s $$ e) (VarE $ mkName "decons2") $ replicate (i-1) $ VarE $ mkName "second"
      in  AppE t $ dec (i-1)
    fprime = Clause (fmap (\i -> VarP $ mkName $ "x" ++ show i) [1..n]) (NormalB fprimebod) []
    fprimebod = VarE (mkName "toPrimitive") $$
       ( appList $ dec 0 : fmap (\i -> ParensE $ AppE (VarE $ mkName "fromPrimitive") $ VarE $ mkName $ "x" ++ show i) [1..n]
         )
    ps = fmap (\i -> ValD (VarP $ mkName $ "p" ++ show i)
                          (NormalB $ VarE (mkName "toPrimitive") `AppE` VarE (mkName $ "h" ++ show i))
                          []
                ) [0..n]
