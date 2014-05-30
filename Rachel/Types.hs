
{-# LANGUAGE FunctionalDependencies #-}

module Rachel.Types (
    -- * Pretty printing
    Pretty (..)
    -- * Rachel values
    -- ** Primitive values
  , PrimitiveType (..)
  , RReal (..), RInteger (..)
  , RBool (..), RSound (..)
    -- ** All values
  , Value (..)
    -- * Expressions
  , Id
  , Exp (..)
    -- * Type of a expression
  , Type (..)
  , typeVars
  , mapTypeVars
  , prefixType, sufixType
  , hasHash, removeHash
    -- * Context
  , Context, Environment
  , OpAssoc (..)
  , Fixity (..), defaultFixity
  , Entity (..)
  , insertEntity
  , contextFromEnv
    -- * Declarations
  , Dec (..)
  ) where

import Data.Sound
-- Data structures
import Data.Map (Map,insert)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Fixed (Micro, showFixed)
-- Other imports
import Data.String
import Data.Char (isLetter)
import qualified Data.Foldable as F

-- Pretty printing

-- | Human readable representation.
class Pretty a where
  pretty :: a -> String

parens :: Bool -> String -> String
parens True str = '(' : (str ++ ")")
parens _ str = str

-- Rachel types

-- | Real numbers represented as floating point numbers (double precision).
newtype RReal = RReal Double deriving (Show,Eq)

instance Pretty RReal where
  pretty (RReal r) = showFixed True ((fromRational . toRational) r :: Micro)

-- | Integer numbers.
newtype RInteger = RInteger Integer deriving (Show,Eq)

instance Pretty RInteger where
  pretty (RInteger i) = show i

-- | Booleans.
newtype RBool = RBool Bool

instance Pretty RBool where
  pretty (RBool True) = "true"
  pretty (RBool False) = "false"

-- | A sound. See 'Sound'.
newtype RSound = RSound Sound

instance Pretty RSound where
  pretty (RSound _) = "#sound#"

--------------------------

class PrimitiveType p h | h -> p , p -> h where
  toPrimitive :: h -> p
  fromPrimitive :: p -> h
  toValue :: p -> Value
  fromValue :: Value -> Maybe p
  toType :: p -> Type

instance PrimitiveType RInteger Integer where
  toPrimitive = RInteger
  fromPrimitive (RInteger i) = i
  toValue = VInteger
  fromValue (VInteger i) = Just i
  fromValue _ = Nothing
  toType _ = TInteger

instance PrimitiveType RReal Double where
  toPrimitive = RReal
  fromPrimitive (RReal i) = i
  toValue = VReal
  fromValue (VReal i) = Just i
  fromValue _ = Nothing
  toType _ = TReal

instance PrimitiveType RBool Bool where
  toPrimitive = RBool
  fromPrimitive (RBool i) = i
  toValue = VBool
  fromValue (VBool i) = Just i
  fromValue _ = Nothing
  toType _ = TBool

instance PrimitiveType RSound Sound where
  toPrimitive = RSound
  fromPrimitive (RSound i) = i
  toValue = VSound
  fromValue (VSound i) = Just i
  fromValue _ = Nothing
  toType _ = TSound

-- instance (PrimitiveType p h, PrimitiveType p' h')
--       => PrimitiveType (p -> p') (h -> h') where
--   toPrimitive f = toPrimitive . f . fromPrimitive

--------------------------

-- | Any identifier contains a value of this type.
data Value =
    -- | Real numbers.
    VReal RReal
    -- | Integer numbers.
  | VInteger RInteger
    -- | Booleans.
  | VBool RBool
    -- | Sounds.
  | VSound RSound
    -- | Product.
  | VProd Value Value
    -- | Left value of a sum.
  | VSumL Value
    -- | Right value of a sum.
  | VSumR Value
    -- | Functions.
  | VFun (Value -> Value)
    -- | Bottom.
  | VBottom String

instance Pretty Value where
  pretty (VReal r) = pretty r
  pretty (VInteger i) = pretty i
  pretty (VBool b) = pretty b
  pretty (VSound s) = pretty s
  pretty (VProd x y) = "(" ++ pretty x ++ "," ++ pretty y ++ ")"
  pretty (VSumL x) = "L (" ++ pretty x ++ ")"
  pretty (VSumR x) = "R (" ++ pretty x ++ ")"
  pretty (VFun _) = "#function#"
  pretty (VBottom str) = "_|_: " ++ str

-- Expressions and types

-- | Identifiers are stored as 'String's.
type Id = String

-- | An expression in the Rachel language.
data Exp = EReal RReal       -- ^ @1.0@
         | EInteger RInteger -- ^ @1@
         | EVar Id           -- ^ @x@
         | EProd Exp Exp     -- ^ @(e1,e2)@
         | ESumL             -- ^ @L@
         | ESumR             -- ^ @R@
         | EApp Exp Exp      -- ^ @e1 e2@
         | ELambda Id Exp    -- ^ @\\x.e@
         | ELet Id Exp Exp   -- ^ @let x = e1 in e2@
           deriving (Show,Eq)

instance IsString Exp where
  fromString = EVar

isEOp :: Exp -> Bool
isEOp (EProd _ _) = True
isEOp (EApp _ _) = True
isEOp _ = False

instance Pretty Exp where
  pretty (EReal r) = pretty r
  pretty (EInteger i) = pretty i
  pretty (EVar str) =
    case str of
      [] -> error "Variable with empty name!"
      (x:_)  -> if isLetter x || x == '_'
                   then str
                   else "(" ++ str ++ ")"
  pretty (EProd x y) =
    let pretty' e = parens (isEOp e) (pretty e)
    in  "(" ++ pretty' x ++ "," ++ pretty' y ++ ")"
  pretty ESumL = "L"
  pretty ESumR = "R"
  pretty (EApp f x) =
    let pretty' e = parens (isEOp e) (pretty e)
    in  pretty' f ++ " " ++ pretty' x
  pretty (ELambda x e) = "\\" ++ x ++ "." ++ pretty e
  pretty (ELet x e1 e2) = "let " ++ x ++ " = " ++ pretty e1 ++ " in " ++ pretty e2

-- | The type of Rachel types.
data Type = -- Primitive types
            TReal | TInteger | TBool | TSound
            -- Arity two types
          | TFun Type Type
          | TProd Type Type
          | TSum Type Type
            -- Type variables
          | TVar Id deriving (Show,Eq)

isTOp :: Type -> Bool
isTOp (TFun _ _) = True
isTOp (TProd _ _) = True
isTOp (TSum _ _) = True
isTOp _ = False

isTFun :: Type -> Bool
isTFun (TFun _ _) = True
isTFun _ = False

hasHash :: Type -> Bool
hasHash = F.any (elem '#') . typeVars

-- | Remove all the characters after a hash (including the hash)
--   in every type variable.
removeHash :: Type -> Type
removeHash = mapTypeVars $ takeWhile (/='#')

instance Pretty Type where
 pretty TReal = "Real"
 pretty TInteger = "Integer"
 pretty TBool = "Bool"
 pretty TSound = "Sound"
 pretty (TFun f x) =
   let pretty' e = parens (isTFun e) (pretty e)
   in  pretty' f ++ " -> " ++ pretty x
 pretty (TProd x y) =
   let pretty' e = parens (isTOp e) (pretty e)
   in  pretty' x ++ "*" ++ pretty' y
 pretty (TSum x y) =
   let pretty' e = parens (isTOp e) (pretty e)
   in  pretty' x ++ "+" ++ pretty' y
 pretty (TVar str) = str

-- | Map a function over all variable names in a type.
mapTypeVars :: (String -> String) -> Type -> Type
mapTypeVars f = go
  where
   go (TVar v) = TVar $ f v
   go (TFun x y) = TFun (go x) (go y)
   go (TProd x y) = TProd (go x) (go y)
   go (TSum x y) = TSum (go x) (go y)
   go t = t

-- | Add a prefix to all the variables in a type.
--   This has the property that, given any two types @t1@ and @t2@,
--   if the strings @str1@ and @str2@ are /different/, then the
--   set of variables of @prefixType str1 t1@ and @prefixType str2 t2@
--   are disjoint.
prefixType :: String -> Type -> Type
prefixType str = mapTypeVars (str++)

-- | Add a sufix to all the variables in a type.
sufixType :: String -> Type -> Type
sufixType str = mapTypeVars (++str)

-- | Type variables in a type.
typeVars :: Type -> Set Id
typeVars = typeVarsAux Set.empty

typeVarsAux :: Set Id -> Type -> Set Id
typeVarsAux xs (TVar x) = Set.insert x xs
typeVarsAux xs (TFun f x) =
  let ys = typeVarsAux xs f
  in  typeVarsAux ys x
typeVarsAux xs (TProd x y) =
  let ys = typeVarsAux xs x
  in  typeVarsAux ys y
typeVarsAux xs (TSum x y) =
  let ys = typeVarsAux xs x
  in  typeVarsAux ys y
typeVarsAux xs _ = xs

data OpAssoc = LeftA | RightA deriving Eq

-- | A fixity value of an 'Entity'.
--   The first parameter indicates the associativity as operator.
--   The second parameter indicates the priority.
data Fixity = Fixity OpAssoc Int deriving Eq

instance Pretty Fixity where
  pretty (Fixity a p) =
    let str = if a == LeftA then "l" else "r"
    in  "infix" ++ str ++ " " ++ show p

defaultFixity :: Fixity
defaultFixity = Fixity LeftA 10

-- | A complete 'Entity' with identity, type, fixity and value.
data Entity = Entity Id Type Fixity Value

-- | A context is a map from each defined identity to its 'Type'.
type Context = Map Id Type

-- | An environment is a map from each defined identity to its full definition,
--   i.e. its type, its fixity and its value.
type Environment = Map Id (Type,Fixity,Value)

-- | Insert an 'Entity' in an environment, replacing the type and value of an
--   already defined identity.
insertEntity :: Entity -> Environment -> Environment
insertEntity (Entity i t f v) = insert i (t,f,v)

-- | Discard the values of every identity to get a 'Context' from an 'Environment'.
contextFromEnv :: Environment -> Context
contextFromEnv = fmap $ \(t,_,_) -> t

-- Declarations

-- | Rachel top level declarations.
data Dec =
    TypeDec Id Type
  | FunDec Id Exp
  | InfixDec Id Fixity
  | PatDec Id Int [[Id]] -- ^ Pattern declaration. The 'Int' value specifies the number of columns.

