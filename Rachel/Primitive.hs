
{-# LANGUAGE TemplateHaskell #-}

module Rachel.Primitive (
    -- * Primitives
    -- | The list of every primitive entity.
    allPrimitives
  ) where

import Rachel.Types
import Rachel.TH

import Control.Arrow (second)

import Data.Sound -- wavy
import Data.Sound.Core.Chunked

a, b, c :: Type
a = TVar "a"
b = TVar "b"
c = TVar "c"

infixr 3 ->>

(->>) :: Type -> Type -> Type
(->>) = TFun

decons2 :: t a b -> (a,b)
decons2 = undefined

$(genPrim 0)
$(genPrim 1)
$(genPrim 2)
$(genPrim 3)
$(genPrim 4)
 
-- | Boolean value /true/.
--
-- > true : Bool
-- > true = true
e_true :: Entity
e_true = prim0 "true" True

-- | Boolean value /false/.
--
-- > false : Bool
-- > false = false
e_false :: Entity
e_false = prim0 "false" False

-- | Integer equality.
--
-- > intEq : Integer -> Integer -> Bool
-- > intEq x y = x == y
e_intEq :: Entity
e_intEq = prim2 "intEq" ((==) :: Integer -> Integer -> Bool)

-- | Real equality.
--
-- > realEq : Real -> Real -> Bool
-- > realEq x y = x == y
e_realEq :: Entity
e_realEq = prim2 "realEq" ((==) :: Double -> Double -> Bool)

-- | Integer addition.
--
-- > intPlus : Integer -> Integer -> Integer
-- > intPlus x y = x + y
e_intPlus :: Entity
e_intPlus = prim2 "intPlus" ((+) :: Integer -> Integer -> Integer)

-- | Real addition.
--
-- > realPlus : Real -> Real -> Real
-- > realPlus x y = x + y
e_realPlus :: Entity
e_realPlus = prim2 "realPlus" ((+) :: Double -> Double -> Double)

-- | Integer product.
--
-- > intProd : Integer -> Integer -> Integer
-- > intProd x y = x * y
e_intProd :: Entity
e_intProd = prim2 "intProd" ((*) :: Integer -> Integer -> Integer)

-- | Real product.
--
-- > realProd : Real -> Real -> Real
-- > realProd x y = x * y
e_realProd :: Entity
e_realProd = prim2 "realProd" ((*) :: Double -> Double -> Double)

-- | Integer division.
e_intDiv :: Entity
e_intDiv = prim2 "intDiv" (div :: Integer -> Integer -> Integer)

-- | Real division.
e_realDiv :: Entity
e_realDiv = prim2 "realDiv" ((/) :: Double -> Double -> Double)

-- | Fixed-point combinator.
--
-- > fix : (a -> a) -> a
-- > fix f = let x = f x in x
e_fix :: Entity
e_fix = Entity "fix" ((a ->> a) ->> a) defaultFixity $
  VFun $ \v -> 
    case v of
      VFun f -> let x = f x in x
      VBottom str -> VBottom str
      _ -> error "Unexpected value, aborting..."

-- | Conditional combinator.
--
-- > if : Bool -> a -> a -> a
-- > if b x y = if b then x else y
e_if :: Entity
e_if = Entity "if" (TBool ->> a ->> a ->> a) defaultFixity $
  VFun $ \(VBool (RBool p)) ->
    VFun $ \x ->
      VFun $ \y -> if p then x else y

-- | Tau constant.
e_tau :: Entity
e_tau = prim0 "tau" (2*pi :: Double)

-- | Sine function.
e_sin :: Entity
e_sin = prim1 "sin" (sin :: Double -> Double)

-- | Cosine function.
e_cos :: Entity
e_cos = prim1 "cos" (cos :: Double -> Double)

-- | Integer negation.
e_intNeg :: Entity
e_intNeg = prim1 "intNeg" (negate :: Integer -> Integer)

-- | Real negation.
e_realNeg :: Entity
e_realNeg = prim1 "realNeg" (negate :: Double -> Double)

-- | Alternative application using sum type.
e_choice :: Entity
e_choice = Entity "choice" ((a ->> c) ->> (b ->> c) ->> TSum a b ->> c) defaultFixity $
  VFun $ \(VFun f) ->
    VFun $ \(VFun g) ->
      VFun $ \s ->
        case s of
          VSumL x -> f x
          VSumR x -> g x 
          _ -> undefined

e_bottom :: Entity
e_bottom = Entity "bottom" a defaultFixity $ VBottom "bottom"

e_soundSeq :: Entity
e_soundSeq = prim2 "soundSeq" (<.>)

e_soundAdd :: Entity
e_soundAdd = prim2 "soundAdd" (<+>)

e_zeroSound :: Entity
e_zeroSound = prim1 "zeroSound" zeroSound

e_addAt :: Entity
e_addAt = prim3 "addAt" addAt

-- e_soundFunction :: Entity
-- e_soundFunction = prim2 "soundFunction" (\d f -> fromFunction 16 d Nothing f)

e_soundFunction :: Entity
e_soundFunction = Entity "soundFunction" (TReal ->> (TReal ->> TReal) ->> TSound) defaultFixity $
  VFun $ \(VReal (RReal d)) ->
    VFun $ \(VFun f) ->
      VSound $ RSound $
        fromFunction 44100 d Nothing $ \t ->
          case fromValue . f . toValue . toPrimitive $ t of
            Nothing -> undefined
            Just x -> monoSample $ fromPrimitive x

-- Requires Strings...
-- e_soundFile :: Entity
-- e_soundFile = Entity "soundFile"

-- Generate list of all primitives

$(genEntList)
