{-# language GADTs #-}
{-# language TypeFamilies #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language UndecidableInstances #-}
{-# language UndecidableSuperClasses #-}
{-# language TypeOperators #-}
{-# language Rank2Types #-}
{-# language ConstraintKinds #-}

{-# language ScopedTypeVariables #-}

module Feldspar.Representation where

import Data.Struct

import Data.Constraint
import Data.Word
import Data.List (genericTake)
import Data.Typeable hiding (typeRep, TypeRep)

-- syntactic.
import Language.Syntactic hiding ((:+:))
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple

-- hardware-edsl.
import Language.Embedded.Hardware.Expression.Represent (Inhabited(..))

-- operational-higher.
import Control.Monad.Operational.Higher (Program, Param2, (:+:))

--------------------------------------------------------------------------------
-- * Types.
--------------------------------------------------------------------------------

-- | Representation of supported feldspar types as typed binary trees over
--   primitive types.
type TypeRep pred rep = Struct pred rep

-- | Representation of supported value types and N-ary functions over such
--   types.
data TypeRepF pred rep a
  where
    ValT :: TypeRep pred rep a -> TypeRepF pred rep a
    FunT :: TypeRep pred rep a -> TypeRepF pred rep b -> TypeRepF pred rep (a -> b)

--------------------------------------------------------------------------------

-- | ... hmm ...
type family Expr (lang :: * -> *) :: * -> *

-- | Domain associated with a language.
type family DomainOf (lang :: * -> *) :: * -> *

-- | Predicate associated with a domain.
type family PredicateOf (dom :: * -> *) :: * -> Constraint

-- | Type representation associated with a predicate.
type family RepresentationOf (pred :: * -> Constraint) :: * -> *

--------------------------------------------------------------------------------
  
-- | Supported types, that is, types which can be represented as nested pairs of
--   simpler values that respect `pred` and are in turn represented using `trep`.
class (Eq a, Show a, Typeable a, Inhabited a) => Type pred a
  where
    typeRep :: TypeRep pred (RepresentationOf pred) a

-- Pairs of valid types are themselves also valid types.
instance (Type pred a, Type pred b) => Type pred (a, b)
  where
    typeRep = Branch typeRep typeRep

--------------------------------------------------------------------------------

-- | ...
class    (Type pred a, pred a) => PrimType pred a
instance (Type pred a, pred a) => PrimType pred a

--------------------------------------------------------------------------------

type Length = Word32

type Index  = Word32

--------------------------------------------------------------------------------

instance (Inhabited a, Inhabited b) => Inhabited (a, b)
  where
    reset = (reset, reset)

--------------------------------------------------------------------------------
