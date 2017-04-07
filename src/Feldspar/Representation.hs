{-# language GADTs #-}
{-# language DataKinds #-}
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
import Data.Inhabited

import Data.Constraint
import Data.Int (Int8)
import Data.Word (Word8)
import Data.List (genericTake)
import Data.Typeable hiding (typeRep, TypeRep)

-- syntactic.
import Language.Syntactic hiding ((:+:))
import Language.Syntactic.Functional
import Language.Syntactic.Functional.Tuple

-- operational-higher.
import Control.Monad.Operational.Higher (ProgramT, Program, Param2, (:+:))

--------------------------------------------------------------------------------
-- * Program.
--------------------------------------------------------------------------------
{-
data ProgramT instr fs m a
  where
    Lift  :: m a -> ProgramT instr fs m a
    Bind  :: ProgramT instr fs m a -> (a -> ProgramT instr fs m b) -> ProgramT instr fs m b
    Instr :: instr '(ProgramT instr fs m, fs) a -> ProgramT instr fs m a
-}

type family Expression (m :: * -> *) :: * -> *
  where
    Expression (ProgramT instr '(exp, fs) m) = exp

type family Predicate (m :: * -> *) :: * -> *
  where
    Predicate (ProgramT instr '(exp, '(pred, fs)) m) = pred

--------------------------------------------------------------------------------
-- * Type.
--------------------------------------------------------------------------------

-- ...

--------------------------------------------------------------------------------
-- * Interface.
--------------------------------------------------------------------------------

class Eq exp => Bits exp
  where
    (.&.) :: pred a => exp a -> exp a -> exp a

--------------------------------------------------------------------------------

class Reference m
  where
    type Ref m
    newRef :: pred a => a -> m (Ref m a)

-- pred a ~ Syntactic a, Type (Predicate m) (Internal a), 

--------------------------------------------------------------------------------
