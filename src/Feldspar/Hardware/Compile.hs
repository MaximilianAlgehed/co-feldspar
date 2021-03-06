{-# language GADTs #-}
{-# language TypeOperators #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language ScopedTypeVariables #-}

module Feldspar.Hardware.Compile (compile, icompile, icompileWrap) where

import Feldspar.Sugar
import Feldspar.Representation
import Feldspar.Hardware.Primitive
import Feldspar.Hardware.Primitive.Backend
import Feldspar.Hardware.Representation
-- ...
import Feldspar.Hardware.Frontend (component, input, ret, process)
--import Feldspar.Software.Representation ()

import Data.Struct

import Control.Monad.Identity
import Control.Monad.Reader
import Data.Constraint hiding (Sub)
import Data.Map (Map)
import qualified Data.Map as Map

-- operational-higher.
import Control.Monad.Operational.Higher (Program, ProgramT)
import qualified Control.Monad.Operational.Higher as Oper

-- syntactic.
import Language.Syntactic hiding (Nil)
import Language.Syntactic.Functional hiding (Binding (..))
import Language.Syntactic.Functional.Tuple
import qualified Language.Syntactic as Syn

-- hardware-edsl.
import Language.Embedded.Hardware (Signal, FreeExp (..))
import qualified Language.Embedded.Hardware         as Hard
import qualified Language.Embedded.Hardware.Command as Hard

--------------------------------------------------------------------------------
-- * Hardware compiler.
--------------------------------------------------------------------------------

compile :: Hardware a -> String
compile = Hard.compile . translate

icompile :: Hardware a -> IO ()
icompile = Hard.icompile . translate

--------------------------------------------------------------------------------

icompileWrap :: Hardware () -> IO ()
icompileWrap = Hard.icompile . translate . wrap

--------------------------------------------------------------------------------

-- todo: use wrap from hardware-edsl.
wrap :: Hardware () -> Hardware ()
wrap prg = void $ component $
  ret $ process [] $ prg

--------------------------------------------------------------------------------
-- ** Instructions.
--------------------------------------------------------------------------------

type TargetCMD
    =        Hard.VariableCMD
    Oper.:+: Hard.ArrayCMD
    Oper.:+: Hard.VArrayCMD
    Oper.:+: Hard.LoopCMD
    Oper.:+: Hard.ConditionalCMD
    Oper.:+: Hard.ComponentCMD
    Oper.:+: Hard.StructuralCMD
    Oper.:+: Hard.ConstantCMD
    Oper.:+: Hard.SignalCMD

type TargetT m = ReaderT Env (ProgramT TargetCMD (Oper.Param2 Prim HardwarePrimType) m)

type ProgH = Program TargetCMD (Oper.Param2 Prim HardwarePrimType)

--------------------------------------------------------------------------------

-- ...

--------------------------------------------------------------------------------
-- ** Expressions.
--------------------------------------------------------------------------------

-- | Struct expression.
type VExp = Struct HardwarePrimType Prim

-- | Struct expression with hidden result type.
data VExp' where
  VExp' :: Struct HardwarePrimType Prim a -> VExp'

newRefV :: Monad m => HTypeRep a -> String -> TargetT m (Struct HardwarePrimType Hard.Variable a)
newRefV t base = lift $ mapStructA (const (Hard.newNamedVariable base)) t

initRefV :: Monad m => String -> VExp a -> TargetT m (Struct HardwarePrimType Hard.Variable a)
initRefV base = lift . mapStructA (Hard.initNamedVariable base)

getRefV :: Monad m => Struct HardwarePrimType Hard.Variable a -> TargetT m (VExp a)
getRefV = lift . mapStructA Hard.getVariable

setRefV :: Monad m => Struct HardwarePrimType Hard.Variable a -> VExp a -> TargetT m ()
setRefV r = lift . sequence_ . zipListStruct Hard.setVariable r

unsafeFreezeRefV :: Monad m => Struct HardwarePrimType Hard.Variable a -> TargetT m (VExp a)
unsafeFreezeRefV = lift . mapStructA Hard.unsafeFreezeVariable

--------------------------------------------------------------------------------

type Env = Map Name VExp'

localAlias :: MonadReader Env m => Name -> VExp a -> m b -> m b
localAlias v e = local (Map.insert v (VExp' e))

lookAlias :: MonadReader Env m => HTypeRep a -> Name -> m (VExp a)
lookAlias t v = do
  env <- ask
  return $ case Map.lookup v env of
    Nothing -> error $ "lookAlias: variable " ++ show v ++ " not in scope."
    Just (VExp' e) -> case hardwareTypeEq t (hardwareTypeRep e) of Just Dict -> e

--------------------------------------------------------------------------------

translateExp :: forall m a . Monad m => HExp a -> TargetT m (VExp a)
translateExp = goAST . unHExp
  where
    goAST :: ASTF HardwareDomain b -> TargetT m (VExp b)
    goAST = simpleMatch (\(s :&: ValT t) -> go t s)

    goSmallAST :: HardwarePrimType b => ASTF HardwareDomain b -> TargetT m (Prim b)
    goSmallAST = fmap extractNode . goAST

    go    :: HTypeRep (DenResult sig)
          -> HardwareConstructs sig
          -> Syn.Args (AST HardwareDomain) sig
          -> TargetT m (VExp (DenResult sig))
    go t lit Syn.Nil
      | Just (Lit a) <- prj lit
      = return $ mapStruct (litE . runIdentity) $ toStruct t a
    go t lit Syn.Nil
      | Just (Literal a) <- prj lit
      = return $ mapStruct (litE . runIdentity) $ toStruct t a
    go t var Syn.Nil
      | Just (FreeVar v) <- prj var
      = return $ Node $ sugarSymPrim $ FreeVar v
    go t var Syn.Nil
      | Just (VarT v) <- prj var
      = lookAlias t v
    go t lt (a :* (lam :$ body) :* Syn.Nil)
      | Just (Let tag) <- prj lt
      , Just (LamT v)  <- prj lam
      = do let base = if Prelude.null tag then "let" else tag
           r  <- initRefV base =<< goAST a
           a' <- unsafeFreezeRefV r
           localAlias v a' $ goAST body
    go t tup (a :* b :* Syn.Nil)
      | Just Pair <- prj tup =
          Branch <$> goAST a <*> goAST b
    go t sel (ab :* Syn.Nil)
      | Just Fst <- prj sel = do
          Branch a _ <- goAST ab
          return a
      | Just Snd <- prj sel = do
          Branch _ b <- goAST ab
          return b
    go ty cond (b :* t :* f :* Syn.Nil)
      | Just Cond <- prj cond = do
          res <- newRefV ty "b"
          b'  <- goSmallAST b
          ReaderT $ \env -> Hard.iff b'
            (flip runReaderT env $ setRefV res =<< goAST t)
            (flip runReaderT env $ setRefV res =<< goAST f)
          unsafeFreezeRefV res
    go t op (a :* Syn.Nil)
      | Just Neg <- prj op = liftStruct (sugarSymPrim Neg) <$> goAST a
      | Just Not <- prj op = liftStruct (sugarSymPrim Not) <$> goAST a
      | Just I2N <- prj op = liftStruct (sugarSymPrim I2N) <$> goAST a
      | Just BitCompl <- prj op =
          liftStruct (sugarSymPrim BitCompl) <$> goAST a
    go t op (a :* b :* Syn.Nil)
      | Just Add <- prj op = liftStruct2 (sugarSymPrim Add) <$> goAST a <*> goAST b
      | Just Sub <- prj op = liftStruct2 (sugarSymPrim Sub) <$> goAST a <*> goAST b
      | Just Mul <- prj op = liftStruct2 (sugarSymPrim Mul) <$> goAST a <*> goAST b
      | Just Div <- prj op = liftStruct2 (sugarSymPrim Div) <$> goAST a <*> goAST b
      | Just Mod <- prj op = liftStruct2 (sugarSymPrim Mod) <$> goAST a <*> goAST b
      | Just Eq  <- prj op = liftStruct2 (sugarSymPrim Eq)  <$> goAST a <*> goAST b
      | Just And <- prj op = liftStruct2 (sugarSymPrim And) <$> goAST a <*> goAST b
      | Just Or  <- prj op = liftStruct2 (sugarSymPrim Or)  <$> goAST a <*> goAST b
      | Just Lt  <- prj op = liftStruct2 (sugarSymPrim Lt)  <$> goAST a <*> goAST b
      | Just Lte <- prj op = liftStruct2 (sugarSymPrim Lte) <$> goAST a <*> goAST b
      | Just Gt  <- prj op = liftStruct2 (sugarSymPrim Gt)  <$> goAST a <*> goAST b
      | Just Gte <- prj op = liftStruct2 (sugarSymPrim Gte) <$> goAST a <*> goAST b
      | Just BitAnd <- prj op =
          liftStruct2 (sugarSymPrim BitAnd) <$> goAST a <*> goAST b
      | Just BitOr  <- prj op =
          liftStruct2 (sugarSymPrim BitOr) <$> goAST a <*> goAST b
      | Just BitXor <- prj op =
          liftStruct2 (sugarSymPrim BitXor) <$> goAST a <*> goAST b
      | Just ShiftL <- prj op =
          liftStruct2 (sugarSymPrim ShiftL) <$> goAST a <*> goAST b
      | Just ShiftR <- prj op =
          liftStruct2 (sugarSymPrim ShiftR) <$> goAST a <*> goAST b          
      | Just RotateL <- prj op =
          liftStruct2 (sugarSymPrim RotateL) <$> goAST a <*> goAST b
      | Just RotateR <- prj op =
          liftStruct2 (sugarSymPrim RotateR) <$> goAST a <*> goAST b
    go _ arrIx (i :* Syn.Nil)
      | Just (ArrIx arr) <- prj arrIx
      = goSmallAST i >>= return . Node . sugarSymPrim (ArrIx arr)
    go _ s _ = error $ "hardware translation handling for symbol " ++ Syn.renderSym s ++ " is missing."

unsafeTranslateSmallExp :: Monad m => HExp a -> TargetT m (Prim a)
unsafeTranslateSmallExp a =
  do Node b <- translateExp a
     return b

--------------------------------------------------------------------------------

translate :: Hardware a -> ProgH a
translate = flip runReaderT Map.empty . Oper.reexpressEnv unsafeTranslateSmallExp . unHardware

--------------------------------------------------------------------------------
