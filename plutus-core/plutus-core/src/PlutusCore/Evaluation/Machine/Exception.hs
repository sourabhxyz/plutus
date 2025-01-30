-- editorconfig-checker-disable-file
-- | The exceptions that an abstract machine can throw.

-- appears in the generated instances
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module PlutusCore.Evaluation.Machine.Exception
    ( UnliftingError (..)
    , AsUnliftingError (..)
    , BuiltinError (..)
    , MachineError (..)
    , AsMachineError (..)
    , EvaluationError (..)
    , AsEvaluationError (..)
    , ErrorWithCause (..)
    , EvaluationException
    , throwNotAConstant
    , throwing
    , throwing_
    , throwingWithCause
    , splitStructuralOperational
    , unsafeSplitStructuralOperational
    ) where

import PlutusPrelude

import PlutusCore.Builtin.Result
import PlutusCore.Evaluation.ErrorWithCause
import PlutusCore.Evaluation.Result
import PlutusCore.Pretty

import Control.Lens
import Data.Either.Extras
import Data.Word (Word64)
import Prettyprinter

-- | Errors which can occur during a run of an abstract machine.
data MachineError fun
    = NonPolymorphicInstantiationMachineError
      -- ^ An attempt to reduce a not immediately reducible type instantiation.
    | NonWrapUnwrappedMachineError
      -- ^ An attempt to unwrap a not wrapped term.
    | NonFunctionalApplicationMachineError
      -- ^ An attempt to reduce a not immediately reducible application.
    | OpenTermEvaluatedMachineError
      -- ^ An attempt to evaluate an open term.
    | UnliftingMachineError UnliftingError
      -- ^ An attempt to compute a constant application resulted in 'UnliftingError'.
    | BuiltinTermArgumentExpectedMachineError
      -- ^ A builtin expected a term argument, but something else was received.
    | UnexpectedBuiltinTermArgumentMachineError
      -- ^ A builtin received a term argument when something else was expected
    | NonConstrScrutinized
    | MissingCaseBranch Word64
    | NonLambdaFixedMachineError
    | FixLoopMachineError
    deriving stock (Show, Eq, Functor, Generic)
    deriving anyclass (NFData)

mtraverse makeClassyPrisms
    [ ''MachineError
    ]

instance structural ~ MachineError fun =>
        AsMachineError (EvaluationError structural operational) fun where
    _MachineError = _StructuralEvaluationError
    {-# INLINE _MachineError #-}

instance AsUnliftingError (MachineError fun) where
    _UnliftingError = _UnliftingMachineError
    {-# INLINE _UnliftingError #-}

type EvaluationException structural operational =
    ErrorWithCause (EvaluationError structural operational)

{- Note [Ignoring context in OperationalEvaluationError]
The 'OperationalEvaluationError' error has a term argument, but 'splitStructuralOperational' just
discards this and returns 'EvaluationFailure'. This means that, for example, if we use the @plc@
command to execute a program containing a division by zero, @plc@ exits silently without reporting
that anything has gone wrong (but returning a non-zero exit code to the shell via 'exitFailure').
This is because 'OperationalEvaluationError' is used in cases when a PLC program itself goes wrong
(see the Haddock of 'EvaluationError'). This is used to signal unsuccessful validation and so is
not regarded as a real error; in contrast structural errors are genuine errors and we report their
context if available.
-}

-- See the Haddock of 'EvaluationError' for what structural and operational errors are.
-- See Note [Ignoring context in OperationalEvaluationError].
-- | Preserve the contents of an 'StructuralEvaluationError' as a 'Left' and turn an
-- 'OperationalEvaluationError' into a @Right EvaluationFailure@ (thus erasing the content of the
-- error in the latter case).
splitStructuralOperational
    :: Either (EvaluationException structural operational term) a
    -> Either (ErrorWithCause structural term) (EvaluationResult a)
splitStructuralOperational (Right term) = Right $ EvaluationSuccess term
splitStructuralOperational (Left (ErrorWithCause evalErr cause)) = case evalErr of
    StructuralEvaluationError err -> Left $ ErrorWithCause err cause
    OperationalEvaluationError _  -> Right EvaluationFailure

-- | Throw on a 'StructuralEvaluationError' and turn an 'OperationalEvaluationError' into an
-- 'EvaluationFailure' (thus erasing the content of the error in the latter case).
unsafeSplitStructuralOperational
    :: (PrettyPlc structural, PrettyPlc term, Typeable structural, Typeable term)
    => Either (EvaluationException structural operational term) a
    -> EvaluationResult a
unsafeSplitStructuralOperational = unsafeFromEither . splitStructuralOperational

instance (HasPrettyDefaults config ~ 'True, Pretty fun) =>
            PrettyBy config (MachineError fun) where
    prettyBy _      NonPolymorphicInstantiationMachineError =
        "Attempted to instantiate a non-polymorphic term."
    prettyBy _      NonWrapUnwrappedMachineError          =
        "Cannot unwrap a not wrapped term."
    prettyBy _      NonFunctionalApplicationMachineError   =
        "Attempted to apply a non-function."
    prettyBy _      OpenTermEvaluatedMachineError         =
        "Cannot evaluate an open term"
    prettyBy _      BuiltinTermArgumentExpectedMachineError =
        "A builtin expected a term argument, but something else was received"
    prettyBy _      UnexpectedBuiltinTermArgumentMachineError =
        "A builtin received a term argument when something else was expected"
    prettyBy _      (UnliftingMachineError unliftingError)  =
        pretty unliftingError
    prettyBy _      NonConstrScrutinized =
        "A non-constructor value was scrutinized in a case expression"
    prettyBy _      (MissingCaseBranch i) =
        "Case expression missing the branch required by the scrutinee tag:" <+> pretty i
    prettyBy _      NonLambdaFixedMachineError =
        "Attempted to take the fixed-point of a non-lambda term"
    prettyBy _      FixLoopMachineError =
        "Fix entered infinite recursion"
