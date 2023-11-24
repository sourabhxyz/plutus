-- | The main module of the library.

module Text.PrettyBy
    ( PrettyBy (..)
    , IgnorePrettyConfig (..)
    , InnerPrettyConfig (..)
    , InnerPrettyBy
    , AttachPrettyConfig (..)
    , PrettyAny (..)
    , withAttachPrettyConfig
    , defaultPrettyFunctorBy
    , defaultPrettyBifunctorBy
    , NonDefaultPrettyBy (..)
    , HasPrettyDefaults
    , PrettyDefaultBy
    , Render (..)
    , display
    , displayBy
    ) where

import Text.PrettyBy.Default
import Text.PrettyBy.Internal
