{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Control.Effect.Array
  where

import           Control.Effect.Labelled
import           Data.Kind

data Array i e (m :: Type -> Type) a where
  ReadArray :: i -> Array i e m e
  WriteArray :: i -> e -> Array i e m ()

readArray :: HasLabelled Array (Array i e) sig m => i -> m e
readArray  = sendLabelled @Array . ReadArray

writeArray :: HasLabelled Array (Array i e) sig m => i -> e -> m ()
writeArray i e = sendLabelled @Array (WriteArray i e)
