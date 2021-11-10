{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Effect.SizeArray where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Effect.Labelled
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import qualified Data.Array.IO as A
import           Data.Functor
import           Data.Kind
import           Data.Proxy
import           Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as V
import           GHC.TypeLits
import qualified System.Random as R

data SizeArray e (m :: Type -> Type) a where
  ArrayWidth  :: SizeArray e m Int
  ArrayHeight :: SizeArray e m Int
  ReadArray   :: Int -> Int -> SizeArray e m e
  WriteArray  :: Int -> Int -> e -> SizeArray e m ()

{-# INLINE readArray #-}
readArray :: HasLabelled SizeArray (SizeArray e) sig m => Int -> Int -> m e
readArray x y = sendLabelled @SizeArray (ReadArray x y)

{-# INLINE writeArray #-}
writeArray :: HasLabelled SizeArray (SizeArray e) sig m => Int -> Int -> e -> m ()
writeArray x y e = sendLabelled @SizeArray (WriteArray x y e)

{-# INLINE arrayWidth #-}
arrayWidth :: HasLabelled SizeArray (SizeArray e) sig m => m Int
arrayWidth = sendLabelled @SizeArray ArrayWidth

{-# INLINE arrayHeight #-}
arrayHeight :: HasLabelled SizeArray (SizeArray e) sig m => m Int
arrayHeight = sendLabelled @SizeArray ArrayHeight

{-# INLINE sfor #-}
sfor :: (HasLabelled SizeArray (SizeArray e) sig m)
     => (Int -> Int -> m ())
     -> m ()
sfor f = do
  w <- arrayWidth
  h <- arrayHeight
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      f x y
