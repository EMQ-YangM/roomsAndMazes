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
module Control.Carrier.SizeArray.IO (
    arrayHeight
  , arrayWidth
  , readArray
  , runArray
  , runArray'
  , sfor
  , writeArray
  , SizeArray
  ) where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Effect.Labelled
import           Control.Effect.SizeArray
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


newtype ArrayC e m a = ArrayC { runArrayC :: ReaderC (Int, Int, IOVector e) m a }
  deriving (Functor, Applicative , Monad , MonadIO )

instance (Algebra sig m, MonadIO m) => Algebra (SizeArray e :+: sig) (ArrayC e m) where
  alg hdl sig ctx =
    ArrayC $ case sig of
      L (ReadArray x y)     -> ReaderC $ \(w,h,arr) -> liftIO (V.unsafeRead arr (x + y * w)) <&> (<$ ctx)
      L (WriteArray x y e ) -> ReaderC $ \(w,h,arr) -> liftIO (V.unsafeWrite arr (x + y * w) e) >> pure ctx
      L ArrayWidth          -> ReaderC $ \(w,h,arr) -> pure (w <$ ctx)
      L ArrayHeight         -> ReaderC $ \(w,h,arr) -> pure (h <$ ctx)
      R other               -> alg (runArrayC . hdl) (R other) ctx
  {-# INLINE alg #-}

{-# INLINE runArray #-}
runArray :: MonadIO m
         => Int
         -> Int
         -> e
         -> Labelled SizeArray (ArrayC e) m a
         -> m a
runArray w h e fun = do
  arr <- liftIO $ V.replicate (w * h) e
  runReader (w,h,arr) . runArrayC . runLabelled $ fun

{-# INLINE runArray' #-}
runArray' :: (MonadIO m)
         => Int
         -> Int
         -> IOVector e
         -> Labelled SizeArray (ArrayC e) m a
         -> m a
runArray' w h arr fun = do
  runReader (w, h, arr) . runArrayC . runLabelled $ fun
