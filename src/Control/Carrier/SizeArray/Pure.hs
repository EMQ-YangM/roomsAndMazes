{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Carrier.SizeArray.Pure (
    SizeArray
  , arrayHeight
  , arrayWidth
  , readArray
  , runArray
  , runArray'
  , sfor
  , writeArray
  ) where

import           Control.Arrow (Arrow (first))
import           Control.Carrier.Lift
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Effect.Optics (use)
import           Control.Effect.SizeArray
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import qualified Data.Array.IO as A
import           Data.Functor
import           Data.Kind
import           Data.Proxy
import           Data.Vector
import           GHC.TypeLits
import           Optics (makeLenses)
import           Prelude hiding (replicate)
import qualified System.Random as R

data InternalState e
  = InternalState
  { _width  :: Int
  , _heigh  :: Int
  , _vector :: Vector e
  }

makeLenses ''InternalState

newtype ArrayC e m a = ArrayC { runArrayC :: StateC (InternalState e) m a }
  deriving (Functor, Applicative , Monad , MonadIO )

instance (Algebra sig m, MonadIO m) => Algebra (SizeArray e :+: sig) (ArrayC e m) where
  alg hdl sig ctx =
    ArrayC $ case sig of
      L (ReadArray x y) -> StateC $ \(InternalState w h vec) -> pure (InternalState w h vec, (vec ! (x + y * w)) <$ ctx)
      L (WriteArray x y e) -> StateC $ \(InternalState w h vec) -> pure (InternalState w h (vec // [(x + y * w, e)]), ctx)
      L ArrayWidth -> StateC $ \(InternalState w h vec) -> pure (InternalState w h vec, w <$ ctx)
      L ArrayHeight -> StateC $ \(InternalState w h vec) -> pure (InternalState w h vec, h <$ ctx)
      R other               -> alg (runArrayC . hdl) (R other) ctx
  {-# INLINE alg #-}

{-# INLINE runArray #-}
runArray :: MonadIO m
         => Int
         -> Int
         -> e
         -> Labelled SizeArray (ArrayC e) m a
         -> m a
runArray w h e fun =
  let arr = replicate (w * h) e
  in snd <$> (runState (InternalState w h arr) . runArrayC . runLabelled $ fun)

{-# INLINE runArray' #-}
runArray' :: (MonadIO m)
         => Int
         -> Int
         -> Vector e
         -> Labelled SizeArray (ArrayC e) m a
         -> m (Vector e, a)
runArray' w h arr fun = do
  first _vector <$> (runState (InternalState w h arr) . runArrayC . runLabelled $ fun)
