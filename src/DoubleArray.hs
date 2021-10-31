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
module DoubleArray
  where


import           Control.Carrier.Lift
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import qualified Data.Array.IO as A
import           Data.Functor
import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits
import qualified System.Random as R


data DoubleArray (width :: Nat) (height :: Nat) e (m :: Type -> Type) a where
  ReadArray :: Int -> Int -> DoubleArray width height e m e
  WriteArray :: Int -> Int -> e -> DoubleArray width height e m ()
  SwapArray :: DoubleArray width height e m ()

readArray :: HasLabelled DoubleArray (DoubleArray width height e) sig m => Int -> Int -> m e
readArray x y = sendLabelled @DoubleArray (ReadArray x y)

writeArray :: HasLabelled DoubleArray (DoubleArray width height e) sig m => Int -> Int -> e -> m ()
writeArray x y e = sendLabelled @DoubleArray (WriteArray x y e)

swapArray :: HasLabelled DoubleArray (DoubleArray width height e) sig m => m ()
swapArray = sendLabelled @DoubleArray SwapArray

newtype ArrayC (width :: Nat) (height :: Nat) e m a = ArrayC { runArrayC :: StateC (A.IOArray (Int,Int) e, A.IOArray (Int,Int) e) m a }
  deriving (Functor, Applicative , Monad , MonadIO )

instance (Algebra sig m, MonadIO m) => Algebra (DoubleArray width height e :+: sig) (ArrayC width height e m) where
  alg hdl sig ctx = ArrayC $ case sig of
    L (ReadArray x y)    -> StateC $ \s@(larr, rarr) -> do
      v <- liftIO (A.readArray larr (x, y))
      pure (s, v <$ ctx)
    L (WriteArray x y e ) -> StateC $ \s@(larr, rarr) -> liftIO (A.writeArray rarr (x, y) e) >> pure (s, ctx)
    L SwapArray -> StateC $ \(larr, rarr) -> pure ((rarr,larr), ctx)
    R other     -> alg (runArrayC . hdl) (R other) ctx

runArray :: forall m e width height a sym.
            (MonadIO m,
             KnownNat height,
             KnownNat width)
         => e
         -> Labelled DoubleArray (ArrayC width height e) m a
         -> m a
runArray e fun = do
  let w = natVal @width Proxy
      h = natVal @height Proxy
  arr <- liftIO $ A.newArray ((0,0), (fromIntegral w - 1, fromIntegral  h - 1)) e
  arr1 <- liftIO $ A.newArray ((0,0), (fromIntegral w - 1, fromIntegral  h - 1)) e
  evalState (arr, arr1) . runArrayC . runLabelled $ fun

sfor :: forall width height sig m e sym.
        (HasLabelled DoubleArray (DoubleArray width height e) sig m,
         KnownNat width, KnownNat height)
     => (Int -> Int -> m ())
     -> m ()
sfor f = do
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      f x y
