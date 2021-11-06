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
module SizeArray
  where

import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Effect.Labelled
import           Control.Monad (forM_)
import           Control.Monad.IO.Class
import qualified Data.Array.IO as A
import           Data.Functor
import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits
import qualified System.Random as R


data SizeArray (width :: Nat) (height :: Nat) e (m :: Type -> Type) a where
  ReadArray :: Int -> Int -> SizeArray width height e m e
  WriteArray :: Int -> Int -> e -> SizeArray width height e m ()

readArray :: HasLabelled SizeArray (SizeArray width height e) sig m => Int -> Int -> m e
readArray x y = sendLabelled @SizeArray (ReadArray x y)

writeArray :: HasLabelled SizeArray (SizeArray width height e) sig m => Int -> Int -> e -> m ()
writeArray x y e = sendLabelled @SizeArray (WriteArray x y e)

sfor :: forall width height sig m e sym.
        (HasLabelled SizeArray (SizeArray width height e) sig m,
         KnownNat width, KnownNat height)
     => (Int -> Int -> m ())
     -> m ()
sfor f = do
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      f x y

newtype ArrayC (width :: Nat) (height :: Nat) e m a = ArrayC { runArrayC :: ReaderC (A.IOArray (Int,Int) e) m a }
  deriving (Functor, Applicative , Monad , MonadIO )

instance (Algebra sig m, MonadIO m) => Algebra (SizeArray width height e :+: sig) (ArrayC width height e m) where
  alg hdl sig ctx = ArrayC $ case sig of
    L (ReadArray x y)    -> ReaderC $ \arr -> liftIO (A.readArray arr (x, y)) <&> (<$ ctx)
    L (WriteArray x y e ) -> ReaderC $ \arr -> liftIO (A.writeArray arr (x, y) e) >> pure ctx
    R other     -> alg (runArrayC . hdl) (R other) ctx

runArray :: forall m e width height a sym.
            (MonadIO m,
             KnownNat height,
             KnownNat width)
         => e
         -> Labelled SizeArray (ArrayC width height e) m a
         -> m a
runArray e fun = do
  let w = natVal @width Proxy
      h = natVal @height Proxy
  arr <- liftIO $ A.newArray ((0,0), (fromIntegral w - 1, fromIntegral  h - 1)) e
  runReader arr . runArrayC . runLabelled $ fun

runArray' :: forall m e width height a sym.
            (MonadIO m,
             KnownNat height,
             KnownNat width)
         => A.IOArray (Int, Int) e
         -> Labelled SizeArray (ArrayC width height e) m a
         -> m a
runArray' arr fun = do
  runReader arr . runArrayC . runLabelled $ fun
