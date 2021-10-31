{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Control.Carrier.Array.IO
  ( runArray
    -- reExport
  , module Control.Effect.Array
  ) where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Array
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Array.IO as A
import           Data.Functor

newtype ArrayC i e m a = ArrayC { runArrayC :: ReaderC (A.IOArray i e) m a }
  deriving (Functor, Applicative , Monad , MonadIO )

instance (Algebra sig m, MonadIO m, A.Ix i) => Algebra (Array i e :+: sig) (ArrayC i e m) where
  alg hdl sig ctx = ArrayC $ case sig of
    L (ReadArray i)     -> ReaderC $ \arr -> liftIO (A.readArray arr i) <&> (<$ ctx)
    L (WriteArray i e ) -> ReaderC $ \arr -> liftIO (A.writeArray arr i e) >> pure ctx
    R other             -> alg (runArrayC . hdl) (R other) ctx

runArray :: A.IOArray i e -> Labelled Array (ArrayC i e) m a -> m a
runArray arr = runReader arr . runArrayC . runLabelled

-- example :: (HasLabelled Array (Array Int Int) sig m,
--             MonadIO m)
--         => m ()
-- example = do
--   forM_ [1..10] $ \i -> do
--     v <- readArray i
--     liftIO $ print v
--     writeArray i i
--     v <- readArray i
--     liftIO $ print v

-- runExample :: IO ()
-- runExample = do
--   arr <- A.newArray (1,20) 0
--   runM @IO $ runArray arr example

