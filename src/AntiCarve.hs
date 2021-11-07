
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module AntiCarve where

import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Array as A
import qualified Data.Array.IO as A
import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits
import           Room
import           SizeArray
import           System.Random (randomIO)
import qualified System.Random as R

dr = [(1,0), (0, -1), (-1,0), (0,1)] :: [(Int, Int)]

antiCarve :: forall width height sig m.
             (IsOdd width, IsOdd height,
              HasLabelled SizeArray (SizeArray width height Block) sig m,
              Has (Random :+: Error Skip) sig m,
              MonadIO m)
          => m ()
antiCarve = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

      go (x, y) = do
        readArray x y >>= \case
          Span -> do

            res <- forM dr $ \(dx, dy) -> do
              let nx = x+dx
                  ny = y+dy
              readArray nx ny >>= \case
                Empty -> pure (1, [])
                Span  -> pure (0, [(nx, ny)])
                _     -> pure (0, [])

            let (a,b) = unzip res

            when (sum a == 3) $ do
              case concat b of
                [kv] -> writeArray x y Empty >> go kv
                _    -> pure ()
          _     -> pure ()

  forM_ [1 .. h-2] $ \y -> do
    forM_ [1 ..w-2] $ \x -> do
      go (x, y)

