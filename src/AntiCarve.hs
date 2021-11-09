
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
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Effect.Optics (use)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Array as A
import qualified Data.Array.IO as A
import           Data.Kind
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           GHC.TypeLits
import           Room
import           SizeArray
import           System.Random (randomIO)
import qualified System.Random as R

dr = [(1,0), (0, -1), (-1,0), (0,1)] :: [(Int, Int)]


antiCarve :: forall width height sig m.
             (IsOdd width, IsOdd height,
              HasLabelled SizeArray (SizeArray width height Block) sig m,
              Has (State FillStack) sig m)
          => m ()
antiCarve = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

      go (x, y) = do
            res <- forM dr $ \(dx, dy) -> do
              let nx = x+dx
                  ny = y+dy
              readArray nx ny >>= \case
                Span -> pure [(nx, ny)]
                _    -> pure []
            case concat res of
              [(nx, ny)] -> writeArray x y Empty >> go (nx, ny)
              _          -> pure ()

  ls <- Set.toList <$> use endPoint
  forM_ ls go

  forM_ [1 .. h-2] $ \y -> do
    forM_ [1 ..w-2] $ \x -> do
      r <- readArray x y
      case r of
        Road -> writeArray x y Empty
        _    -> pure ()

