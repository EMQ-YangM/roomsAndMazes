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

module ConnectPoint where

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

connectPoint :: forall width height sig m.
                (IsOdd width, IsOdd height,
                 HasLabelled SizeArray (SizeArray width height Block) sig m)
             => m ()
connectPoint = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

  forM_ [1 .. h-2] $ \y -> do
    forM_ [1 ..w-2] $ \x -> do
      readArray x y >>= \case
        Full      -> pure ()
        Road      -> pure ()
        ConnPoint -> pure ()
        Span      -> pure ()
        Empty     -> do
          res <- forM dr $ \(dx, dy) -> do
            readArray (x+dx) (y+dy) >>= \case
              Full -> return 10
              Road -> return 1
              _    -> return 0
          if sum res > 10
            then writeArray x y ConnPoint
            else pure ()


