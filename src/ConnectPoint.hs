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
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Effect.SizeArray
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Array as A
import qualified Data.Array.IO as A
import           Data.Kind
import           Data.Proxy
import qualified Data.Set as Set
import           GHC.TypeLits
import           Room
import           System.Random (randomIO)
import qualified System.Random as R

dr = [(1,0), (0, -1), (-1,0), (0,1)] :: [(Int, Int)]

connectPoint :: (HasLabelled SizeArray (SizeArray Block) sig m)
             => CPoints
             -> m ()
connectPoint (CPoints s) = do

  w <- arrayWidth
  h <- arrayHeight

  let go c p@(x,y) ds = do
        if c > 10
          then writeArray x y ConnPoint
          else do
             case ds of
               [] -> pure ()
               (dx, dy) : ls -> do
                    readArray (x+dx) (y+dy) >>= \case
                      Full -> go (c + 10) p ls
                      Road -> go (c + 1) p ls
                      _    -> go c p ls

  forM_ s $ \(x, y) -> do
      readArray x y >>= \case
        Full      -> pure ()
        Road      -> pure ()
        ConnPoint -> pure ()
        Span      -> pure ()
        Empty     -> do
           if x > 0 && x < w-1 &&
              y > 0 && y < h-1
             then  go 0 (x, y) dr
             else pure ()

