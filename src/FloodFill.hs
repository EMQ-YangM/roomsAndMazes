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
module FloodFill where

import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
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
import           Shuffle
import           SizeArray
import           System.Random (mkStdGen, randomIO)
import qualified System.Random as R

dr = [(1,0), (0, -1), (-1,0), (0,1)] :: [(Int, Int)]

checkStartPoint :: forall width height sig m.
                   (IsOdd width, IsOdd height,
                    HasLabelled SizeArray (SizeArray width height Block) sig m,
                    MonadIO m)
                => (Int, Int)
                -> m Bool
checkStartPoint ps@(x, y) = do
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy
  readArray x y >>= \case
    Empty -> do
      res <- forM dr $ \(dx,dy) -> readArray (x + dx) (y + dy)
      pure $ all (==Empty) res
    _ -> pure False


{-# INLINE dt2 #-}
dt2 :: forall width height sig m.
       (IsOdd width, IsOdd height,
        HasLabelled SizeArray (SizeArray width height Block) sig m,
        MonadIO m)
    => (Int, Int)   -- start point
    -> (Int, Int)   -- dir dx dy
    -> m Bool
dt2 ps@(x, y) dir@(dx, dy) = do
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

      nx = x + dx * 2
      ny = y + dy * 2

  if nx >= 0 && nx <= w-1 &&
     ny >= 0 && ny <= h-1
    then readArray nx ny >>= \case
           Empty -> pure True
           _     -> pure False
    else pure False

dirFill :: forall width height sig m.
              (IsOdd width, IsOdd height,
               HasLabelled SizeArray (SizeArray width height Block) sig m,
               Has (State (Set (Int, Int))) sig m,
               -- Has (Error Skip :+: State [((Int, Int), (Int, Int))]) sig m,
               MonadIO m)
           => (Int, Int)   -- start point
           -> (Int, Int)   -- dir dx dy
           -> m ()
dirFill ps@(x, y) dir@(dx, dy) = do
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

  writeArray x y Road
  dt2 ps dir >>= \case
    True  -> dirFill (x + dx, y + dy) (dx, dy)
    False -> do
      let rx = dy
          ry = dx

      dt2 ps (rx, ry) >>= \case
        True  -> dirFill (x + rx, y + ry) (rx, ry)
        False -> pure ()

      dt2 ps (-rx, -ry) >>= \case
        True  -> dirFill (x - rx, y - ry) (-rx, -ry)
        False -> modify (Set.insert (x, y))

floodFill :: forall width height sig m.
             (IsOdd width, IsOdd height,
              HasLabelled SizeArray (SizeArray width height Block) sig m,
              Has (Error Skip :+: State (Set (Int, Int))) sig m,
              MonadIO m)
          => m ()
floodFill = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

  forM_ [1, 3 .. h-1] $ \y -> do
    forM_ [1, 3 ..w-1] $ \x -> do
      checkStartPoint (x, y) >>= \case
        False -> pure ()
        True -> do
          modify (Set.insert (x, y))
          dirFill (x, y) (1, 0)

















