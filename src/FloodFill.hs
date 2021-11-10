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
import           Control.Effect.Optics (use, (%=), (.=))
import           Control.Effect.SizeArray
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
import           System.Random (mkStdGen, randomIO)
import qualified System.Random as R

dr = [(1,0), (0, -1), (-1,0), (0,1)] :: [(Int, Int)]

{-# INLINE checkStartPoint #-}
checkStartPoint :: (HasLabelled SizeArray (SizeArray Block) sig m)
                => (Int, Int)
                -> m Bool
checkStartPoint ps@(x, y) = do
  readArray x y >>= \case
    Empty -> do
      res <- forM dr $ \(dx,dy) -> readArray (x + dx) (y + dy)
      pure $ all (==Empty) res
    _ -> pure False


{-# INLINE dtn #-}
dtn :: (HasLabelled SizeArray (SizeArray Block) sig m)
    => Int -> (Int, Int)   -- start point
    -> (Int, Int)   -- dir dx dy
    -> m Bool
dtn i ps@(x, y) dir@(dx, dy) = do
  w <- arrayWidth
  h <- arrayHeight
  let nx = x + dx * i
      ny = y + dy * i

  if nx >= 0 && nx <= w-1 &&
     ny >= 0 && ny <= h-1
    then readArray nx ny >>= \case
           Empty -> pure True
           _     -> pure False
    else pure False


dirFill :: (HasLabelled SizeArray (SizeArray Block) sig m,
            Has (State FillStack) sig m)
        => (Int, Int)   -- start point
        -> (Int, Int)   -- dir dx dy
        -> m ()
dirFill ps@(x, y) dir@(dx, dy) = do

  readArray x y >>= \case
    Empty -> do
      writeArray x y Road
      dtn 2 ps dir >>= \case
        True  -> dirFill (x + dx, y + dy) (dx, dy)
        False -> do
          let rx = dy
              ry = dx

          dtn 2 ps (rx, ry) >>= \case
            True  -> fillStack %= (((x + rx, y + ry), (rx, ry)) :)
            False -> pure ()

          dtn 2 ps (-rx, -ry) >>= \case
            True  -> fillStack %= (((x - rx, y - ry), (-rx, -ry)) :)
            False -> endPoint %= Set.insert (x, y)
    _ -> pure ()

floodFill :: (HasLabelled SizeArray (SizeArray Block) sig m,
              Has (Error Skip :+: State FillStack) sig m)
          => m ()
floodFill = do
  w <- arrayWidth
  h <- arrayHeight

  forM_ [1, 3 .. h-1] $ \y -> do
    forM_ [1, 3 ..w-1] $ \x -> do
      checkStartPoint (x, y) >>= \case
        False -> pure ()
        True -> do
          endPoint %= Set.insert (x, y)
          dirFill (x, y) (1, 0)
          let go = do
               res <- use fillStack
               case res of
                 [] -> pure ()
                 (ps, dir):xs -> do
                   fillStack .= xs
                   dtn 1 ps dir >>= \case
                     True  -> dirFill ps dir
                     False -> pure ()
                   go
          go

















