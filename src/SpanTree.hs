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
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module SpanTree where

import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Effect.Optics
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
import           Optics (makeLenses)
import           Room
import           System.Random (randomIO)
import qualified System.Random as R

dr = [(1,0), (0, -1), (-1,0), (0,1)] :: [(Int, Int)]

newtype CPSet
  = CPSet { _cpSet :: Set (Int, Int) }
  deriving (Show)

makeLenses ''CPSet


fillFull :: (HasLabelled SizeArray (SizeArray Block) sig m,
             Has (Random :+: State CPSet) sig m)
         => (Int, Int)
         -> m ()
fillFull p@(px, py) = do
  readArray px py >>= \case
    Full -> do
      writeArray px py Span
      forM_ dr $ \(dx,dy) -> fillFull (px + dx, py + dy)
    Road -> do
      writeArray px py Span
      forM_ dr $ \(dx,dy) -> fillFull (px + dx, py + dy)
    ConnPoint -> do
      cps <- use cpSet
      if Set.member p cps
        then do
          i <- uniformR (1, 100)
          if i < (7 :: Int)
            then do
              res <- forM dr $ \(dx, dy) -> do
                readArray (px + dx) (py + dy) >>= \case
                  Span -> pure 1
                  _    -> pure 0
              if sum res < 3
                then writeArray px py Span
                else writeArray px py Empty
            else writeArray px py Empty
          cpSet %= Set.delete p
        else cpSet %= Set.insert p
    _    -> pure ()

selectAConnectPoint :: (HasLabelled SizeArray (SizeArray Block) sig m,
                        Has (Random :+: State CPSet) sig m)
                    => m (Block, (Int,Int), (Int, Int))
selectAConnectPoint = do
  cps <- use cpSet
  let size = Set.size cps
  v <- uniformR (0, size - 1)
  let p@(px, py) = Set.elemAt v cps

  cpSet %= Set.delete p -- delete select point from set

  let go [] = error "never happened"
      go ((dx,dy) : ls) = do
           let npx = px + dx
               npy = py + dy
               np = (npx, npy)
           readArray npx npy >>= \case
             Road -> pure (Road, p, np)
             Full -> pure (Full, p, np)
             _    -> go ls
  go dr

spanTree :: (HasLabelled SizeArray (SizeArray Block) sig m,
             Has (Random :+: State CPSet) sig m)
         => m ()
spanTree = do

  w <- arrayWidth
  h <- arrayHeight

  let getStart = do
        sx <- uniformR (1, w-1)
        sy <- uniformR (1, h-1)
        readArray sx sy >>= \case
          Full -> pure (sx, sy)
          _    -> getStart

  sp <- getStart
  fillFull sp

  let go = do
        cps <- use cpSet
        if Set.null cps
          then pure ()
          else do
            (t, (px, py), np) <- selectAConnectPoint
            case t of
               Road -> writeArray px py Span >> fillFull np
               Full -> writeArray px py Span >> fillFull np
               _    -> error "never happened"
            go
  go
