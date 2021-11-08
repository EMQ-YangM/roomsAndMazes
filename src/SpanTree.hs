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
import           SizeArray
import           System.Random (randomIO)
import qualified System.Random as R

dr = [(1,0), (0, -1), (-1,0), (0,1)] :: [(Int, Int)]

newtype CPSet
  = CPSet { _cpSet :: Set (Int, Int) }
  deriving (Show)

makeLenses ''CPSet

spanTree :: forall width height sig m.
            (IsOdd width, IsOdd height,
             HasLabelled SizeArray (SizeArray width height Block) sig m,
             Has (Random :+: State CPSet) sig m,
             MonadIO m)
         => m ()
spanTree = do
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy
      getStart = do
        sx <- uniformR (1, w-1)
        sy <- uniformR (1, h-1)
        readArray sx sy >>= \case
          Full -> pure (sx, sy)
          _    -> getStart

  let fillFull p@(px, py) = do
        readArray px py >>= \case
          Full -> do
            writeArray px py Span
            forM_ dr $ \(dx,dy) -> fillFull (px + dx, py + dy)
          ConnPoint -> do
            cps <- use cpSet
            if Set.member p cps
              then do
                i <- uniformR (1, 100)
                if i < (10 :: Int)
                  then writeArray px py Span
                  else writeArray px py Empty
                -- writeArray px py Empty
                cpSet %= Set.delete p
              else cpSet %= Set.insert p
          _    -> pure ()

      selectAConnectPoint = do
        cps <- use cpSet
        let size = Set.size cps
        v <- uniformR (0, size - 1)
        let p@(px, py) = Set.elemAt v cps

        cpSet %= Set.delete p -- delete select point from set

        forM dr $ \(dx, dy) -> do
          let npx = px + dx
              npy = py + dy
              np = (npx, npy)
          readArray npx npy >>= \case
            Road -> pure [(Road, p, np)]
            Full -> pure [(Full, p, np)]
            _    -> pure []


      fillRoad p@(px, py) = do
        readArray px py >>= \case
          Road -> do
            writeArray px py Span
            forM_ dr $ \(dx,dy) -> fillRoad (px + dx, py + dy)
          ConnPoint -> do
            cps <- use cpSet
            if Set.member p cps
              then do
                i <- uniformR (1, 100)
                if i < (10 :: Int)
                  then writeArray px py Span
                  else writeArray px py Empty
                cpSet %= Set.delete p
              else cpSet %= Set.insert p
          _ -> pure ()

  sp <- getStart
  fillFull sp

  let go = do
        cps <- use cpSet
        if Set.null cps
          then pure ()
          else do
            res <- selectAConnectPoint
            case concat res of
              [(t, (px, py), np)] -> case t of
                Road -> writeArray px py Span >> fillRoad np
                Full -> writeArray px py Span >> fillFull np
                _    -> error "never happened"
              _        -> error "never happened"
            go
  go
