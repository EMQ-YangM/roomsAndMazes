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

ddd :: (Int, Int) -> [((Int, Int),[(Int, Int)])]
ddd (x, y) =
  [ ((x+1, y), [(x, y+1), (x, y-1), (x+1, y), (x+1, y+1), (x+1, y-1)])   -- right
  , ((x, y-1), [(x+1, y), (x-1, y), (x, y-1), (x+1, y-1), (x-1, y-1)])  -- down
  , ((x-1, y), [(x, y+1), (x, y-1), (x-1, y), (x-1, y+1), (x-1, y-1)])  -- left
  , ((x, y+1), [(x+1, y), (x-1, y), (x, y+1), (x+1, y+1), (x-1, y+1)])  -- up
  ]


checkValue :: forall width height sig m.
              (IsOdd width, IsOdd height,
               HasLabelled SizeArray (SizeArray width height Block) sig m,
               Has (Random :+: Error Skip) sig m,
               MonadIO m)
           => (Int, Int)
           -> m [(Int, Int)]
checkValue ps@(x, y) = do
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

  if x > 0 && x < w-1 &&
     y > 0 && y < h-1
    then do
      cv <- readArray x y
      if cv /= Empty
        then pure []
        else do
          res <- forM (ddd ps) $ \(kk, ks) -> do
            catchError @Skip
               (do forM_ ks $ \(kx, ky) ->
                     readArray kx ky >>= \case
                       Empty -> pure ()
                       Road  -> throwError Skip
                       Full  -> throwError Skip

                   pure [kk]
               ) (\_ -> pure [] )
          pure $ concat res
    else pure []

floodFill :: forall width height sig m.
             (IsOdd width, IsOdd height,
              HasLabelled SizeArray (SizeArray width height Block) sig m,
              Has (Random :+: Error Skip) sig m,
              MonadIO m)
          => m ()
floodFill = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

      go ss@(sx, sy) = do
        res <- checkValue ss
        if null res
          then pure ()
          else do
            writeArray sx sy Road
            forM_ [(sx + jx, sy + jy) | (jx,jy) <- dr] go

  forM_ [1, 3 .. h-1] $ \y -> do
    forM_ [1, 3 ..w-1] $ \x -> do
      res <- checkValue (x, y)
      if null res
        then pure ()
        else go (x, y)


















