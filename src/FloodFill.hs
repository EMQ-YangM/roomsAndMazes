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
{-# INLINE dr #-}


ddd :: (Int, Int) -> [[(Int, Int)]]
ddd (x, y) =
  [ [(x, y+1), (x, y-1), (x+1, y), (x+1, y+1), (x+1, y-1)]   -- right
  , [(x+1, y), (x-1, y), (x, y-1), (x+1, y-1), (x-1, y-1)]  -- down
  , [(x, y+1), (x, y-1), (x-1, y), (x-1, y+1), (x-1, y-1)]  -- left
  , [(x+1, y), (x-1, y), (x, y+1), (x+1, y+1), (x-1, y+1)]  -- up
  ]
{-# INLINE ddd #-}

checkStartPoint :: forall width height sig m.
                   (IsOdd width, IsOdd height,
                    HasLabelled SizeArray (SizeArray width height Block) sig m,
                    Has (Random :+: Error Skip) sig m,
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


{-# INLINE checkValue #-}
checkValue :: forall width height sig m.
              (IsOdd width, IsOdd height,
               HasLabelled SizeArray (SizeArray width height Block) sig m,
               Has (Random :+: Error Skip) sig m,
               MonadIO m)
           => (Int, Int)
           -> m Bool
checkValue ps@(x, y) = do
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

  if x > 0 && x < w-1 &&
     y > 0 && y < h-1
    then do
      cv <- readArray x y
      if cv /= Empty
        then pure False
        else catchError @Skip  (do
            forM_ (ddd ps) $ \ks ->
              catchError @Skip (do
                forM_ ks $ \(kx, ky) -> do
                  readArray kx ky >>= \case
                    Empty     -> pure ()
                    Road      -> throwError Skip
                    Full      -> throwError Skip
                    ConnPoint -> throwError Skip
                    Span      -> throwError Skip
                throwError SkipCM) (\case
                                       Skip    -> pure ()
                                       SkipCM  -> throwError Skip
                                       SkipM _ -> error "never happened")
            pure False) (\_ -> pure True)
    else pure False

floodFill :: forall width height sig m.
             (IsOdd width, IsOdd height,
              HasLabelled SizeArray (SizeArray width height Block) sig m,
              Has (Random :+: Error Skip :+: State (Set (Int, Int))) sig m,
              MonadIO m)
          => m ()
floodFill = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

      go s@(sx, sy) = do
            writeArray sx sy Road
            xs <- catchError @Skip (do forM_ [(sx + jx, sy + jy) | (jx,jy) <- dr] $ \x -> do
                                         r <- checkValue x
                                         when r $ throwError (SkipM x)
                                       pure Nothing
                                   ) (\(SkipM x) -> pure (Just x))

            case xs of
              Just x  -> go x
              Nothing -> modify (Set.insert s) >> pure ()

  forM_ [1, 3 .. h-1] $ \y -> do
    forM_ [1, 3 ..w-1] $ \x -> do
      checkStartPoint (x, y) >>= \case
        False -> pure ()
        True -> do
          modify (Set.insert (x, y))
          go (x, y)

















