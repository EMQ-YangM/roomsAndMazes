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
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module GenerateImage where

import           SDL hiding (get)

import           AntiCarve
import           Codec.Picture
import           Codec.Picture.Bitmap
import           ConnectPoint
import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Effect.Optics ((%=), (.=))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Array as A
import           Data.Array.Base (unsafeFreezeIOArray)
import qualified Data.Array.IO as A
import           Data.Kind
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time
import           Data.Vector (unsafeFreeze, (!))
import qualified Data.Vector.Mutable as V
import           Data.Word (Word8)
import           FloodFill
import           Foreign.C.Types (CInt)
import           GHC.TypeLits
import           Room
import           SDL.Font as SF
import           SDL.Framerate hiding (get)
import           SDL.Primitive
import           Control.Carrier.SizeArray.IO
import           SpanTree
import           System.Process
import           System.Random (randomIO)
import qualified System.Random as R

createAll :: (HasLabelled SizeArray (SizeArray Block) sig m,
              MonadIO m)
          => Int -> m ()
createAll gen = do
  (gen1, (cp, _)) <-runRandom (R.mkStdGen gen)
            $ runState (CPoints [])
            $ runError @Skip
            $ withTime "create rooms"
            $ createRooms 1000000

  (s, _) <- runState (FillStack [] Set.empty)
            $ runError @Skip
            $ withTime "flood fill" floodFill

  withTime "connect point" (connectPoint cp)

  runRandom gen1
            $ runState @CPSet (CPSet Set.empty)
            $ withTime "span tree" spanTree

  runState s $ withTime "anti carve" antiCarve

  return ()

t :: Block -> Word8
t Span  = 0
t Empty = 255
t e     = error $ show e


withTime :: MonadIO m
         => String
         -> m ()
         -> m ()
withTime s f = do
  t1 <- liftIO getCurrentTime
  f
  t2 <- liftIO getCurrentTime
  liftIO $ putStrLn $ s ++ ": " ++ show (diffUTCTime t2 t1)

rungen :: IO ()
rungen = do
  let w = 2011
      h = 2011

  t1 <- getCurrentTime

  vec <- liftIO $ V.replicate (fromIntegral $ w * h) Empty
  -- r <- randomIO
  let r = 10
  runArray' w h vec
    $ createAll r
  newArr <- unsafeFreeze vec
  let img = generateImage @Pixel8 (\x y -> t $ newArr ! (x + y * w))  w h
  writeBitmap "bigMap.bmp" img
  t2 <- getCurrentTime
  print $ diffUTCTime t2 t1
  return ()
