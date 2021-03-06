
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
module Front where

import           SDL hiding (get)

import           AntiCarve
import           ConnectPoint
import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Carrier.SizeArray.IO
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Effect.Optics ((%=), (.=))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Array as A
import qualified Data.Array.IO as A
import           Data.Kind
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Vector.Mutable as V
import           FloodFill
import           Foreign.C.Types (CInt)
import           GHC.TypeLits
import           Room
import           SDL.Font as SF
import           SDL.Framerate hiding (get)
import           SDL.Primitive
import           SpanTree
import           System.Random (randomIO)
import qualified System.Random as R

initGUI :: CInt -> CInt -> IO (Renderer, Manager)
initGUI w h = do
  initializeAll
  window <-
    createWindow
      "rooms"
      WindowConfig
        { windowBorder = True,
          windowHighDPI = False,
          windowInputGrabbed = False,
          windowMode = Windowed,
          windowGraphicsContext = NoGraphicsContext,
          windowPosition = Wherever,
          windowResizable = True,
          windowInitialSize = V2 w h,
          windowVisible = True
        }
  renderer <- createRenderer window (-1) defaultRenderer
  fm <- SDL.Framerate.manager
  SDL.Framerate.set fm 60
  return (renderer, fm)

{-# INLINE renderAll #-}
renderAll :: (HasLabelled SizeArray (SizeArray Block) sig m,
              Has (Random :+: Error Skip
                          :+: State CPSet
                          :+: State FillStack
                          :+: State Bool
                          :+: State CPoints
                  ) sig m,
              MonadIO m)
          => Renderer
          -> Manager
          -> m ()
renderAll render manager = do

  createRooms 2000
  floodFill
  cp <- get @CPoints
  connectPoint cp
  spanTree
  antiCarve
  put True  -- is render ?

  w <- arrayWidth
  h <- arrayHeight
  let handler event =
        case eventPayload event of
          QuitEvent -> throwError Skip
          (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeEscape _))) -> throwError Skip
          (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeSpace _))) -> do

            sfor (\x y -> writeArray x y Empty)
            cpSet .= Set.empty
            put (FillStack [] Set.empty)
            put (CPoints [])

            createRooms 2000
            floodFill
            cp <- get @CPoints
            connectPoint cp
            spanTree
            antiCarve
            put True  -- is render ?

          _ -> return ()
  let go = do
         events <- liftIO pollEvents
         mapM_ handler events

         -- rendererDrawColor render $= V4 255 255 255 255
         get >>= \case
           False -> do
                delay_ manager
                go
           True -> do
                put False

                -- rendererDrawColor render $= V4 155 100 0 255
                rendererDrawColor render $= V4 0 0 0 255
                clear render


                sfor $ \x y -> do
                    readArray x y >>= \case
                      Empty -> pure ()
                      Road -> do
                           rendererDrawColor render $= V4 0 0 0 255
                           fillRect render
                                 (Just (Rectangle (P (V2 (fromIntegral x * blockWidth)
                                                   (fromIntegral y * blockWidth)))
                                                   (V2 blockWidth blockWidth) ))
                      Full -> do
                           rendererDrawColor render $= V4 0 0 255 255
                           drawRect render
                                 (Just (Rectangle (P (V2 (fromIntegral x * blockWidth)
                                                   (fromIntegral y * blockWidth)))
                                                   (V2 blockWidth blockWidth) ))
                      ConnPoint -> do
                           rendererDrawColor render $= V4 255 255 255 255
                           fillRect render
                                 (Just (Rectangle (P (V2 (fromIntegral x * blockWidth)
                                                   (fromIntegral y * blockWidth)))
                                                   (V2 blockWidth blockWidth) ))
                      Span -> do
                           -- rendererDrawColor render $= V4 100 155 255 255
                           -- drawRect render
                           rendererDrawColor render $= V4 255 255 0 255
                           drawRect render
                                 (Just (Rectangle (P (V2 (fromIntegral x * blockWidth)
                                                   (fromIntegral y * blockWidth)))
                                                   (V2 blockWidth blockWidth) ))
                present render
                delay_ manager
                go
  go


blockWidth = 10 :: CInt

rungen :: IO ()
rungen = do
  let w = 161 -- natVal @width Proxy
      h = 89 --  natVal @height Proxy
  (render, manager) <- initGUI (fromIntegral w * blockWidth) (fromIntegral h * blockWidth)

  -- arr <- liftIO $ A.newArray ((0,0), (w - 1, h - 1)) Empty
  arr <- liftIO $ V.replicate (fromIntegral $ w * h) Empty

  r <- randomIO
  -- let r = 10
  runRandom (R.mkStdGen r)
    $ runState @CPSet (CPSet Set.empty)
    $ runArray' w h arr
    $ runState (FillStack [] Set.empty)
    $ runState (CPoints [])
    $ runState False
    $ runError @Skip
    $ do
      renderAll render manager
  SDL.quit
  return ()

