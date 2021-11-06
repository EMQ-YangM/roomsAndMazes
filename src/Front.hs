
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE LambdaCase #-}
module Front where

import           SDL

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
import qualified Data.Text as T
import           Foreign.C.Types (CInt)
import           GHC.TypeLits
import           Room
import           FloodFill
import           SDL.Font as SF
import           SDL.Framerate
import           SDL.Primitive
import           SizeArray
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
  SDL.Framerate.set fm 5
  return (renderer, fm)


renderAll :: forall width height sig m.
             (IsOdd width, IsOdd height,
              HasLabelled SizeArray (SizeArray width height Block) sig m,
              Has (Random :+: Error Skip) sig m,
              MonadIO m)
          => Renderer
          -> Manager
          -> m ()
renderAll render manager = do

  createRooms
  floodFill
  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy
      handler event =
        case eventPayload event of
          QuitEvent -> throwError Skip
          (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeEscape _))) -> throwError Skip
          (KeyboardEvent (KeyboardEventData _ Pressed _ (Keysym _ KeycodeSpace _))) -> do
            sfor (\x y -> writeArray x y Empty)
            createRooms
            floodFill
          _ -> return ()
  let go = do
         events <- liftIO pollEvents
         mapM_ handler events

         rendererDrawColor render $= V4 255 255 255 255
         clear render


         sfor $ \x y -> do
             readArray x y >>= \case
               Empty -> pure ()
               Road -> do
                    rendererDrawColor render $= V4 155 100 0 255
                    drawRect render
                          (Just (Rectangle (P (V2 (fromIntegral x * blockWidth)
                                            (fromIntegral y * blockWidth)))
                                            (V2 blockWidth blockWidth) ))
               Full -> do
                    rendererDrawColor render $= V4 0 0 255 255
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
  let w = 209 -- natVal @width Proxy
      h = 109 --  natVal @height Proxy
  (render, manager) <- initGUI (fromIntegral w * blockWidth) (fromIntegral h * blockWidth)

  arr <- liftIO $ A.newArray ((0,0), (w - 1, h - 1)) Empty

  r <- randomIO
  runRandom (R.mkStdGen r) $ runArray' arr $ runError @Skip $ do
    renderAll @209 @109 render manager
  SDL.quit
  return ()

