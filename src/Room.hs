{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Room where

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
import           SizeArray
import           System.Random (randomIO)
import qualified System.Random as R

data Block
  = Empty
  | Full
  | Road
  | ConnPoint
  deriving (Show, Eq)

type family IsOdd' (a :: Nat) :: Constraint where
  IsOdd' 0 = TypeError (Text "need Odd, but input is Even" )
  IsOdd' 1 = ()
  IsOdd' a = IsOdd' (Mod a 2)

type IsOdd a = (KnownNat a, IsOdd' a)

cb = [(9,10), (5,10), (7,4), (15, 1), (25, 10)] :: [(Int,Int)]

createA :: [(Int,Int)] -> (Int, A.Array Int Int)
createA input =
  let all = sum $ map snd input
      is = concat [replicate b a | (a,b) <- input]
  in (all, A.array (0, all-1) (zip [0 ..] is))

data Skip = Skip deriving (Eq, Show)

createRooms :: forall width height sig m.
                (IsOdd width, IsOdd height,
                 HasLabelled SizeArray (SizeArray width height Block) sig m,
                 Has (Random :+: Error Skip) sig m,
                 MonadIO m)
             => m ()
createRooms = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

      maxCycle = 2000

      (oneS', oneWB) = createA cb

      oneS = oneS' - 1

      go i = do
         if i > maxCycle
           then pure ()
           else do
              roomWIndex <- uniformR (0, oneS)
              roomHIndex <- uniformR (0, oneS)

              let roomW = oneWB A.! roomWIndex
                  roomH = oneWB A.! roomHIndex

              let getOdd (a, b) = do
                     index <- uniformR (a, b)
                     if index `mod` 2 == 1
                       then return index
                       else getOdd (a, b)


              startX <- getOdd (1, w - roomW - 1)
              startY <- getOdd (1, h - roomH - 1)

              catchError @Skip
                (do

                  when (roomH > roomW || roomW > (2 * roomH) ) (throwError Skip) -- room w and h

                  forM_ [0, 2.. roomH -1] $ \y -> do
                    forM_ [0, 2 .. roomW -1] $ \x -> do
                      v <- readArray (startX + x) (startY + y)
                      when (v == Full) (throwError Skip)

                  forM_ [0 .. roomH -1] $ \y ->
                    forM_ [0 .. roomW -1] $ \x ->
                      writeArray (startX + x) (startY + y) Full
                  go (i + 1))

                (\_ -> go (i + 1))


  go 0

  -- forM_ [0..h-1] $ \y -> do
  --   ls <- forM [0..w-1] $ \x -> do
  --     v <- readArray x y
  --     return (b2c v)
  --   liftIO $ putStrLn ls

b2b :: Bool -> Block
b2b True  = Empty
b2b False = Full

b2c :: Block -> Char
b2c Empty     = '_'
b2c Full      = '*'
b2c Road      = '.'
b2c ConnPoint = '+'

rungen :: IO ()
rungen = do
  let w = 269 -- natVal @width Proxy
      h = 79 --  natVal @height Proxy

  arr <- liftIO $ A.newArray ((0,0), (fromIntegral w - 1, fromIntegral  h - 1)) Empty

  let go = do
        l <- getLine
        r <- randomIO
        forM_ [0..h-1] $ \y -> do
          forM [0..w-1] $ \x -> do
            A.writeArray arr (x,y) Empty
        runRandom (R.mkStdGen r) $ runArray' arr $ runError @Skip (createRooms @269 @79)
        go
  go
