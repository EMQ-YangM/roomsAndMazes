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
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Room where

import           Control.Carrier.Error.Either
import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Carrier.State.Strict
import           Control.Effect.Labelled
import           Control.Effect.Optics ((%=))
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Array as A
import qualified Data.Array.IO as A
import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits
import           Optics (makeLenses)
import           SizeArray
import           System.Random (randomIO)
import qualified System.Random as R

data Block
  = Empty
  | Full
  | Road
  | ConnPoint
  | Span
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

data Skip
  = Skip
  | SkipCM
  | SkipM (Int, Int)
  deriving (Eq, Show)

createRooms :: forall width height sig m.
                (IsOdd width, IsOdd height,
                 HasLabelled SizeArray (SizeArray width height Block) sig m,
                 Has (Random :+: Error Skip) sig m,
                 MonadIO m)
             => m ()
createRooms = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

      maxCycle = 1000
      -- maxCycle = 1000000

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

                  when (roomH > (2 * roomW) || roomW > (2 * roomH) ) (throwError Skip) -- room w and h

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
