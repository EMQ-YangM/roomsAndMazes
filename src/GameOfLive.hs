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
module GameOfLive
  where

import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Kind
import           Data.Proxy
import qualified DoubleArray as SA
import           GHC.TypeLits
import qualified System.Random as R

data Cell = Live | Dead deriving (Show)

type family IsOdd' (a :: Nat) :: Constraint where
  IsOdd' 0 = TypeError (Text "need Odd, but input is Even" )
  IsOdd' 1 = ()
  IsOdd' a = IsOdd' (Mod a 2)

type IsOdd a = (KnownNat a, IsOdd' a)


genGlobalCell :: forall width height sig m.
                (IsOdd width, IsOdd height,
                 HasLabelled SA.DoubleArray (SA.DoubleArray width height Cell) sig m,
                 Has Random sig m,
                 MonadIO m)
             => m ()
genGlobalCell = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

  forM_ [0..h-1] $ \y -> do
    ls <- forM [0..w-1] $ \x -> do
      v0 <- uniformR @Int (1, 20)
      let res = b2b (v0 > 8 )
      SA.writeArray x y res
      return (b2c res)
    liftIO $ putStrLn ls

  SA.swapArray

  SA.sfor $ \x y -> do
    va <- SA.readArray x y
    SA.writeArray x y va

  let go = do
         SA.sfor updateState
         SA.swapArray
         liftIO $ putStrLn (replicate (w+2) '-')
         forM_ [0..h-1] $ \y -> do
           ls <- forM [0..w] $ \x -> do
             if x == w
               then return '|'
               else do
                 v <- SA.readArray x y
                 return (b2c v)
           liftIO $ putStrLn ('|' : ls)
         go
  go

b2b :: Bool -> Cell
b2b True  = Live
b2b False = Dead

isLive :: Cell -> Bool
isLive Live = True
isLive Dead = False

b2c :: Cell -> Char
b2c Live = '*'
b2c Dead = ' '

arround :: [(Int,Int)]
arround = [(0,-1), (0,1)] ++ [(x,y) | x <- [-1,1], y <- [-1,0,1]]

updateState :: forall width height sig m.
                (IsOdd width, IsOdd height,
                 HasLabelled SA.DoubleArray (SA.DoubleArray width height Cell) sig m)
             => Int -- start x
             -> Int -- start y
             -> m ()
updateState x y = do
  let maxWidth = fromIntegral $ natVal @width Proxy
      maxHeight = fromIntegral $ natVal @height Proxy

  ls <- forM arround $ \(dx,dy) -> do
    let x' = (x + dx) `mod` maxWidth
        y' = (y + dy) `mod` maxHeight
    SA.readArray x' y'
  let totalLive = length $ filter isLive ls

  old <- SA.readArray x y

  case totalLive of
    3 -> SA.writeArray x y Live
    2 -> SA.writeArray x y old
    _ -> SA.writeArray x y Dead



rungen :: IO ()
rungen = do
  r <- R.randomIO
  runRandom (R.mkStdGen r) $ SA.runArray Dead (genGlobalCell @191 @81)
  return ()
