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
module Room
  where

import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Effect.Labelled
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Kind
import           Data.Proxy
import           GHC.TypeLits
import           SizeArray
import qualified System.Random as R

data Block = Empty | Full deriving (Show)

type family IsOdd' (a :: Nat) :: Constraint where
  IsOdd' 0 = TypeError (Text "need Odd, but input is Even" )
  IsOdd' 1 = ()
  IsOdd' a = IsOdd' (Mod a 2)

type IsOdd a = (KnownNat a, IsOdd' a)

genGlobalMap :: forall width height sig m.
                (IsOdd width, IsOdd height,
                 HasLabelled SizeArray (SizeArray width height Block) sig m,
                 Has Random sig m,
                 MonadIO m)
             => m ()
genGlobalMap = do

  let w = fromIntegral $ natVal @width Proxy
      h = fromIntegral $ natVal @height Proxy

  forM_ [0..h-1] $ \y -> do
    ls <- forM [0..w-1] $ \x -> do
      v0 <- uniform
      writeArray x y (b2b v0)
      v <- readArray x y
      return (b2c v)
    liftIO $ putStrLn ls

b2b :: Bool -> Block
b2b True  = Empty
b2b False = Full

b2c :: Block -> Char
b2c Empty = ' '
b2c Full  = '*'

rungen :: IO ()
rungen = do
  runRandom (R.mkStdGen 20) $ runArray Empty (genGlobalMap @21 @5)
  return ()
