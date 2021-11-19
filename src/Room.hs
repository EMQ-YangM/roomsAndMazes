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
import           Control.Effect.SizeArray
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Array as A
import qualified Data.Array.IO as A
import           Data.Kind
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed.Base as B
import           Data.Word
import           GHC.TypeLits
import           Optics (makeLenses)
import           System.Random (randomIO)
import qualified System.Random as R

data Block
  = Empty
  | Full
  | Road
  | ConnPoint
  | Span
  deriving (Show, Eq)


fromBlock :: Block -> Word8
{-# INLINE fromBlock #-}
fromBlock Empty     = 0
fromBlock Full      = 1
fromBlock Road      = 2
fromBlock ConnPoint = 3
fromBlock Span      = 4

toBlock :: Word8 -> Block
{-# INLINE toBlock #-}
toBlock 0 = Empty
toBlock 1 = Full
toBlock 2 = Road
toBlock 3 = ConnPoint
toBlock 4 = Span
toBlock _ = error ".."

newtype instance B.MVector s Block = MV_Block (P.MVector s Word8)
newtype instance B.Vector    Block = V_Block  (P.Vector    Word8)

instance M.MVector B.MVector Block where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Block v) = M.basicLength v
  basicUnsafeSlice i n (MV_Block v) = MV_Block $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Block v1) (MV_Block v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Block `liftM` M.basicUnsafeNew n
  basicInitialize (MV_Block v) = M.basicInitialize v
  basicUnsafeReplicate n x = MV_Block `liftM` M.basicUnsafeReplicate n (fromBlock x)
  basicUnsafeRead (MV_Block v) i = toBlock `liftM` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Block v) i x = M.basicUnsafeWrite v i (fromBlock x)
  basicClear (MV_Block v) = M.basicClear v
  basicSet (MV_Block v) x = M.basicSet v (fromBlock x)
  basicUnsafeCopy (MV_Block v1) (MV_Block v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Block v1) (MV_Block v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Block v) n = MV_Block `liftM` M.basicUnsafeGrow v n


instance G.Vector B.Vector Block where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Block v) = V_Block `liftM` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Block v) = MV_Block `liftM` G.basicUnsafeThaw v
  basicLength (V_Block v) = G.basicLength v
  basicUnsafeSlice i n (V_Block v) = V_Block $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Block v) i = toBlock `liftM` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Block mv) (V_Block v) = G.basicUnsafeCopy mv v
  elemseq _ = seq

instance B.Unbox Block

type family IsOdd' (a :: Nat) :: Constraint where
  IsOdd' 0 = TypeError (Text "need Odd, but input is Even" )
  IsOdd' 1 = ()
  IsOdd' a = IsOdd' (Mod a 2)

type IsOdd a = (KnownNat a, IsOdd' a)

cb = [(9,10), (5,10), (7,4), (15, 1), (25, 5)] :: [(Int,Int)]

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

newtype CPoints
  = CPoints { _cpoints :: [(Int, Int)] }

makeLenses ''CPoints

data FillStack
  = FillStack
  { _fillStack :: [((Int, Int), (Int, Int))]
  , _endPoint  :: Set (Int, Int)
  }

makeLenses ''FillStack

createRooms :: (HasLabelled SizeArray (SizeArray Block) sig m,
                Has (Random :+: Error Skip :+: State CPoints) sig m)
            => Int
            -> m ()
createRooms maxCycle = do
  w <- arrayWidth
  h <- arrayHeight

  let (oneS', oneWB) = createA cb

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

                  let nx = startX - 1
                      ny = startY - 1

                      nw = roomW + 1
                      nh = roomH + 1

                  forM_ [-1, 0 .. roomH] $ \y ->
                    forM_ [-1, 0 .. roomW] $ \x ->
                      if (x == -1) || (y == -1) || (x == roomW) || (y == roomH)
                        then cpoints %= ((startX + x, startY + y) :)
                        else writeArray (startX + x) (startY + y) Full
                  go (i + 1))

                (\_ -> go (i + 1))


  go 0
