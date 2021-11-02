{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module SendMessage
  where

import           Control.Algebra
import           Control.Carrier.Lift
import           Control.Carrier.Random.Gen
import           Control.Carrier.Reader
import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Effect.Labelled
import           Control.Monad.IO.Class
import           Data.Kind
import           Data.Proxy
import           System.Random.Stateful (mkStdGen)

-- data SendMethod = Email | Phone | WeChat
--   deriving Show

-- data Send message (m :: Type -> Type) a where
--   SMessage :: SendMethod -> message -> Send message m ()
--   RMessage :: Send message m message

-- sMessage :: Has (Send message) sig m => SendMethod -> message -> m ()
-- sMessage f m = send $ SMessage f m

-- SendEmail
-- SendPhton
-- SendWeChat

data SendMessage message (m :: Type -> Type) a where
  SendMessage :: message -> SendMessage message m ()
  ReceiveMessage :: SendMessage message m message

sendMessage :: Has (SendMessage message) sig m => message -> m ()
sendMessage = send . SendMessage

receiveMessage :: Has (SendMessage message) sig m => m message
receiveMessage = send ReceiveMessage

class Decode message where
  decode :: String -> message

instance Decode String where
  decode = id


newtype SendMessageC message m a = SendMessageC { runSendMessageC :: ReaderC (Chan message, Chan message) m a}
  deriving (Functor, Applicative ,Monad, MonadIO)


instance (Algebra sig m, MonadIO m, Show message, Decode message) =>
  Algebra (SendMessage message :+: sig) (SendMessageC message m) where
    alg hdl sig ctx = SendMessageC $ case sig of
      L (SendMessage message) -> ReaderC $ \(input, output) ->  liftIO (writeChan output message) >> pure ctx
      L ReceiveMessage        -> ReaderC $ \(input, output) ->  (<$ ctx) <$> liftIO (readChan input)
      R other                 -> alg ( runSendMessageC . hdl) (R other) ctx

-- newtype SendMessageC message m a = SendMessageC { runSendMessageC :: m a }
--   deriving (Functor, Applicative ,Monad, MonadIO)

-- instance (Algebra sig m, MonadIO m, Show message, Decode message) =>
--   Algebra (SendMessage message :+: sig) (SendMessageC message m) where
--     alg hdl sig ctx = SendMessageC $ case sig of
--       L (SendMessage message) -> liftIO (print message) >> pure ctx
--       L ReceiveMessage        -> (<$ ctx) . decode <$> liftIO getLine
--       R other                 -> alg (runSendMessageC . hdl) other ctx

runSendMessage :: MonadIO m => Chan message -> Chan message -> SendMessageC message m a -> m a
runSendMessage input output f = runReader (input, output) $ runSendMessageC f

foo :: (Has (SendMessage String :+: Reader String :+: Random) sig m, MonadIO m) => m ()
foo = do
  let go = do
         time <- uniformR (1,10)
         liftIO $ threadDelay (time * 10^4)
         sendMessage "hello"
         v <- receiveMessage @String
         lable <- ask @String
         liftIO $ print (lable ++ " " ++ v ++ " random value is : " ++ show time)
         go
  go

runfoo :: IO ()
runfoo = do

  input <- newChan
  output <- newChan

  forkIO $ runSendMessage @IO @String input output $ evalRandom (mkStdGen 10) $ runReader "thread2" foo
  runSendMessage @IO @String output input  $ evalRandom (mkStdGen 10) $ runReader "thread1" foo
