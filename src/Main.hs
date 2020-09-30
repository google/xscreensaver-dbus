-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy of
-- the License at
--
--     https://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations under
-- the License.

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar ( MVar, modifyMVar, modifyMVar_, newMVar
                               , newEmptyMVar, putMVar, readMVar, takeMVar )
import Control.Exception (assert)
import Control.Monad (forever, unless, void)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Set as Set
import Data.Word (Word32)
import qualified DBus as D
import qualified DBus.Client as DC
import System.IO (hPutStrLn, stderr)
import System.Posix.Signals ( Handler(Catch), fullSignalSet, installHandler
                            , sigTERM, sigUSR1)
import qualified System.Process as Process

import Pool (Pool)
import qualified Pool

busName = "org.freedesktop.ScreenSaver" :: D.BusName
screensaverObjectPath = "/org/freedesktop/ScreenSaver" :: D.ObjectPath
screensaverInterfaceName = "org.freedesktop.ScreenSaver" :: D.InterfaceName

wakeupSeconds = 10 :: Int

type State = (Pool Word32, Maybe ThreadId)

main :: IO ()
main = do
  -- Start with 0 as a reserved cookie value. The spec [1] doesn't say anything
  -- about allowable cookie values, but Firefox uses the zero cookie value to
  -- indicate “no cookie” [2]. Ensure that we never return a zero cookie to a
  -- legitimate inhibition request.
  --
  -- [1] https://people.freedesktop.org/~hadess/idle-inhibition-spec/index.html
  -- [2] https://dxr.mozilla.org/mozilla-central/rev/a58462ad649eb95372a1665480505bdefc2b7531/widget/gtk/WakeLockListener.cpp#71
  state <- newMVar (Pool.withReserved (Set.singleton 0), Nothing)
  -- Set up SIGUSR1 to dump state.
  void $ installHandler sigUSR1 (Catch (showInfo state)) (Just fullSignalSet)
  -- Connect to the bus and start listening.
  client <- DC.connectSession
  requestNameReply <- DC.requestName client busName [DC.nameDoNotQueue]
  unless (requestNameReply == DC.NamePrimaryOwner) $
    error $ "unable to reserve bus name: " ++ show requestNameReply
  DC.export client screensaverObjectPath
    DC.defaultInterface
      { DC.interfaceName = screensaverInterfaceName
      , DC.interfaceMethods =
          [ DC.autoMethod "Inhibit" (inhibit state)
          , DC.autoMethod "UnInhibit" (uninhibit state) ] }
  -- Wait for exit. Reference:
  -- https://mail.haskell.org/pipermail/haskell-cafe/2010-May/077841.html
  shouldExit <- newEmptyMVar
  void $ installHandler sigTERM (Catch $ putMVar shouldExit ()) Nothing
  takeMVar shouldExit

inhibit :: MVar State -> String -> String -> IO Word32
inhibit stateM _appname _reason =
  modifyMVar stateM $ \(inhibitions, waker) -> do
    let (cookie, inhibitions') = fromJust $ Pool.take inhibitions
    waker' <- if Pool.null inhibitions
              then assert (isNothing waker) $
                   Just <$> forkIO wakerMain
              else return waker
    return ((inhibitions', waker'), cookie)

uninhibit :: MVar State -> Word32 -> IO ()
uninhibit stateM cookie = modifyMVar_ stateM $ \state@(inhibitions, waker) ->
  case Pool.put cookie inhibitions of
    Nothing -> return state
    Just inhibitions' -> do
      waker' <- if Pool.null inhibitions'
                then do
                  assert (isJust waker) (killThread $ fromJust waker)
                  return Nothing
                else return waker
      return (inhibitions', waker')

wakerMain :: IO ()
wakerMain = forever $ do
  Process.callProcess "xscreensaver-command" ["-deactivate"]
  threadDelay $ wakeupSeconds * microsecondsPerSecond

showInfo :: MVar State -> IO ()
showInfo stateM = do
  state <- readMVar stateM
  hPutStrLn stderr $ show state

microsecondsPerSecond = 1000 * 1000 :: Int
