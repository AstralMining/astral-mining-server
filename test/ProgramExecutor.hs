module ProgramExecutor
       ( ProcSpec
       , withAstralServer
       ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (void)
import System.Directory (removeDirectoryRecursive)
import System.IO (Handle, IOMode (WriteMode), openFile)
import System.Process ( CreateProcess (..)
                      , ProcessHandle
                      , StdStream (UseHandle)
                      , createProcess
                      , terminateProcess
                      , waitForProcess
                      , proc )

type ProcSpec = (FilePath, [String])

withAstralServer :: ProcSpec -> IO a -> IO a
withAstralServer spec action =
  bracket (startSilentProc spec)
          (\p -> do terminateProcess p
                    void $ waitForProcess p
                    removeDirectoryRecursive "./state")
          (\_ -> do threadDelay 1000000
                    action)

startSilentProc :: ProcSpec -> IO ProcessHandle
startSilentProc spec = do
  (_, _, _, processHandle) <- createProcess =<< silentProc spec
  return processHandle

silentProc :: ProcSpec -> IO CreateProcess
silentProc spec = procWithHandle spec <$> openFile "/dev/null" WriteMode

procWithHandle :: ProcSpec -> Handle -> CreateProcess
procWithHandle (path, opts) handle =
  (proc path opts) { std_out = UseHandle handle
                   , std_err = UseHandle handle }
