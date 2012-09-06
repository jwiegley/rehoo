{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Concurrent.MSem as MSem
import           Control.Exception
import           Control.Monad hiding (sequence)
import           Data.Foldable
import qualified Data.List as L
import           Data.List.Split
import           Data.Text.Lazy as T hiding (filter, map, chunksOf)
import           Data.Traversable
import           Filesystem.Path
import           GHC.Conc
import           Prelude hiding (FilePath, sequence)
import           Shelly
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)
import           System.Exit
import           System.IO hiding (FilePath)

default (Text)

version :: String
version       = "0.1.0"

copyright :: String
copyright     = "2012"

reHooSummary :: String
reHooSummary = "rehoo v" ++ version ++ ", (C) John Wiegley " ++ copyright

data Rehoo = Rehoo
    { chunks  :: Int
    , jobs    :: Int
    , outfile :: String
    , dir     :: String }
    deriving (Data, Typeable, Show, Eq)

reHoo :: Rehoo
reHoo = Rehoo
    { chunks  = def &= typ "INT"
                &= help "Process INT .hoo's at a time (def: 16)"
    , jobs    = def &= name "j" &= typ "INT"
                &= help "Run INT hoogle combine's (def: # of capabilities)"
    , outfile = def &= typFile   &= help "Output file (defaults to default.hoo)"
    , dir     = def &= args &= typDir } &=
    summary reHooSummary &=
    program "rehoo" &=
    help "Rebuild default.hoo from many .hoo files in the given directory"

main :: IO ()
main = do
  -- process command-line options
  mainArgs <- getArgs
  opts     <- withArgs (if L.null mainArgs then ["--help"] else mainArgs)
                       (cmdArgs reHoo)

  caps <- GHC.Conc.getNumCapabilities

  let jobs'   = max (jobs opts) caps
      chunks' = max (chunks opts) 16

  _ <- GHC.Conc.setNumCapabilities jobs'

  hoos <- shelly $ filter <$> pure (`hasExtension` "hoo")
                          <*> (ls . fromText . T.pack . dir $ opts)

  putStrLn $ "Running with " ++ show jobs'
          ++ " workers and " ++ show chunks'
          ++ " sized chunks per worker"

  pool     <- MSem.new jobs'
  tempPath <- processHoos pool chunks' hoos

  shelly $ verbosely $
    mv tempPath $ fromText $ case outfile opts of
                               "" -> "default.hoo"
                               x  -> T.pack x
  exitSuccess

processHoos :: MSem.MSem Int -> Int -> [FilePath] -> IO FilePath
processHoos pool size hoos
  | L.length hoos > size = do
    -- Split the list into 'size' sized chunks, then fork off a thread to
    -- recursively process each chunk.  The results are collected in series
    -- from MVars that each contain the final pathname of the subjob.
    bracket (traverse forkProcessHoos (chunksOf size hoos) >>=
             traverse takeMVar)
            (shelly . verbosely . traverse_ rm)
            (processHoos pool size)

  | otherwise = do
    -- Now that we have a list of files < size elements long, and we are
    -- already in our own thread, we can start the expensive process of
    -- running hoogle combine with the output going to a temp file.
    (tempPath, hndl) <- openTempFile "." "rehoo.hoo"
    hClose hndl

    MSem.with pool $
      shelly $ verbosely $
        run_ "hoogle" ( "combine" : "--outfile" : T.pack tempPath
                      : map toTextIgnore hoos)

    return . fromText . T.pack $ tempPath

  where
    forkProcessHoos :: [FilePath] -> IO (MVar FilePath)
    forkProcessHoos xs = do
      mVar <- newEmptyMVar
      _    <- forkIO $ processHoos pool size xs >>= putMVar mVar
      return mVar

-- Main.hs (rehoo) ends here
