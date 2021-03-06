{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Control.Applicative
import           Control.Concurrent.ParallelIO.Local
import           Control.Exception
import           Control.Monad hiding (sequence)
import           Data.Foldable
import qualified Data.List as L
import           Data.List.Split
#if MIN_VERSION_shelly(1, 0, 0)
import           Data.Text as T hiding (filter, map, chunksOf)
#else
import           Data.Text.Lazy as T hiding (filter, map, chunksOf)
#endif
import           Filesystem.Path
import           GHC.Conc
import           Prelude hiding (FilePath, sequence, catch)
import           Shelly
import           System.Console.CmdArgs
import           System.Environment (getArgs, withArgs)
import           System.IO hiding (FilePath)

default (Text)

version :: String
version = "0.3.1"

copyright :: String
copyright = "2012-2014"

reHooSummary :: String
reHooSummary = "rehoo v" ++ version ++ ", (C) John Wiegley " ++ copyright

data Rehoo = Rehoo { chunks  :: Int
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
    , outfile = def &= typFile
                &= help "Output file (defaults to default.hoo)"
    , dir     = def &= args &= typDir } &=
    summary reHooSummary &=
    program "rehoo" &=
    help "Rebuild default.hoo from many .hoo files in the given directory"

main :: IO ()
main = do
  mainArgs <- getArgs
  opts     <- withArgs (if L.null mainArgs then ["--help"] else mainArgs)
                       (cmdArgs reHoo)
  caps     <- GHC.Conc.getNumCapabilities

  let jobs'      = case jobs opts   of 0 -> caps; x -> x
      chunks'    = case chunks opts of 0 -> 16;   x -> x
      outputPath = fromText $ case outfile opts of
                                "" -> "default.hoo"
                                x  -> T.pack x

  putStrLn $ "Running with " ++ show jobs'++ " workers and "
          ++ show chunks' ++ " sized chunks per worker"

  shelly $ verbosely $ rm_f outputPath

  _        <- GHC.Conc.setNumCapabilities jobs'
  hoos     <- shelly $ filter <$> pure (`hasExtension` "hoo")
                              <*> (ls . fromText . T.pack . dir $ opts)
  tempPath <- withPool jobs' $ \pool -> processHoos pool chunks' hoos

  shelly $ verbosely $ mv tempPath outputPath

processHoos :: Pool -> Int -> [FilePath] -> IO FilePath
processHoos pool size hoos
  | L.length hoos > size =
    -- Split the list into 'size' sized chunks and fork off a thread to
    -- recursively process each chunk
    let f = processHoos pool size in
    bracket (parallel pool $ map f $ chunksOf size hoos)
            (shelly . verbosely . traverse_ rm) f

  | otherwise = do
    -- Now that we have a list of files < size elements long, and are in our
    -- own thread, we can start the process of running hoogle combine with
    -- output going to a temp file
    (tempPath, hndl) <- openTempFile "." "rehoo.hoo"
    hClose hndl

    let file = T.pack tempPath
    shelly $ verbosely $
      run_ "hoogle" ("combine": "--outfile":file :map toTextIgnore hoos)
    return $ fromText file

-- Main.hs (rehoo) ends here
