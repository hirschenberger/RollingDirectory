{-
  RollingDirectory is a daemon watching a directory limiting it's size
  Copyright (C) 2011  Falco Hirschenberger <hirsch@bogfoot.de>

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import Data.Maybe
import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Posix.Daemonize
import System.Posix.Syslog
import Control.Monad

import qualified Watcher as W
import qualified Utils as U

data Options = Options {optDaemonize :: Maybe String,
                        optDirectory :: Maybe FilePath,
                        optSize      :: String}

defaultOptions:: Options
defaultOptions = Options { optDaemonize = Nothing,
                           optDirectory = Nothing,
                           optSize      = "100" }

options:: [OptDescr (Options -> IO Options)]
options = [Option "h" ["help"] 
           (NoArg (\_ ->  printHelp >> exitWith ExitSuccess)) 
           "Print this helping text",
           
           Option "D" ["daemon"]
           (ReqArg (\arg opt -> return opt {optDaemonize = Just arg })
            "start|stop|restart")
           "Start the program in daemon mode", 
           
           Option "d" ["dir"]
           (ReqArg (\arg opt -> return opt {optDirectory = Just arg})
            "directory")
           "The directory to monitor and process",
           
           Option "s" ["size"]
           (ReqArg (\arg opt -> return opt {optSize = arg})
            "size (MB, GB, TB)")
           "The maximum size of the directory in KB,\n\
            \the extensions 'MB', 'GB' and 'TB' are supported. " 
           ]

printHelp:: IO ()
printHelp = do
        prog <- getProgName
        hPutStrLn stderr (usageInfo prog options)

main::IO()
main = useSyslog "RollingDirectory" $ do
    args <- getArgs
        
    -- Parse options, getting a list of option actions
    let (actions, _, _) = getOpt RequireOrder options args
    
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return defaultOptions) actions         

    let size = fromMaybe 0 (U.parseSize $ optSize opts)
    
    when (size==0) ( do putStrLn "Invalid size parameter" 
                        printHelp
                        exitWith (ExitFailure 1))

    case optDirectory opts of
         Just dir -> case optDaemonize opts of
                          -- start the daemon with it's own cmdline args 
                          Just dmn -> withArgs [dmn] $ serviced (daemonMain dir size) 
                          Nothing -> W.start dir size ()
         Nothing -> printHelp >> exitWith (ExitFailure 1) 

daemonMain:: FilePath -> Int -> CreateDaemon ()
daemonMain p s = simpleDaemon { program = W.start p s}

                 

