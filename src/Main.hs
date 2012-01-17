module Main where

import System.Exit
import System.IO
import System.Environment
import System.Console.GetOpt
import System.Posix.Daemonize
import qualified Watcher as W

data Options = Options {optDaemonize :: Maybe String,
                        optDirectory :: Maybe FilePath,
                        optSize      :: Int}

defaultOptions:: Options
defaultOptions = Options { optDaemonize = Nothing,
                           optDirectory = Nothing,
                           optSize      = 100 }

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
           (ReqArg (\arg opt -> return opt {optSize = read arg})
            "size in MB")
           "The maximum size of the directory in Megabytes" 
           ]

printHelp:: IO ()
printHelp = do
        prog <- getProgName
        hPutStrLn stderr (usageInfo prog options)

main::IO()
main = do
    args <- getArgs
        
    -- Parse options, getting a list of option actions
    let (actions, _, _) = getOpt RequireOrder options args
    
    -- Here we thread startOptions through all supplied option actions
    opts <- foldl (>>=) (return defaultOptions) actions         

    let size = optSize opts  

    case optDirectory opts of
         Just dir -> case optDaemonize opts of
                          -- start the daemon with it's own cmdline args 
                          Just dmn -> withArgs [dmn] $ serviced (daemonMain dir size) 
                          Nothing -> W.start dir size ()
         Nothing -> printHelp >> exitWith ExitSuccess

daemonMain:: FilePath -> Int -> CreateDaemon ()
daemonMain p s = simpleDaemon { program = W.start p s}



