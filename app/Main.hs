{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import Control.Monad.Except (MonadIO, MonadError, catchError, liftIO)
import Control.Applicative  ((<$>))
import System.Environment   (getEnv)

import Database.Bolt

import SimpleServer

-- |Default configuration for localhost neo4j server
defaultConfig :: BoltCfg
defaultConfig = def {user = "neo4j", password = "neo4j"}

-- |Run as PORT=8080 stack exec hasbolt-sample-app-exe
main :: IO ()
main = run `catchError` failMsg
  where run = do port <- read <$> getEnv "PORT"
                 config <- readConfig `catchError` const (return defaultConfig)
                 runServer port config
        readConfig = do
          bolt <- getEnv "GRAPHENEDB_BOLT_URL"
          user <- read <$> getEnv "GRAPHENEDB_BOLT_USER"
          pass <- read <$> getEnv "GRAPHENEDB_BOLT_PASSWORD"
          let (host, port) = let sp = last (elemIndices ':' bolt)
                             in (read $ take sp bolt, read $ drop (sp+1) bolt)
          return def { user = user, password = pass, host = host, port = port }

  --(read <$> getEnv "PORT" >>= runServer) `catchError` failMsg

failMsg :: (MonadError e m, MonadIO m, Show e) => e -> m ()
failMsg e = liftIO $ putStrLn ("Ooops: " ++ show e)
