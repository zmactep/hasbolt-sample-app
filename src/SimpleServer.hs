{-# LANGUAGE OverloadedStrings #-}

module SimpleServer
    ( runServer
    ) where

import           Control.Monad.Trans.Reader           (runReaderT)
import           Data.Text.Lazy                       (Text)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Web.Scotty.Trans                     (ScottyT, defaultHandler,
                                                       get, middleware, scottyT)

import           Routes
import           Data

type Port = Int

-- |Run server with connection pool as a state
runServer :: Port -> IO ()
runServer port = do state <- constructState defaultConfig
                    scottyT port (`runReaderT` state) $ do
                      middleware logStdoutDev
                      get  "/"             mainR
                      get  "/graph"        graphR
                      get  "/search"       searchR
                      get  "/movie/:title" movieR
