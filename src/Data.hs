{-# LANGUAGE OverloadedStrings #-}

module Data
    ( ServerState (..), WebM (..)
    , constructState
    , querySearch, queryMovie, queryGraph
    ) where

import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.List                  (nub)
import           Data.Maybe                 (fromJust)
import           Data.Map.Strict            (fromList, (!))
import           Data.Monoid                ((<>))
import           Data.Pool                  (Pool, createPool)
import           Data.Text                  (Text)
import           Database.Bolt

import           Type

-- |A pool of connections to Neo4j server
data ServerState = ServerState { pool :: Pool Pipe }

-- |Reader monad over IO to store connection pool
type WebM = ReaderT ServerState IO

-- |Search movie by title pattern
querySearch :: Text -> BoltActionT IO [Movie]
querySearch q = do records <- queryP cypher params
                   nodes <- traverse (`at` "movie") records
                   traverse toMovie nodes
  where cypher = "MATCH (movie:Movie) WHERE movie.title =~ {title} RETURN movie"
        params = fromList [("title", T $ "(?i).*" <> q <> ".*")]

-- |Returns movie by title
queryMovie :: Text -> BoltActionT IO MovieInfo
queryMovie title = do result <- head <$> queryP cypher params
                      T title <- result `at` "title"
                      L members <- result `at` "cast"
                      cast <- traverse toCast members
                      return $ MovieInfo title cast
  where cypher = "MATCH (movie:Movie {title:{title}}) " <>
                 "OPTIONAL MATCH (movie)<-[r]-(person:Person) " <>
                 "RETURN movie.title as title," <>
                 "collect([person.name, " <>
                 "         head(split(lower(type(r)), '_')), r.roles]) as cast " <>
                 "LIMIT 1"
        params = fromList [("title", T title)]

-- |Returns movies with all it's actors
queryGraph :: Int -> BoltActionT IO MGraph
queryGraph limit = do records <- queryP cypher params
                      nodeTuples <- traverse toNodes records
                      let movies = fst <$> nodeTuples
                      let actors = nub $ concatMap snd nodeTuples
                      let actorIdx = fromJust . (`lookup` zip actors [0..])
                      let modifyTpl (m, as) = (m, actorIdx <$> as)
                      let indexMap = fromList $ modifyTpl <$> nodeTuples
                      let mkTuples (m, t) = (`MRel` t) <$> indexMap ! m
                      let relations = concatMap mkTuples $ zip movies [length actors..]
                      return $ MGraph (actors <> movies) relations
  where cypher = "MATCH (m:Movie)<-[:ACTED_IN]-(a:Person) " <>
                 "RETURN m.title as movie, collect(a.name) as cast " <>
                 "LIMIT {limit}"
        params = fromList [("limit", I limit)]


-- |Create pool of connections (4 stripes, 500 ms timeout, 1 resource per stripe)
constructState :: BoltCfg -> IO ServerState
constructState bcfg = do pool <- createPool (connect bcfg) close 4 500 1
                         return (ServerState pool)
