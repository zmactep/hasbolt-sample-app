# hasbolt-sample-app
Sample Movie Database application with Haskell backend

Feel free to copy this example and use [hasbolt](https://github.com/zmactep/hasbolt) as you with :)

Build
-----

To build the project just clone it and run build command by stack:
```
git clone https://github.com/zmactep/hasbolt-sample-app.git
cd hasbolt-sample-app
stack build
```

Usage
-----

```
PORT=8080 stack exec hasbolt-sample-app-exe
```

Cloud deployment
----------------

**TODO:** write something here

Under the hood
--------------

Here I use static jQuery html from [movies-python-bolt](https://github.com/neo4j-examples/movies-python-bolt) and the same API.

Http backend uses [Scotty](https://github.com/scotty-web/scotty) web framework. To store the internal state with connection pool I use a ReaderT monad transformer.

### Server state

We have default hardcoded Neo4j server parameters in this example ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Data.hs#L68)):
```haskell
-- |Default configuration for localhost neo4j server
defaultConfig :: BoltCfg
defaultConfig = def {user = "neo4j", password = "neo4j"}
```

First, we need to create a connection pool to out Neo4j database ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Data.hs#L21)):
```haskell
-- |A pool of connections to Neo4j server
data ServerState = ServerState { pool :: Pool Pipe }

-- |Reader monad over IO to store connection pool
type WebM = ReaderT ServerState IO
```

To create new connection pool we use `connect :: BoltCfg -> IO Pipe` and `close :: Pipe -> IO ()` functions from **hasbolt** to tell resource-pool how to create a new connection and how to close one ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Data.hs#L72)):
```haskell
-- |Create pool of connections (4 stripes, 500 ms timeout, 1 resource per stripe)
constructState :: BoltCfg -> IO ServerState
constructState bcfg = do pool <- createPool (connect bcfg) close 4 500 1
                         return (ServerState pool)
```

### Simple server

After we created a representation of our server state, we can create a simple server on given port ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/SimpleServer.hs#L18)):
```haskell
runServer :: Port -> IO ()
runServer port = do state <- constructState defaultConfig
                    scottyT port (`runReaderT` state) $ do
                      middleware logStdoutDev
                      get  "/"             mainR
                      get  "/graph"        graphR
                      get  "/search"       searchR
                      get  "/movie/:title" movieR
```

Here we construct a new state by `constrictState :: BoltCfg -> ServerState` function from hardcoded default configuration and set routes. Let's implement these routes.

First of all we need to respond with static [index.html](https://github.com/zmactep/hasbolt-sample-app/blob/master/index.html) on "/" route ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Routes.hs#L20)):
```haskell
-- |Main page route
mainR :: ActionT Text WebM ()
mainR = file "index.html"
```

On a search request we get a text "q" parameter and perform a movie search, then respond it as json ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Routes.hs#L30)):
```haskell
-- |Search response route
searchR :: ActionT Text WebM ()
searchR = do q <- param "q" :: ActionT Text WebM Text
             results <- runQ $ querySearch (toStrict q)
             json results
```

A movie select is quick and unsafe way to get a json with movie info by it's exact title ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Routes.hs#L36)):
```haskell
-- |Movie response route
movieR :: ActionT Text WebM ()
movieR = do t <- param "title" :: ActionT Text WebM Text
            movieInfo <- runQ $ queryMovie (toStrict t)
            json movieInfo
```

At last we have to return a graph of movies and actors ([source]()):
```haskell
-- |Graph response route
graphR :: ActionT Text WebM ()
graphR = do limit <- param "limit" `rescue` const (return 100)
            graph <- runQ $ queryGraph limit
            json graph
```

### Frontend <-> backend JSON protocol serialization

**TODO:** Uninteresting JSON data serialization ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Type.hs)).

### Database queries by hasbolt

To run queries we need to get a connection pool, get one `Pipe` and do a request by this pipe. All of this in the `runQ` function ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Routes.hs#L15)):
```haskell
-- |Run BOLT action in scotty 'ActionT' monad transformer
runQ :: BoltActionT IO a -> ActionT Text WebM a
runQ act = do ss <- lift ask
              liftIO $ withResource (pool ss) (`run` act)
```

See comments in the code to understand the query functions.

Search ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Data.hs#L27)):
```haskell
-- |Search movie by title pattern
querySearch :: Text -> BoltActionT IO [Movie]
querySearch q = do records <- queryP cypher params           -- get record list by cypher query and params
                   nodes <- traverse (`at` "movie") records  -- from each record get only "movie" field
                   traverse toMovie nodes                    -- serialize movies to jsonable data
  where cypher = "MATCH (movie:Movie) WHERE movie.title =~ {title} RETURN movie"
        params = fromList [("title", T $ "(?i).*" <> q <> ".*")]
```


Movie ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Data.hs#L35)):
```haskell
-- |Returns movie by title
queryMovie :: Text -> BoltActionT IO MovieInfo
queryMovie title = do result <- head <$> queryP cypher params -- get first record from received record list by cypher query and params
                      T title <- result `at` "title"          -- get movie title as text (you also can use exact function here)
                      L members <- result `at` "cast"         -- get movie cast as list
                      cast <- traverse toCast members         -- serialize cast to jsonable data
                      return $ MovieInfo title cast           -- serialize all to json object
  where cypher = "MATCH (movie:Movie {title:{title}}) " <>
                 "OPTIONAL MATCH (movie)<-[r]-(person:Person) " <>
                 "RETURN movie.title as title," <>
                 "collect([person.name, " <>
                 "         head(split(lower(type(r)), '_')), r.roles]) as cast " <>
                 "LIMIT 1"
        params = fromList [("title", T title)]
```

Graph ([source](https://github.com/zmactep/hasbolt-sample-app/blob/master/src/Data.hs#L50)):
```haskell
-- |Returns movies with all it's actors
queryGraph :: Int -> BoltActionT IO MGraph
queryGraph limit = do records <- queryP cypher params                        -- get first record from received record
                      nodeTuples <- traverse toNodes records                 -- convert records to list of tuples (movie, [actors])
                      let movies = fst <$> nodeTuples                        -- get list of movies
                      let actors = nub $ concatMap snd nodeTuples            -- get list of actors
                      let actorIdx = fromJust . (`lookup` zip actors [0..])  -- some magic here to obtain relations
                      let modifyTpl (m, as) = (m, actorIdx <$> as)
                      let indexMap = fromList $ modifyTpl <$> nodeTuples
                      let mkTuples (m, t) = (`MRel` t) <$> indexMap ! m
                      let relations = concatMap mkTuples $ zip movies [length actors..]
                      return $ MGraph (actors <> movies) relations           -- serialize all to json object
  where cypher = "MATCH (m:Movie)<-[:ACTED_IN]-(a:Person) " <>
                 "RETURN m.title as movie, collect(a.name) as cast " <>
                 "LIMIT {limit}"
        params = fromList [("limit", I limit)]
```
