{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Type where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Text    (Text)

import Database.Bolt (Record, Value (..), RecordValue (..), Node (..), at)

data Movie = Movie { _id       :: Int
                   , _title    :: Text
                   , _released :: Int
                   , _tagline  :: Text
                   }
  deriving (Show, Eq)

data MovieInfo = MovieInfo { _mTitle :: Text
                           , _cast   :: [Cast]
                           }
  deriving (Show, Eq)

data Cast = Cast { _name :: Text
                 , _job  :: Text
                 , _role :: Value
                 }
  deriving (Show, Eq)

data MNode = MNode { _mnTitle :: Text
                   , _label   :: Text
                   }
  deriving (Show, Eq, Ord)

data MRel = MRel { _source :: Int
                 , _target :: Int
                 }
  deriving (Show, Eq)

data MGraph = MGraph { _nodes :: [MNode]
                     , _links :: [MRel]
                     }
  deriving (Show, Eq)

instance ToJSON Value where
  toJSON (N _) = toJSON ()
  toJSON (B b) = toJSON b
  toJSON (I i) = toJSON i
  toJSON (F d) = toJSON d
  toJSON (T t) = toJSON t
  toJSON (L l) = toJSON l
  toJSON _     = undefined  -- we do not need Maps and Structures in this example

instance ToJSON Movie where
  toJSON (Movie i t r tl) = object [ "id" .= i, "title" .= t
                                   , "released" .= r, "tagline" .= tl
                                   ]

instance ToJSON Cast where
  toJSON (Cast n j r) = object [ "name" .= n, "job" .= j, "role" .= r ]

instance ToJSON MovieInfo where
  toJSON (MovieInfo t c) = object [ "title" .= t, "cast" .= c ]

instance ToJSON MNode where
  toJSON (MNode t l) = object ["title" .= t, "label" .= l]

instance ToJSON MRel where
  toJSON (MRel s t) = object ["source" .= s, "target" .= t]

instance ToJSON MGraph where
  toJSON (MGraph n r) = object ["nodes" .= n, "links" .= r]

-- |Converts some BOLT value to 'Cast'
toCast :: Monad m => Value -> m Cast
toCast (L [T name, T job, role']) = return $ Cast name job role'
toCast _                          = fail "Not a Cast value"

-- |Converts some BOLT value to 'Movie'
toMovie :: Monad m => Value -> m Movie
toMovie v = do node :: Node <- exact v
               let props = nodeProps node
               let identity = nodeIdentity node
               title :: Text <- (props `at` "title") >>= exact
               released :: Int <- (props `at` "released") >>= exact
               tagline :: Text <- (props `at` "tagline") >>= exact
               return $ Movie identity title released tagline

-- |Create movie node and actors node from single record
toNodes :: Monad m => Record -> m (MNode, [MNode])
toNodes r = do title :: Text <- (r `at` "movie") >>= exact
               casts :: [Text] <- (r `at` "cast") >>= exact
               return (MNode title "movie", (`MNode` "actor") <$> casts)
