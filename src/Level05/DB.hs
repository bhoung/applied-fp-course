{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime, UTCTime)
 
import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.AppM                       (AppM, liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB { dbConn  :: Connection }

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB = Sql.close . dbConn

initDB :: FilePath -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDB :: (a -> Either Error b) -> IO a -> AppM b
runDB f dbQuery = do
  xs <- first DBError <$> (liftIO $ Sql.runDBAction dbQuery)
  y <- liftEither xs
  liftEither (f y)

-- answer:
--   xs <- first DBError <$> dbAction
--   liftEither $ f =<< xs  
--   where
--     dbAction = liftIO $ Sql.runDBAction dbQuery
--
-- attempts below
{--
runDB f ioa = let x = f <$> ioa 
                  y = liftEither <$> x
              in x_ y
--}
--let appma = (liftIO ioa) in _todo

  -- This function is intended to abstract away the running of DB functions and
  -- the catching of any errors. As well as the process of running some
  -- processing function over those results.

  -- Move your use of DB.runDBAction to this function to avoid repeating
  -- yourself in the various DB functions.

getComments :: FirstAppDB -> Topic -> AppM [Comment]
getComments fadb t = let
      sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
      q' = (Sql.query (dbConn fadb) sql (Sql.Only $ getTopic t))   
      z = runDB (traverse fromDBComment) q'
      in z 

-- initial thought process:
-- (a -> E E b) -> IO a
-- IO [a]
--
-- traverse  f [a] -> [f a] 
-- [IO a]

{-- version from Level04:
do
       x <- runDB (query (dbConn fadb) sql (Sql.Only $ getTopic t))
       let y = first DBError x
       pure (y >>= \z -> traverse fromDBComment z)
--}

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> AppM ()
addCommentToTopic fadb t ct = do 
     time <- liftIO getCurrentTime 
     let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
     let q = Sql.execute (dbConn fadb) sql (getTopic t, getCommentText ct, time)
--     let db = runDB (\() -> pure ()) q 
     --runDB (\a -> Right a) q
     runDB (pure) q

{--
addCommentToTopic fadb t ct = let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
   in do
   time <- getCurrentTime
   x <- Sql.runDBAction $ Sql.execute (dbConn fadb) sql (getTopic t, getCommentText ct, time)
   let y = first DBError x
   pure y
--}

getTopics :: FirstAppDB -> AppM [Topic]
getTopics fadb = let
    sql = "SELECT DISTINCT topic FROM comments"
    q = Sql.query_ (dbConn fadb) sql
  in runDB (traverse (mkTopic . Sql.fromOnly)) q

deleteTopic :: FirstAppDB -> Topic -> AppM ()
deleteTopic fadb t = let
    sql = "DELETE FROM comments WHERE topic = ?"
    q = Sql.execute (dbConn fadb) sql (Sql.Only (getTopic t))
    in runDB pure q

{--
 getTopics :: FirstAppDB -> IO (Either Error [Topic])
107 getTopics fadb =
108   let sql = "SELECT DISTINCT topic FROM comments"
109   in do
110     x <- Sql.runDBAction $ Sql.query_ (dbConn fadb) sql
111     let y = first DBError x
112     --a <- pure 1
113     --let a' = a + 1
114     pure (y >>= \z -> traverse (mkTopic . Sql.fromOnly) z)
115
116 deleteTopic :: FirstAppDB -> Topic -> IO (Either Error ())
117 deleteTopic fadb t =
118   let sql = "DELETE FROM comments WHERE topic = ?"
119   in do
120     x <- Sql.runDBAction $ Sql.execute_ (dbConn fadb) sql
121     let y = first DBError x
122     pure (y)
--}
-- Go to 'src/Level05/Core.hs' next.
