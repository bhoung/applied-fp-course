{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level04.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Bifunctor                     (bimap, first)

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query), 
                                                    close, open, execute_, query, toRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level04.Types                      (Comment(..), CommentText, getCommentText,
                                                     Error(..), Topic, getTopic, fromDBComment, mkTopic)
import           Level04.DB.Types                   (DBComment(..))

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB { dbConn :: Connection }

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB fadb = close $ dbConn fadb

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB :: FilePath -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = let q = Query createTableQ in 
              Sql.runDBAction $ open fp >>= \conn -> 
                                execute_ conn q >> 
                                return (FirstAppDB conn)
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DBComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DBComment to a Comment, we need to use ``fromDBComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments :: FirstAppDB -> Topic -> IO (Either Error [Comment])
getComments fadb t = 
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DBComment
  -- cannot be converted to a Comment, or simply ignoring any DBComment that is
  -- not valid.
  --
  in do 
     x <- Sql.runDBAction (query (dbConn fadb) sql (Sql.Only $ getTopic t))
     let y = first DBError x 
     pure (y >>= \z -> traverse fromDBComment z)

   -- (>>=) :: Either Error [r] -> ([r] -> Either Error b) -> Either Error b
   --Either Error [r]
   --
   --pure (traverse ( y)

   --where f xs = map (\x -> fromDBComment x) (Right xs)

   -- f (Optional b a)
   --IO (Either SQLITEERROR [r])
   --either (\e -> (DBError e)) 
   --(\y -> fromDBComment y) x 

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> IO (Either Error ())
addCommentToTopic fadb t ct = let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in do
  time <- getCurrentTime
  x <- Sql.runDBAction $ Sql.execute (dbConn fadb) sql (getTopic t, getCommentText ct, time)
  let y = first DBError x
  pure y 


getTopics :: FirstAppDB -> IO (Either Error [Topic])
getTopics fadb = 
  let sql = "SELECT DISTINCT topic FROM comments"
  in do
    x <- Sql.runDBAction $ Sql.query_ (dbConn fadb) sql 
    let y = first DBError x
    --a <- pure 1 
    --let a' = a + 1
    pure (y >>= \z -> traverse (mkTopic . Sql.fromOnly) z)

deleteTopic :: FirstAppDB -> Topic -> IO (Either Error ())
deleteTopic fadb t = 
  let sql = "DELETE FROM comments WHERE topic = ?"
  in do 
    x <- Sql.runDBAction $ Sql.execute_ (dbConn fadb) sql 
    let y = first DBError x
    pure (y)
