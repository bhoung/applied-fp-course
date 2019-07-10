{-# LANGUAGE OverloadedStrings #-}
module Level02.Core (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody, rawPathInfo)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           Level02.Types            (ContentType(..), Error(..), RqType(..),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- |-------------------------------------------|
-- |- Don't start here, go to Level02.Types!  -|
-- |-------------------------------------------|

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse s ct t = responseLBS s [("Content-Type", renderContentType ct)] t

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 ct = mkResponse status200 ct 

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 ct = mkResponse status404 ct 

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 ct = mkResponse status400 ct 

-- |----------------------------------------------------------------------------------
-- These next few functions will take raw request information and construct         --
-- one of our types.                                                                --
--                                                                                  --
-- By breaking out these smaller functions, we're able to isolate our               --
-- validation requirements into smaller components that are simpler to maintain     --
-- and verify. It also allows for greater reuse and it also means that              --
-- validation is not duplicated across the application, maybe incorrectly.          --
--------------------------------------------------------------------------------------

mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest "" _ = Left EmptyTopic
mkAddRequest _ "" = Left EmptyComment

mkAddRequest t lbs = let commenttext = mkCommentText (lazyByteStringToStrictText lbs)
                     in AddRq <$> (mkTopic t) <*> (commenttext)
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText = decodeUtf8 . LBS.toStrict

mkViewRequest :: Text -> Either Error RqType
mkViewRequest t = ViewRq <$> mkTopic t 

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

-- |----------------------------------
-- end of RqType creation functions --
--------------------------------------

mkErrorResponse :: Error -> Response
mkErrorResponse e = case e of 
                      EmptyTopic -> resp200 PlainText "Topic not entered"
                      EmptyComment -> resp404 PlainText "Comment is empty string"
                      WrongUrl -> resp400 PlainText "doesn't match one of: /<topic>/add, /topic/view or /list"

-- | Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO ( Either Error RqType )
mkRequest req = let body = strictRequestBody req 
                    rm = requestMethod req
                in case (pathInfo req, rm) of
                    (["list"], "GET") -> pure mkListRequest
                    ([t, "view"], "GET") -> pure (mkViewRequest t)
                    ([t, "add"], "POST") -> (mkAddRequest t) <$> body
                    (_, _) -> pure (Left WrongUrl)

{--
mkRequest req = let path = pathInfo req
                    (x:xs) = path
                    rm = requestMethod req
                    body = strictRequestBody req
                in if x == "list" && rm == "GET"
                   then (pure mkListRequest)
                   else case (head xs) of
                          "add" -> (mkAddRequest x) <$> body
                          "view" -> pure (mkViewRequest x)
                          _  -> pure (Left WrongUrl)
--}
                   
-- Remembering your pattern-matching skills will let you implement the entire
-- specification in this function.

-- | If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Either Error Response
handleRequest rqType = case rqType of 
                      AddRq t ct -> Right (mkResponse status200 PlainText "AddRq")
                      ViewRq t -> Right (mkResponse status200 PlainText "ViewRq")
                      ListRq -> Right (mkResponse status200 PlainText "ListRq")
                      _ -> Left WrongUrl

-- | Reimplement this function using the new functions and ``RqType`` constructors as a guide.
app :: Application
--
--app :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app req cb = let x = mkRequest req 
             in x >>= \a -> 
               case a of 
                 Right r -> either (cb . mkErrorResponse) (cb . id) $ handleRequest r 
                 Left e -> cb . mkErrorResponse $ e

runApp :: IO ()
runApp = run 3000 app
