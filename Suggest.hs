{-# LANGUAGE OverloadedStrings, BangPatterns, TupleSections #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.Char (toLower)
import Data.Enumerator (Iteratee)
import Data.Enumerator (run_, enumList, ($$))
import Data.IORef
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp

import TST

import Prelude hiding (lookup)

dictFile :: FilePath
dictFile = "resources/frequency"

searchPage :: FilePath
searchPage = "resources/search.html"

wordsLimit :: Int
wordsLimit = 10

toJSON :: [String] -> String
toJSON ws = "[" ++ intercalate "," (map show ws) ++ "]"

type Cache = TST [String]
type Dictionary = TST Int

lookupCache :: Dictionary -> IORef Cache -> String -> IO [String]
lookupCache dict cache w = do
  wsm <- fmap (lookup w) (readIORef cache)
  case wsm of
    Just ws -> return ws
    Nothing -> do
      let ws = take wordsLimit . map fst . sortBy (comparing snd) . prefix w $ dict
      atomicModifyIORef cache ((, ()) . insert w ws)
      return ws

suggest :: Dictionary -> IORef Cache -> String -> Iteratee ByteString IO Response
suggest dict cache w = do
  ws <- lift $ lookupCache dict cache w
  return $ ResponseBuilder status200 [("Content-Type", "application/json")]
           (fromString . toJSON  $ ws)

search :: Response
search = ResponseFile status200 [("Content-Type", "text/html")] searchPage Nothing

e404 :: Response
e404 = ResponseBuilder status404 [("Content-Type", "text/html")] (fromString "404")

app :: Dictionary -> IORef Cache -> Application
app dict cache req = case rawPathInfo req of
  "/suggest.json" -> case queryString req of
                       [("q", (Just w))] -> suggest dict cache (toString w)
                       _                 -> return e404
  "/"        -> return search
  _          -> return e404

main :: IO ()
main = do
  !dict <- fmap (fromList . flip zip [1..] . lines . map toLower) $ readFile dictFile
  cache <- newIORef empty
  putStrLn "Server ready"
  run 3000 (app dict cache)
