{-# LANGUAGE OverloadedStrings, BangPatterns, TupleSections, PatternGuards #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Control.Monad.Trans (lift)
import Data.ByteString (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.Char (toLower)
import Data.Enumerator (Iteratee)
import Data.IORef
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp

import Wildcard
import TST

import Prelude hiding (lookup)

dictFile :: FilePath
dictFile = "resources/frequency"

searchPage :: FilePath
searchPage = "resources/search.html"

wordsLimit :: Int
wordsLimit = 10

type Dictionary = TST Char Int
type SuggestCache = TST Char [String]
type CorrectorCache = TST Char (Maybe String)

lookupSuggestCache :: Dictionary -> IORef SuggestCache -> String -> IO [String]
lookupSuggestCache dict cache w = do
  wsm <- fmap (lookup w) (readIORef cache)
  case wsm of
    Just ws -> return ws
    Nothing -> do
      let ws = take wordsLimit . map fst . sortBy (comparing snd) . prefix w $ dict
      atomicModifyIORef cache ((, ()) . insert w ws)
      return ws

suggest :: Dictionary -> IORef SuggestCache -> String -> Iteratee ByteString IO Response
suggest dict cache w = do
  ws <- lift $ lookupSuggestCache dict cache w
  return $ ResponseBuilder status200 [("Content-Type", "application/json")]
           (fromString . toJSON $ ws)
  where
    toJSON ws = "[" ++ intercalate "," (map show ws) ++ "]"

lookupCorrectorCache :: Dictionary -> IORef CorrectorCache -> String -> IO (Maybe String)
lookupCorrectorCache dict cache w = do
  wsm <- fmap (lookup w) (readIORef cache)
  case wsm of
    Just wm -> return wm
    Nothing -> do
      let wm | Just _ <- lookup w dict = Nothing
             | (w' : _) <- edits1      = Just w'
             | (w' : _) <- edits2      = Just w'
             | otherwise               = Nothing
      atomicModifyIORef cache ((, ()) . insert w wm)
      return (wm :: Maybe String)
  where
    process = concatMap (map fst . flip matchWild dict)
    edits1  = process . edits . wild $ w
    edits2  = process . concatMap edits . edits . wild $ w

      
correct :: Dictionary -> IORef CorrectorCache -> String -> Iteratee ByteString IO Response
correct dict cache w = do
  wm <- lift $ lookupCorrectorCache dict cache w
  return $ ResponseBuilder status200 [("Content-Type", "text/plain")]
           (fromString . show $ wm)

search :: Response
search = ResponseFile status200 [("Content-Type", "text/html")] searchPage Nothing

e404 :: Response
e404 = ResponseBuilder status404 [("Content-Type", "text/html")] (fromString "404")

app :: Dictionary -> IORef SuggestCache -> IORef CorrectorCache -> Application
app dict scache ccache req = case rawPathInfo req of
  "/suggest.json" -> case queryString req of
                       [("q", (Just w))] -> suggest dict scache (toString w)
                       _                 -> return e404
  "/correct.json" -> case queryString req of
                       [("q", (Just w))] -> correct dict ccache (toString w)
                       _                 -> return e404
  "/"             -> return search
  _               -> return e404

main :: IO ()
main = do
  !dict <- fmap (fromList . flip zip [1..] . lines . map toLower) $ readFile dictFile
  scache <- newIORef empty
  ccache <- newIORef empty
  run 3000 (app dict scache ccache)
  putStrLn "Server ready on port 3000"
