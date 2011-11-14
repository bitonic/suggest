{-# LANGUAGE OverloadedStrings #-}
module Main where

import Blaze.ByteString.Builder.Char.Utf8
import Data.ByteString.UTF8 (toString)
import Data.Char (toLower)
import Data.Enumerator (run_, enumList, ($$))
import Data.List (intercalate)
import Network.HTTP.Types (status200, status404)
import Network.Wai
import Network.Wai.Handler.Warp

import TST

toJSON :: [String] -> String
toJSON ws = "{words:" ++ intercalate "," (map show ws) ++ "}"

suggest :: TST -> String -> Response
suggest dict w = ResponseBuilder status200 [("Content-Type", "text/plain")]
                 (fromString . toJSON . take 5 . prefix w $ dict)

search :: Response
search = ResponseFile status200 [("Content-Type", "text/html")] "search.html" Nothing

e404 :: Response
e404 = ResponseBuilder status404 [("Content-Type", "text/html")] (fromString "404")

app :: TST -> Application
app dict req = return $ case rawPathInfo req of
  "/suggest" -> case queryString req of
                  [("q", (Just w))] -> suggest dict (toString w)
                  _                 -> e404
  "/"        -> search
  _          -> e404

main :: IO ()
main = do
  ws <- fmap (lines . map toLower) $ readFile "words"
  let dict = fromList ws
  dict `seq` (putStrLn "Server ready" >> run 3000 (app dict))
  
