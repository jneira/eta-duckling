-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree. An additional grant
-- of patent rights can be found in the PATENTS file in the same directory.


{-# LANGUAGE OverloadedStrings #-}

module Main where
import Control.Applicative hiding (empty)
import Control.Arrow ((***))
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString (ByteString, empty)
import Data.HashMap.Strict (HashMap)
import Data.Maybe
import Data.String
import Data.List
import Data.Text (Text, unpack)
import Data.Time.LocalTime.TimeZone.Series
import Prelude
import System.Directory
import System.Environment (lookupEnv, getArgs)
import TextShow
import Text.Read (readMaybe)
import qualified Data.ByteString.Lazy as LBS  
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Parsing
import Duckling.Core
import Duckling.Data.TimeZone
import Duckling.Resolve (DucklingTime)


main :: IO ()
main = do
  arg <- getArgs
  putStrLn ("Arguments: ")
  -- let text = intercalate " " arg
  let text = "ngày mai"
  putStrLn (text)
  putStrLn("start duckling")
  tzs <- loadTimeZoneSeries "D:\\dev\\app\\msys2-20180531\\usr\\share\\zoneinfo"
  parseHandler tzs text




-- parseHandler2 :: HashMap Text TimeZoneSeries -> String -> IO ()
-- parseHandler2 tzs text = do
--     let t = Text.pack text
--     let tz = Nothing
--     let l = Nothing
--     let ds = Nothing
--     let tz = Nothing
--     let loc = Nothing
--     let ref = Nothing
--     let latent = Nothing
--     let timezone = parseTimeZone tz
--     now <- liftIO $ currentReftime tzs timezone
--     let context = Context
--             { referenceTime = maybe now (parseRefTime timezone) ref
--             , locale = maybe (makeLocale (parseLang l) Nothing) parseLocale Nothing
--             }
--     let options = Options {withLatent = parseLatent latent}
  
--     let dimParse = fromMaybe [] $ decode $ LBS.fromStrict $ fromMaybe "" ds
--     let dims = mapMaybe parseDimension dimParse
--     putStrLn("here")
--     let parsedResult = parse t context options dims
--     putStrLn $ show $ encode parsedResult
--     where
--       defaultLang = VI
--       defaultLocale = makeLocale defaultLang Nothing
--       defaultTimeZone = "Aisa/Ho_Chi_Minh"
--       defaultLatent = False
  
--       parseDimension :: Text -> Maybe (Some Dimension)
--       parseDimension x = fromName x 
  
--       parseTimeZone :: Maybe ByteString -> Text
--       parseTimeZone = maybe defaultTimeZone Text.decodeUtf8
  
--       parseLocale :: ByteString -> Locale
--       parseLocale x = maybe defaultLocale (`makeLocale` mregion) mlang
--         where
--           (mlang, mregion) = case chunks of
--             [a, b] -> (readMaybe a :: Maybe Lang, readMaybe b :: Maybe Region)
--             _      -> (Nothing, Nothing)
--           chunks = map Text.unpack . Text.split (== '_') . Text.toUpper
--             $ Text.decodeUtf8 x
  
--       parseLang :: Maybe ByteString -> Lang
--       parseLang l = fromMaybe defaultLang $ l >>=
--         readMaybe . Text.unpack . Text.toUpper . Text.decodeUtf8
  
--       parseRefTime :: Text -> ByteString -> DucklingTime
--       parseRefTime timezone refTime = makeReftime tzs timezone utcTime
--         where
--           msec = read $ Text.unpack $ Text.decodeUtf8 refTime
--           utcTime = posixSecondsToUTCTime $ fromInteger msec / 1000
  
--       parseLatent :: Maybe ByteString -> Bool
--       parseLatent x = fromMaybe defaultLatent
--         (readMaybe (Text.unpack $ Text.toTitle $ Text.decodeUtf8 $ fromMaybe empty x)::Maybe Bool)
