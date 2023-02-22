{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ident.Fragment (Fragment (..), getFragments, setFragments) where

import Data.Aeson as JSON (FromJSON, Result (..), ToJSON, Value (..), fromJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import Data.Aeson.KeyMap as KeyMap (alterF, lookup)
import Data.Aeson.Types (parseFail)
import qualified Data.Text as T (pack)
import Data.Time.Calendar.Month (Month (..))
import Data.Time.Calendar.WeekDate (DayOfWeek (..))
import Data.Time.Clock (diffTimeToPicoseconds)
import Data.Time.Clock.POSIX (POSIXTime)
import qualified Data.Vector as Vector
import GHC.Generics
import Message (Message)

data Fragment
    = Free String
    | Fixed String
    | Sequence String Int Int Int (Maybe Int)
    | Existing String String
    | YYYY Int
    | YY Int
    | MMMM Month
    | MM Month
    | Weekday DayOfWeek
    | DoM Int
    | Hour Int
    | Minute Int
    | Second Int
    | DateFrom String POSIXTime
    deriving (Generic, Show)

dowToString :: DayOfWeek -> String
dowToString dow = case dow of
    Monday -> "Mon"
    Tuesday -> "Tue"
    Wednesday -> "Wed"
    Thursday -> "Thu"
    Friday -> "Fri"
    Saturday -> "Sat"
    Sunday -> "Sun"

monthToString :: Month -> String
monthToString month =
    case month of
        MkMonth 1 -> "Jan"
        MkMonth 2 -> "Feb"
        MkMonth 3 -> "Mar"
        MkMonth 4 -> "Apr"
        MkMonth 5 -> "May"
        MkMonth 6 -> "Jun"
        MkMonth 7 -> "Jul"
        MkMonth 8 -> "Aug"
        MkMonth 9 -> "Sep"
        MkMonth 10 -> "Oct"
        MkMonth 11 -> "Nov"
        MkMonth 12 -> "Dec"
        _ -> "N/A"

posixToMillis :: POSIXTime -> Integer
posixToMillis = diffTimeToPicoseconds . realToFrac

instance ToJSON Fragment where
    toJSON f = toJSON $ case f of
        Free s -> object ["type" .= String "Free", "value" .= String (T.pack s)]
        Fixed s -> object ["type" .= String "Fixed", "value" .= String (T.pack s)]
        Sequence name padding step start value ->
            object
                [ "type" .= String "Sequence"
                , "name" .= String (T.pack name)
                , "padding" .= padding
                , "step" .= step
                , "start" .= start
                , "value" .= value
                ]
        Existing name value -> object ["type" .= String "Existing", "name" .= String (T.pack name), "value" .= String (T.pack value)]
        YYYY year -> object ["type" .= String "YYYY", "value" .= String (T.pack $ show year)]
        YY year -> object ["type" .= String "YY", "value" .= String (T.pack $ show year)]
        MMMM month -> object ["type" .= String "MMMM", "value" .= String (T.pack $ monthToString month)]
        MM month -> object ["type" .= String "MM", "value" .= String (T.pack $ monthToString month)]
        Weekday dow -> object ["type" .= String "Weekday", "value" .= String (T.pack $ dowToString dow)]
        DoM dom -> object ["type" .= String "DoM", "value" .= String (T.pack $ show dom)]
        Hour hour -> object ["type" .= String "Hour", "value" .= String (T.pack $ show hour)]
        Minute minute -> object ["type" .= String "Minute", "value" .= String (T.pack $ show minute)]
        Second second -> object ["type" .= String "Second", "value" .= String (T.pack $ show second)]
        DateFrom name posix -> object ["type" .= String "DateFrom", "name" .= String (T.pack name), "value" .= String (T.pack $ show $ posixToMillis posix)]

instance FromJSON Fragment where
    parseJSON = withObject "Fragment" $ \o -> do
        type_ <- o .: "type"
        case type_ of
            String "Free" -> Free <$> o .: "value"
            String "Fixed" -> Fixed <$> o .: "value"
            String "Sequence" -> Sequence <$> o .: "name" <*> o .: "padding" <*> o .: "step" <*> o .: "start" <*> o .: "value"
            String "Existing" -> Existing <$> o .: "name" <*> o .: "value"
            String "YYYY" -> YYYY <$> o .: "value"
            String "YY" -> YY <$> o .: "value"
            String "MMMM" -> MMMM <$> o .: "value"
            String "MM" -> MM <$> o .: "value"
            String "Weekday" -> Weekday <$> o .: "value"
            String "DoM" -> DoM <$> o .: "value"
            String "Hour" -> Hour <$> o .: "value"
            String "Minute" -> Minute <$> o .: "value"
            String "Second" -> Second <$> o .: "value"
            String "DateFrom" -> DateFrom <$> o .: "name" <*> o .: "value"
            _ -> parseFail "Unknown fragment"

getFragments :: Message -> [Fragment]
getFragments msg =
    case msg of
        (JSON.Object o) -> case KeyMap.lookup "load" o of
            (Just (JSON.Object m)) ->
                case KeyMap.lookup "fragments" m of
                    Just (JSON.Array fragments) -> do
                        result <- fromJSON <$> Vector.toList fragments
                        case result of
                            Success f -> [f]
                            Error _ -> []
                    _ -> []
            _ -> []
        _ -> []

setFragments :: [Fragment] -> Message -> Message
setFragments fragments message =
    case message of
        JSON.Object keymap ->
            JSON.toJSON $
                alterF
                    ( \case
                        (Just (JSON.Object m)) ->
                            Just $
                                Just $
                                    JSON.toJSON $
                                        alterF
                                            ( \case
                                                (Just _) -> Just $ Just $ JSON.Array $ Vector.fromList $ fmap toJSON fragments
                                                _ -> Nothing
                                            )
                                            "fragments"
                                            m
                        _ -> Nothing
                    )
                    "load"
                    keymap
        _ -> message
