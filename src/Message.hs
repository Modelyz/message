{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Message (isProcessed, getMessages, getUuids, Uuid, Message, getInt, setFlow, getMetaString, excludeType, isType, isAfter, appendMessage, readMessages, getFlow) where

import Control.Exception (SomeException (SomeException), catch)
import Data.Aeson as JSON (decode, encode)
import qualified Data.Aeson as JSON (Value (..), toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Scientific
import qualified Data.Text as T (Text, unpack)
import qualified Data.Text.Lazy as LT (Text, append, intercalate, lines)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy.IO as IO (appendFile, readFile)
import qualified Data.Vector as Vector

type Message = JSON.Value

type Time = Int

type Uuid = String -- uuid as a string generated in Elm

isType :: T.Text -> Message -> Bool
isType t ev = getString "what" ev == Just t

excludeType :: T.Text -> [Message] -> [Message]
excludeType t = filter (not . isType t)

isAfter :: Time -> Message -> Bool
isAfter t ev =
    case ev of
        (JSON.Object o) -> case KeyMap.lookup "meta" o of
            Just m -> case getInt "posixtime" m of
                Just et -> et >= t
                Nothing -> False
            _ -> False
        _ -> False

getInt :: KeyMap.Key -> Message -> Maybe Int
getInt k e =
    case e of
        (JSON.Object o) -> case KeyMap.lookup k o of
            Just (JSON.Number n) -> Just n >>= toBoundedInteger
            _ -> Nothing
        _ -> Nothing

getMessages :: JSON.Value -> [Message]
getMessages e =
    -- TODO use Vector instead
    case e of
        (JSON.Object o) -> case KeyMap.lookup "messages" o of
            (Just (JSON.Array a)) -> Vector.toList a
            _ -> []
        _ -> []

getMetaString :: KeyMap.Key -> Message -> Maybe T.Text
getMetaString k e =
    case e of
        (JSON.Object o) -> case KeyMap.lookup "meta" o of
            (Just (JSON.Object m)) -> case KeyMap.lookup k m of
                Just (JSON.String s) -> Just s
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing

getFlow :: Message -> Maybe T.Text
getFlow = getMetaString "flow"

isProcessed :: Message -> Bool
isProcessed msg = maybe False ((==) "Processed" . T.unpack) (getMetaString "flow" msg)

getString :: KeyMap.Key -> Message -> Maybe T.Text
getString k e =
    case e of
        (JSON.Object o) -> case KeyMap.lookup k o of
            Just (JSON.String s) -> Just s
            _ -> Nothing
        _ -> Nothing

getUuids :: Message -> [Uuid]
getUuids e =
    case e of
        (JSON.Object o) -> case KeyMap.lookup "load" o of
            (Just (JSON.Object m)) ->
                case KeyMap.lookup "uuids" m of
                    Just (JSON.Array uuids) ->
                        Vector.toList
                            ( fmap
                                ( \case
                                    JSON.String uuid -> T.unpack uuid
                                    _ -> ""
                                )
                                uuids
                            )
                    _ -> []
            _ -> []
        _ -> []

setFlow :: T.Text -> Message -> Message
setFlow flow e =
    case e of
        JSON.Object keymap ->
            JSON.toJSON $
                KeyMap.alterF
                    ( \case
                        Just (JSON.Object meta) ->
                            Just $
                                Just $
                                    JSON.toJSON $
                                        KeyMap.alterF
                                            ( \case
                                                Just _ -> Just $ Just $ JSON.String flow
                                                Nothing -> Nothing
                                            )
                                            "flow"
                                            meta
                        Just other -> Just $ Just other
                        Nothing -> Nothing
                    )
                    "meta"
                    keymap
        _ -> e

--    case makeObj
--        [ ("uuid", JSString $ toJSString $ show uuid)
--        , ("type", JSString $ toJSString "AckReceived")
--        , ("posixtime", showJSON time)
--        , ("origin", JSString $ toJSString $ origin)
--        ] of
--        JSObject o -> o
appendMessage :: FilePath -> Message -> IO ()
appendMessage f message =
    IO.appendFile f $ decodeUtf8 (JSON.encode message) `LT.append` "\n"

-- read the message store
readMessages :: FilePath -> IO [Message]
readMessages f =
    do
        es <- catch (IO.readFile f) handleMissing
        case JSON.decode $ encodeUtf8 $ "[" `LT.append` LT.intercalate "," (LT.lines es) `LT.append` "]" of
            Just evs -> return evs
            Nothing -> return []
  where
    handleMissing :: SomeException -> IO LT.Text
    handleMissing (SomeException _) = do
        putStrLn "Could not read MessageSource"
        return ""
