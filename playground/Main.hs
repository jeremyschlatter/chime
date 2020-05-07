{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, OverloadedStrings #-}
module Main where

import BasePrelude hiding (error)
import qualified Codec.Compression.Zstd as Z
import Crypto.Hash.SHA256 (hash)
import Data.Aeson hiding (json, Error)
import Data.ByteString.Base64.URL (decodeBase64Lenient, encodeBase64Unpadded)
import qualified Data.ByteString as B
import qualified Data.ByteString.Delta as Diff
import Network.Google.OAuth2.JWT (getSignedJWT, fromPEMString)
import Network.HTTP.Simple
import Network.HTTP.Types.Status
import Web.Scotty
import Web.Scotty.Trans (ActionT)

import Data.Text as T
import Data.Text.Encoding

import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Text.Lazy as LT

import CacheBelDotBel
import Data as Data
import Eval

data Credentials = Credentials
  { client_email :: Text
  , private_key :: Text
  } deriving (Generic, FromJSON)

data JWTResponse = JWTResponse
  { access_token :: Text
  } deriving (Generic, FromJSON)

data ErrorResponse = ErrorResponse
  { error :: Text
  , error_description :: Text
  } deriving (Generic, FromJSON)

data UploadResponse = UploadResponse
  { mediaLink :: Text
  } deriving (Generic, FromJSON)

data APIResponse a = Error ErrorResponse | Response a deriving Generic
instance (Generic a, FromJSON a) => FromJSON (APIResponse a) where
  parseJSON = genericParseJSON $ defaultOptions { sumEncoding = UntaggedValue }

-- https://cloud.google.com/run/docs/securing/service-identity#access_tokens
refreshTokenInProduction :: IO Text
refreshTokenInProduction = httpJSON
  ("http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token?scopes=https://www.googleapis.com/auth/devstorage.read_write"
  & addRequestHeader "Metadata-Flavor" (encodeUtf8 "Google")) >>= (. getResponseBody) \case
    Error e -> do
      putStrLn $ unpack $ "token refresh failed: " <> error e <> ": " <> error_description e
      pure ""
    Response r -> pure $ access_token r

refreshTokenFromFile :: FilePath -> IO Text
refreshTokenFromFile = eitherDecodeFileStrict' >=> \case
  Left err -> die err
  Right (Credentials email pem) -> fromPEMString (unpack pem) >>= \key ->
    getSignedJWT email Nothing ["https://www.googleapis.com/auth/devstorage.read_write"] Nothing key >>= \case
      Left err -> die err
      Right jwt -> parseRequestThrow
        ("POST https://oauth2.googleapis.com/token"
         <> "?grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion="
         <> show jwt) >>= fmap getResponseBody . httpJSON >>= \case
           Error e -> die $ unpack $ "token refresh failed: " <> error e <> ": " <> error_description e
           Response r -> pure $ access_token r

refreshToken :: IO Text
refreshToken = lookupEnv var >>= \case
  Just f -> refreshTokenFromFile f
  Nothing -> do
    putStrLn $ "No " <> var <> " is set, so I assume I'm in production"
    refreshTokenInProduction
  where
    var = "GOOGLE_APPLICATION_CREDENTIALS"

belDotBelState :: String
belDotBelState = $(serializedBelDotBelState)

data StatefulRequest = StatefulRequest
  { expr :: String
  , state :: LT.Text
  } deriving (Generic, FromJSON)

data StatefulResponse = StatefulResponse
  { result :: String
  , state :: LT.Text
  } deriving (Generic, ToJSON)

belDotBelStateBytes :: B.ByteString
belDotBelStateBytes = encodeUtf8 $ pack belDotBelState

compressState :: String -> LT.Text
compressState =
  LT.fromStrict
  . encodeBase64Unpadded
  . Z.compress 1
  . Diff.diff belDotBelStateBytes
  . encodeUtf8
  . pack

decompressState :: LT.Text -> String
decompressState =
  unpack
  . decodeUtf8
  . fromRight B.empty . Diff.patch belDotBelStateBytes
  . (\case Z.Decompress d -> d; _ -> B.empty)
  . Z.decompress
  . decodeBase64Lenient
  . encodeUtf8
  . LT.toStrict

stateful ::
  EvalState
  -> (String -> String -> EvalState -> IO (Either String (Data.Object IORef), EvalState))
  -> ActionT LT.Text IO ()
stateful baseState f = do
  addHeader "Access-Control-Allow-Origin" "*"
  body >>= (. decode) \case
    Nothing -> text "malformed request"
    Just req -> do
      hydrated <- case state (req :: StatefulRequest) of
        "" -> pure baseState
        s -> stringToState (decompressState s) >>=
          either
            ((*> finish) . text . LT.fromStrict . pack . ("malformed state: " <>))
            pure
      (r, s) <- liftIO $ f "" (expr req) hydrated
      r' <- either pure repr r
      s' <- compressState <$> stateToString s
      json $ StatefulResponse {result=r', state=s'}

main :: IO ()
main = do
  -- Parse the default starting interpreter state.
  baseState <- stringToState belDotBelState >>= either die pure

  -- Grab an auth token for Google Cloud Storage and refresh every 30 minutes.
  token <- refreshToken >>= newIORef
  _ <- forkIO $ forever do
    threadDelay (30 * 60 * 1_000_000) -- 30 minutes, expressed in microseconds
    refreshToken >>= atomicWriteIORef token

  -- Serve HTTP requests until we die.
  (read . fromMaybe "8080" <$> lookupEnv "PORT") >>= flip scotty do

    post "/" do
      addHeader "Access-Control-Allow-Origin" "*"
      body >>= liftIO . flip (readThenRunEval "") baseState . unpack
          . decodeUtf8 . LazyBytes.toStrict
        >>= either pure repr . fst >>= text . LT.fromStrict . pack

    post "/stateful" $ stateful baseState readThenRunEval

    post "/stateful-long" $ stateful baseState readManyThenRunEval

    post "/share" do
      addHeader "Access-Control-Allow-Origin" "*"
      p <- body
      let name = encodeBase64Unpadded $ B.take 10 $ hash $ LazyBytes.toStrict p
      auth <- liftIO $ readIORef token
      r <- parseRequestThrow
        ("POST https://storage.googleapis.com/upload/storage/v1/b/chime-snippets/o"
        <> "?uploadType=media&name=" <> unpack name)
        <&> addRequestHeader "Authorization" (encodeUtf8 $ "Bearer " <> auth)
        <&> setRequestBodyLBS p
      httpJSON r >>= (. getResponseBody) \case
        Error e -> do
          liftIO $ print $ "upload error: " <> error e <> ": " <> error_description e
          status status500
          json @Text "failed to store program"
        Response (_ :: UploadResponse) ->
          json $ ShareResponse name

    get "/" $ text "Chime API Server - send me Bel code and get eval results!"

data ShareResponse = ShareResponse
  { share_id :: Text
  } deriving (Generic, ToJSON)
