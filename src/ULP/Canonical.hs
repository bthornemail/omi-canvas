{-# LANGUAGE OverloadedStrings #-}

module ULP.Canonical
  ( stableJson
  , sha256Hex
  , canonicalPayload
  ) where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import ULP.Types

normalizeValue :: Value -> Value
normalizeValue (Object obj) =
  let sorted = sortOn (K.toText . fst) (KM.toList obj)
   in Object (KM.fromList (map (\(k, v) -> (k, normalizeValue v)) sorted))
normalizeValue (Array arr) = Array (fmap normalizeValue arr)
normalizeValue v = v

stableJson :: Value -> BS.ByteString
stableJson = LBS.toStrict . encode . normalizeValue

sha256Hex :: BS.ByteString -> Text
sha256Hex bs =
  let d :: Digest SHA256
      d = hash bs
      hex = T.toLower (T.pack (show d))
   in TE.decodeUtf8 (BSC.pack "0x" <> TE.encodeUtf8 hex)

commitToCanonicalValue :: CommitEvent -> Value
commitToCanonicalValue c =
  object
    [ "id" .= cid c
    , "t" .= t c
    , "lc" .= lc c
    , "type" .= ctype c
    , "parents" .= parents c
    , "identities" .= identities c
    , "vertex" .= vertex c
    , "edges" .= edges c
    , "faces" .= faces c
    , "centroid" .= centroid c
    , "status" .= cstatus c
    , "prev_hash" .= prev_hash c
    , "merkle" .= merkle c
    ]

canonicalPayload :: CommitEvent -> BS.ByteString
canonicalPayload = stableJson . commitToCanonicalValue
