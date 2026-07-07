{-# LANGUAGE OverloadedStrings #-}

module MnemonicManifold.Ids
  ( shortHashHex16
  ) where

import Data.Word (Word8)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import MnemonicManifold.SHA256 (sha256)

shortHashHex16 :: Text -> Text
shortHashHex16 t =
  let d = sha256 (TE.encodeUtf8 t)
      first8 = BS.take 8 d
  in bsToHex first8

bsToHex :: BS.ByteString -> Text
bsToHex bs = T.concat (map byteHex (BS.unpack bs))
  where
    byteHex :: Word8 -> Text
    byteHex w =
      let hex = "0123456789abcdef"
          hi = fromIntegral (w `div` 16)
          lo = fromIntegral (w `mod` 16)
      in T.pack [hex !! hi, hex !! lo]

