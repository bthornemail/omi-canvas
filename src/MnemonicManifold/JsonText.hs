{-# LANGUAGE OverloadedStrings #-}

module MnemonicManifold.JsonText
  ( jsonNull
  , jsonBool
  , jsonInt
  , jsonInteger
  , jsonWord64
  , jsonText
  , jsonArray
  , jsonObj
  ) where

import Data.Bits ((.&.))
import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

jsonNull :: Text
jsonNull = "null"

jsonBool :: Bool -> Text
jsonBool True = "true"
jsonBool False = "false"

jsonInt :: Int -> Text
jsonInt = T.pack . show

jsonInteger :: Integer -> Text
jsonInteger = T.pack . show

jsonWord64 :: Word64 -> Text
jsonWord64 = T.pack . show

jsonText :: Text -> Text
jsonText t = "\"" <> T.concatMap escape t <> "\""
  where
    escape :: Char -> Text
    escape c = case c of
      '"'  -> "\\\""
      '\\' -> "\\\\"
      '\b' -> "\\b"
      '\f' -> "\\f"
      '\n' -> "\\n"
      '\r' -> "\\r"
      '\t' -> "\\t"
      _ | ord c < 0x20 ->
            let hex = "0123456789abcdef"
                n = ord c
                a = (n `div` 16) .&. 0xF
                b = n .&. 0xF
            in "\\u00" <> T.pack [hex !! a, hex !! b]
        | otherwise -> T.singleton c

jsonArray :: [Text] -> Text
jsonArray xs = "[" <> T.intercalate "," xs <> "]"

jsonObj :: [(Text, Text)] -> Text
jsonObj kvs =
  "{" <> T.intercalate "," (map render kvs) <> "}"
  where
    render (k,v) = jsonText k <> ":" <> v
