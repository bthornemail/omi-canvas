{-# LANGUAGE OverloadedStrings #-}

module ULP.Types where

import Data.Aeson
  ( FromJSON (parseJSON)
  , ToJSON (toJSON)
  , Value (Object, String)
  , object
  , withObject
  , withText
  , (.:)
  , (.:?)
  , (.=)
  )
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T

data ChannelState = Down | Opening | Open | Closing | Closed
  deriving (Show, Eq)

instance ToJSON ChannelState where
  toJSON v = String $ case v of
    Down -> "down"
    Opening -> "opening"
    Open -> "open"
    Closing -> "closing"
    Closed -> "closed"

instance FromJSON ChannelState where
  parseJSON = withText "ChannelState" $ \t -> case t of
    "down" -> pure Down
    "opening" -> pure Opening
    "open" -> pure Open
    "closing" -> pure Closing
    "closed" -> pure Closed
    _ -> fail "invalid ChannelState"

data CommitStatus = Pending | Validated | Sealed | Quarantined
  deriving (Show, Eq)

instance ToJSON CommitStatus where
  toJSON v = String $ case v of
    Pending -> "pending"
    Validated -> "validated"
    Sealed -> "sealed"
    Quarantined -> "quarantined"

instance FromJSON CommitStatus where
  parseJSON = withText "CommitStatus" $ \t -> case t of
    "pending" -> pure Pending
    "validated" -> pure Validated
    "sealed" -> pure Sealed
    "quarantined" -> pure Quarantined
    _ -> fail "invalid CommitStatus"

data FaceStatus = Pass | Fail | Unknown
  deriving (Show, Eq)

instance ToJSON FaceStatus where
  toJSON v = String $ case v of
    Pass -> "pass"
    Fail -> "fail"
    Unknown -> "unknown"

instance FromJSON FaceStatus where
  parseJSON = withText "FaceStatus" $ \t -> case t of
    "pass" -> pure Pass
    "fail" -> pure Fail
    "unknown" -> pure Unknown
    _ -> fail "invalid FaceStatus"

data CommitType = VertexInit | EdgeUpdate | FaceEval | Commit | Projection | Sync
  deriving (Show, Eq)

instance ToJSON CommitType where
  toJSON v = String $ case v of
    VertexInit -> "vertex_init"
    EdgeUpdate -> "edge_update"
    FaceEval -> "face_eval"
    Commit -> "commit"
    Projection -> "projection"
    Sync -> "sync"

instance FromJSON CommitType where
  parseJSON = withText "CommitType" $ \t -> case t of
    "vertex_init" -> pure VertexInit
    "edge_update" -> pure EdgeUpdate
    "face_eval" -> pure FaceEval
    "commit" -> pure Commit
    "projection" -> pure Projection
    "sync" -> pure Sync
    _ -> fail "invalid CommitType"

data VertexIdentity = VertexIdentity
  { vertex_id :: Text
  , path :: Text
  , address :: Text
  , pubkey :: Text
  , fano_point_id :: Int
  } deriving (Show, Eq)

instance ToJSON VertexIdentity where
  toJSON v =
    object
      [ "vertex_id" .= vertex_id v
      , "path" .= path v
      , "address" .= address v
      , "pubkey" .= pubkey v
      , "fano_point_id" .= fano_point_id v
      ]

instance FromJSON VertexIdentity where
  parseJSON = withObject "VertexIdentity" $ \o ->
    VertexIdentity
      <$> o .: "vertex_id"
      <*> o .: "path"
      <*> o .: "address"
      <*> o .: "pubkey"
      <*> o .: "fano_point_id"

data EdgeState = EdgeState
  { edge_id :: Text
  , from :: Text
  , to :: Text
  , channel_state :: ChannelState
  , last_seq :: Int
  } deriving (Show, Eq)

instance ToJSON EdgeState where
  toJSON e =
    object
      [ "edge_id" .= edge_id e
      , "from" .= from e
      , "to" .= to e
      , "channel_state" .= channel_state e
      , "last_seq" .= last_seq e
      ]

instance FromJSON EdgeState where
  parseJSON = withObject "EdgeState" $ \o ->
    EdgeState
      <$> o .: "edge_id"
      <*> o .: "from"
      <*> o .: "to"
      <*> o .: "channel_state"
      <*> o .: "last_seq"

data FaceInvariant = FaceInvariant
  { face_id :: Text
  , vertices :: [Text]
  , invariant_name :: Text
  , status :: FaceStatus
  , evidence :: Maybe Value
  } deriving (Show, Eq)

instance ToJSON FaceInvariant where
  toJSON f =
    object
      [ "face_id" .= face_id f
      , "vertices" .= vertices f
      , "invariant_name" .= invariant_name f
      , "status" .= status f
      , "evidence" .= evidence f
      ]

instance FromJSON FaceInvariant where
  parseJSON = withObject "FaceInvariant" $ \o ->
    FaceInvariant
      <$> o .: "face_id"
      <*> o .: "vertices"
      <*> o .: "invariant_name"
      <*> o .: "status"
      <*> o .:? "evidence"

data CentroidState = CentroidState
  { stop_metric :: Double
  , closure_ratio :: Double
  , sabbath :: Bool
  , reason :: Text
  } deriving (Show, Eq)

instance ToJSON CentroidState where
  toJSON c =
    object
      [ "stop_metric" .= stop_metric c
      , "closure_ratio" .= closure_ratio c
      , "sabbath" .= sabbath c
      , "reason" .= reason c
      ]

instance FromJSON CentroidState where
  parseJSON = withObject "CentroidState" $ \o ->
    CentroidState
      <$> o .: "stop_metric"
      <*> o .: "closure_ratio"
      <*> o .: "sabbath"
      <*> o .: "reason"

data Merkle = Merkle
  { version :: Text
  , sections :: [(Text, Text)]
  , leaf_order :: [Text]
  , root :: Text
  } deriving (Show, Eq)

instance ToJSON Merkle where
  toJSON m =
    object
      [ "version" .= version m
      , "sections" .= Object (KM.fromList (map (\(k, v) -> (K.fromText k, String v)) (sections m)))
      , "leaf_order" .= leaf_order m
      , "root" .= root m
      ]

instance FromJSON Merkle where
  parseJSON = withObject "Merkle" $ \o -> do
    sectionsObj <- o .: "sections"
    sectionPairs <- case sectionsObj of
      Object km ->
        pure
          ( sortOn fst
              (map
              (\(k, v) -> (K.toText k, case v of String t -> t; _ -> T.empty))
              (KM.toList km))
          )
      _ -> fail "merkle.sections must be object"
    Merkle
      <$> o .: "version"
      <*> pure sectionPairs
      <*> o .: "leaf_order"
      <*> o .: "root"

data CommitEvent = CommitEvent
  { cid :: Text
  , t :: Int64
  , lc :: Maybe Int
  , ctype :: CommitType
  , parents :: [Text]
  , identities :: Maybe [VertexIdentity]
  , vertex :: Maybe VertexIdentity
  , edges :: [EdgeState]
  , faces :: [FaceInvariant]
  , centroid :: CentroidState
  , cstatus :: CommitStatus
  , prev_hash :: Maybe Text
  , merkle :: Maybe Merkle
  , self_hash :: Text
  , sig :: Text
  } deriving (Show, Eq)

instance ToJSON CommitEvent where
  toJSON c =
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
      , "self_hash" .= self_hash c
      , "sig" .= sig c
      ]

instance FromJSON CommitEvent where
  parseJSON = withObject "CommitEvent" $ \o ->
    CommitEvent
      <$> o .: "id"
      <*> o .: "t"
      <*> o .:? "lc"
      <*> o .: "type"
      <*> o .: "parents"
      <*> o .:? "identities"
      <*> o .:? "vertex"
      <*> o .: "edges"
      <*> o .: "faces"
      <*> o .: "centroid"
      <*> o .: "status"
      <*> o .:? "prev_hash"
      <*> o .:? "merkle"
      <*> o .: "self_hash"
      <*> o .: "sig"

data ValidationResult = ValidationResult
  { valid :: Bool
  , errors :: [Text]
  } deriving (Show, Eq)

instance ToJSON ValidationResult where
  toJSON v = object ["valid" .= valid v, "errors" .= errors v]

instance FromJSON ValidationResult where
  parseJSON = withObject "ValidationResult" $ \o ->
    ValidationResult <$> o .: "valid" <*> o .: "errors"

-- Runtime options and hooks
data ValidationOptions = ValidationOptions
  { signatureVerifier :: Maybe (CommitEvent -> Text -> IO Bool)
  , invariantChecker :: Maybe (CommitEvent -> IO Bool)
  }

data RuntimeOptions = RuntimeOptions
  { clock :: IO Int64
  , counterStart :: Int
  , signer :: Maybe (CommitEvent -> Text -> IO Text)
  , verifier :: Maybe (CommitEvent -> Text -> IO Bool)
  , storageRoot :: FilePath
  }
