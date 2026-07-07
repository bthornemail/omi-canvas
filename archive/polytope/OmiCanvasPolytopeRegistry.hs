{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | OMI Canvas polytope registry types.
-- This module treats the uploaded registry as projection data only.
-- Rendering/projection is not OMI authority.
module OMI.Canvas.Polytope.Registry
  ( Registry(..)
  , RenderContract(..)
  , Polytope(..)
  , ConfigurationWitness(..)
  , FlexibleInt(..)
  , FlexibleBool(..)
  , loadRegistry
  , polytopesByDimension
  , polytopesByCategory
  , finiteTemplates
  , familyTemplates
  , toCanvas
  , Canvas(..)
  , CanvasNode(..)
  , CanvasEdge(..)
  ) where

import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value(..), (.:), (.:?), (.!=), (.=), object, withObject, withText
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Registry fields such as vertices can be numeric, symbolic ("n+1"),
-- or infinity ("∞"). Keep the symbolic value instead of forcing a number.
data FlexibleInt
  = KnownInt Int
  | SymbolicInt Text
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FlexibleInt where
  parseJSON (Number n) =
    case toBoundedInteger n of
      Just i  -> pure (KnownInt i)
      Nothing -> fail "number outside Int range"
  parseJSON (String s) = pure (SymbolicInt s)
  parseJSON v = fail ("expected integer or symbolic text, got " <> show v)

instance ToJSON FlexibleInt where
  toJSON (KnownInt i) = toJSON i
  toJSON (SymbolicInt s) = toJSON s

-- | Some fields in hand-curated registries may arrive as bool or text.
newtype FlexibleBool = FlexibleBool { getFlexibleBool :: Bool }
  deriving (Eq, Ord, Show, Generic)

instance FromJSON FlexibleBool where
  parseJSON (Bool b) = pure (FlexibleBool b)
  parseJSON (String "true") = pure (FlexibleBool True)
  parseJSON (String "false") = pure (FlexibleBool False)
  parseJSON v = fail ("expected boolean, got " <> show v)

instance ToJSON FlexibleBool where
  toJSON (FlexibleBool b) = toJSON b

data RenderContract = RenderContract
  { rcId        :: Maybe Text
  , rcName      :: Maybe Text
  , rcSchlafli  :: Maybe Text
  , rcDimension :: Maybe Text
  , rcCategory  :: Maybe Text
  , rcCounts    :: Maybe Text
  , rcDual      :: Maybe Text
  , rcFamily    :: Maybe Text
  , rcFinite    :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON RenderContract where
  parseJSON = withObject "RenderContract" $ \o ->
    RenderContract <$> o .:? "id"
                   <*> o .:? "name"
                   <*> o .:? "schlafli"
                   <*> o .:? "dimension"
                   <*> o .:? "category"
                   <*> o .:? "counts"
                   <*> o .:? "dual"
                   <*> o .:? "family"
                   <*> o .:? "finite"

instance ToJSON RenderContract where
  toJSON r = object
    [ "id" .= rcId r
    , "name" .= rcName r
    , "schlafli" .= rcSchlafli r
    , "dimension" .= rcDimension r
    , "category" .= rcCategory r
    , "counts" .= rcCounts r
    , "dual" .= rcDual r
    , "family" .= rcFamily r
    , "finite" .= rcFinite r
    ]

data Polytope = Polytope
  { polyId        :: Text
  , polyName      :: Text
  , polyRank      :: Maybe FlexibleInt
  , polyDimension :: Maybe FlexibleInt
  , polyCategory  :: Text
  , polySchlafli  :: Maybe Text
  , polyVertices  :: Maybe FlexibleInt
  , polyEdges     :: Maybe FlexibleInt
  , polyFaces     :: Maybe FlexibleInt
  , polyCells     :: Maybe FlexibleInt
  , polyFacets    :: Maybe FlexibleInt
  , polyDual      :: Maybe Text
  , polyFamily    :: Maybe Bool
  , polyFinite    :: Maybe Bool
  , polyNotes     :: Maybe Text
  , polyOmiPlane  :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON Polytope where
  parseJSON = withObject "Polytope" $ \o ->
    Polytope <$> o .:  "id"
             <*> o .:  "name"
             <*> o .:? "rank"
             <*> o .:? "dimension"
             <*> o .:  "category"
             <*> o .:? "schlafli"
             <*> o .:? "vertices"
             <*> o .:? "edges"
             <*> o .:? "faces"
             <*> o .:? "cells"
             <*> o .:? "facets"
             <*> o .:? "dual"
             <*> o .:? "family"
             <*> o .:? "finite"
             <*> o .:? "notes"
             <*> o .:? "omiPlane"

instance ToJSON Polytope where
  toJSON p = object
    [ "id" .= polyId p
    , "name" .= polyName p
    , "rank" .= polyRank p
    , "dimension" .= polyDimension p
    , "category" .= polyCategory p
    , "schlafli" .= polySchlafli p
    , "vertices" .= polyVertices p
    , "edges" .= polyEdges p
    , "faces" .= polyFaces p
    , "cells" .= polyCells p
    , "facets" .= polyFacets p
    , "dual" .= polyDual p
    , "family" .= polyFamily p
    , "finite" .= polyFinite p
    , "notes" .= polyNotes p
    , "omiPlane" .= polyOmiPlane p
    ]

data ConfigurationWitness = ConfigurationWitness
  { witnessId          :: Text
  , witnessName        :: Text
  , witnessSymbol      :: Maybe Text
  , witnessPoints      :: Maybe FlexibleInt
  , witnessBlocks      :: Maybe FlexibleInt
  , witnessPointDegree :: Maybe FlexibleInt
  , witnessBlockDegree :: Maybe FlexibleInt
  , witnessFlags       :: Maybe FlexibleInt
  , witnessRole        :: Maybe Text
  , witnessAuthority   :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON ConfigurationWitness where
  parseJSON = withObject "ConfigurationWitness" $ \o ->
    ConfigurationWitness <$> o .:  "id"
                         <*> o .:  "name"
                         <*> o .:? "symbol"
                         <*> o .:? "points"
                         <*> o .:? "blocks"
                         <*> o .:? "pointDegree"
                         <*> o .:? "blockDegree"
                         <*> o .:? "flags"
                         <*> o .:? "role"
                         <*> o .:? "authority"

instance ToJSON ConfigurationWitness where
  toJSON w = object
    [ "id" .= witnessId w
    , "name" .= witnessName w
    , "symbol" .= witnessSymbol w
    , "points" .= witnessPoints w
    , "blocks" .= witnessBlocks w
    , "pointDegree" .= witnessPointDegree w
    , "blockDegree" .= witnessBlockDegree w
    , "flags" .= witnessFlags w
    , "role" .= witnessRole w
    , "authority" .= witnessAuthority w
    ]

data Registry = Registry
  { registrySchema                 :: Text
  , registryGenerated              :: Maybe Text
  , registryInvariant              :: Maybe Text
  , registrySource                 :: Maybe Text
  , registryRenderContract         :: Maybe RenderContract
  , registryPolytopes              :: [Polytope]
  , registryConfigurationWitnesses :: [ConfigurationWitness]
  } deriving (Eq, Show, Generic)

instance FromJSON Registry where
  parseJSON = withObject "Registry" $ \o ->
    Registry <$> o .:  "schema"
             <*> o .:? "generated"
             <*> o .:? "invariant"
             <*> o .:? "source"
             <*> o .:? "renderContract"
             <*> o .:  "polytopes"
             <*> o .:? "omiConfigurationWitnesses" .!= []

instance ToJSON Registry where
  toJSON r = object
    [ "schema" .= registrySchema r
    , "generated" .= registryGenerated r
    , "invariant" .= registryInvariant r
    , "source" .= registrySource r
    , "renderContract" .= registryRenderContract r
    , "polytopes" .= registryPolytopes r
    , "omiConfigurationWitnesses" .= registryConfigurationWitnesses r
    ]

loadRegistry :: FilePath -> IO (Either String Registry)
loadRegistry fp = Aeson.eitherDecode <$> BL.readFile fp

polytopesByCategory :: Registry -> Map Text [Polytope]
polytopesByCategory =
  foldr ins Map.empty . registryPolytopes
  where
    ins p = Map.insertWith (<>) (polyCategory p) [p]

polytopesByDimension :: Registry -> Map Text [Polytope]
polytopesByDimension =
  foldr ins Map.empty . registryPolytopes
  where
    showDim Nothing = "unknown"
    showDim (Just (KnownInt n)) = T.pack (show n)
    showDim (Just (SymbolicInt s)) = s
    ins p = Map.insertWith (<>) (showDim (polyDimension p)) [p]

finiteTemplates :: Registry -> [Polytope]
finiteTemplates =
  filter (\p -> fromMaybe False (polyFinite p) && not (fromMaybe False (polyFamily p)))
  . registryPolytopes

familyTemplates :: Registry -> [Polytope]
familyTemplates =
  filter (fromMaybe False . polyFamily) . registryPolytopes

-- Minimal Obsidian Canvas export types.
data Canvas = Canvas
  { nodes :: [CanvasNode]
  , edges :: [CanvasEdge]
  } deriving (Eq, Show, Generic)

instance ToJSON Canvas where
  toJSON c = object ["nodes" .= nodes c, "edges" .= edges c]

data CanvasNode = CanvasNode
  { nodeId     :: Text
  , nodeType   :: Text
  , nodeX      :: Int
  , nodeY      :: Int
  , nodeWidth  :: Int
  , nodeHeight :: Int
  , nodeText   :: Text
  , nodeColor  :: Maybe Text
  } deriving (Eq, Show, Generic)

instance ToJSON CanvasNode where
  toJSON n = object
    [ "id" .= nodeId n
    , "type" .= nodeType n
    , "x" .= nodeX n
    , "y" .= nodeY n
    , "width" .= nodeWidth n
    , "height" .= nodeHeight n
    , "text" .= nodeText n
    , "color" .= nodeColor n
    ]

data CanvasEdge = CanvasEdge
  deriving (Eq, Show, Generic)

instance ToJSON CanvasEdge where
  toJSON CanvasEdge = object []

toCanvas :: Registry -> Canvas
toCanvas reg =
  Canvas
    { nodes = zipWith polyNode [0..] (registryPolytopes reg)
           <> zipWith witnessNode [0..] (registryConfigurationWitnesses reg)
    , edges = []
    }
  where
    showFlex Nothing = ""
    showFlex (Just (KnownInt n)) = T.pack (show n)
    showFlex (Just (SymbolicInt s)) = s

    dimColumn p =
      case polyDimension p of
        Just (KnownInt n) -> n
        _ -> 11

    polyNode idx p =
      let x = 360 * dimColumn p
          y = 180 * idx
          body = T.unlines
            [ "# " <> polyName p
            , "id: `" <> polyId p <> "`"
            , "category: " <> polyCategory p
            , "dimension: " <> showFlex (polyDimension p)
            , "rank: " <> showFlex (polyRank p)
            , "schlafli: " <> fromMaybe "" (polySchlafli p)
            , "V/E/F/C/Facets: "
                <> showFlex (polyVertices p) <> " / "
                <> showFlex (polyEdges p) <> " / "
                <> showFlex (polyFaces p) <> " / "
                <> showFlex (polyCells p) <> " / "
                <> showFlex (polyFacets p)
            , "dual: " <> fromMaybe "" (polyDual p)
            , "family: " <> T.pack (show (fromMaybe False (polyFamily p)))
            , "finite: " <> T.pack (show (fromMaybe False (polyFinite p)))
            , "omiPlane: " <> fromMaybe "" (polyOmiPlane p)
            ]
      in CanvasNode
          { nodeId = "polytope-" <> polyId p
          , nodeType = "text"
          , nodeX = x
          , nodeY = y
          , nodeWidth = 320
          , nodeHeight = 220
          , nodeText = body
          , nodeColor = Just "1"
          }

    witnessNode idx w =
      CanvasNode
        { nodeId = "witness-" <> witnessId w
        , nodeType = "text"
        , nodeX = 0
        , nodeY = 180 * idx
        , nodeWidth = 340
        , nodeHeight = 220
        , nodeText = T.unlines
            [ "# " <> witnessName w
            , "id: `" <> witnessId w <> "`"
            , "symbol: " <> fromMaybe "" (witnessSymbol w)
            , "points/blocks: " <> showFlex (witnessPoints w) <> " / " <> showFlex (witnessBlocks w)
            , "degrees: " <> showFlex (witnessPointDegree w) <> " / " <> showFlex (witnessBlockDegree w)
            , "flags: " <> showFlex (witnessFlags w)
            , "role: " <> fromMaybe "" (witnessRole w)
            , "authority: " <> fromMaybe "" (witnessAuthority w)
            ]
        , nodeColor = Just "6"
        }
