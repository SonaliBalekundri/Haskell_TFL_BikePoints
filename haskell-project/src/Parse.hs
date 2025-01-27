{-|
Module      : Parse
Description : Parsing and Serialization of Bike Point Data

This module provides functionality for working with bike point data. It includes
functions for parsing JSON into `BikePoint` and `BikePointProperty` records, as
well as serializing these records into human-readable JSON files.

Key Features:
  - Parse JSON into strongly-typed Haskell records.
  - Serialize Haskell records into pretty-printed JSON for export.
-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Parse (
    parseBikePointProperties,
    dumpToJsonFile,
    parseBikePointOccupancies
) where

import Types ( BikePointProperty,BikePoint,BikePointOccupancy,AllBikePointDetails)
import Data.Aeson
    ( FromJSON(parseJSON),
      ToJSON,
      encode,
      eitherDecode,
      genericParseJSON,
      defaultOptions,
      Options(fieldLabelModifier), json )
import Data.Aeson.Encode.Pretty (encodePretty)

import qualified Data.ByteString.Lazy.Char8 as L8
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import System.Process (callCommand)

-- | Rename fields from json with our properties in datatype`.
renameFields :: String -> String -> String
renameFields prefix fieldName
    | fieldName == prefix ++ "_id" = "id" 
    | otherwise = fieldName

customOptions :: String -> Options
customOptions prefix = defaultOptions {
    fieldLabelModifier = renameFields prefix
}

-- | JSON decoding for `BikePointProperty` ,BikePointOccupancy and `BikePoint`.
instance FromJSON BikePoint where
    parseJSON = genericParseJSON (customOptions "bp")

instance FromJSON BikePointOccupancy where
    parseJSON = genericParseJSON (customOptions "bpo")

instance FromJSON BikePointProperty


-- | JSON encoding for `BikePointProperty`,  `BikePointOccupancy `,`BikePoint` and AllBikePointDetails.
instance ToJSON BikePointProperty
instance ToJSON BikePoint
instance ToJSON BikePointOccupancy
instance ToJSON AllBikePointDetails


-- | Parses a JSON-encoded byte string into a list of BikePoint records.
parseBikePointProperties :: L8.ByteString -> Either String [BikePoint]
parseBikePointProperties json = eitherDecode json :: Either String [BikePoint]


-- | Parses a JSON-encoded byte string into a list of `BikePointOccupancy` records.
parseBikePointOccupancies :: L8.ByteString -> Either String [BikePointOccupancy]
parseBikePointOccupancies json = eitherDecode json :: Either String [BikePointOccupancy]


-- | Dumps a list of bike points to a JSON file in a human-readable(pretty) format.
dumpToJsonFile :: FilePath -> [AllBikePointDetails] -> IO ()
dumpToJsonFile filePath bikePoints = do
    let jsonData = encodePretty bikePoints
    L8.writeFile filePath jsonData