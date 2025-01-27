{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Types
Description : Data types for Bike Point Management

This module defines data types used in the Bike Point Management application, including:

- `BikePoint`: Represents a bike point with its properties.
- `BikePointPartial`: A simplified version of `BikePoint` for partial queries.
- `BikePointProperty`: Represents a property associated with a bike point.
- `BikePointDetails`: Represents a bike point and its associated properties as a single entity.
- `BikePoints`: A wrapper for multiple `BikePoint` records.

These types are designed for use with JSON serialization and database operations.
-}
module Types (
    BikePoint (..),
    BikePointProperty(..),
    BikePointPartial(..),
    KeyValueBikePointDetails(..),
    BikePointOccupancy(..),
    AllBikePointDetails(..),

) where

import GHC.Generics
import Data.Time (UTCTime)
import Prelude hiding (id)

-- | Represents a bike point with associated properties.

data BikePoint = BikePoint
  { bp_id :: String         
  , url :: String  
  , lat :: Double
  , lon :: Double      
  , commonName :: String  
  , additionalProperties :: [BikePointProperty]
  } deriving (Show, Eq, Generic)

-- | Represents a simplified version of a bike point for partial queries.
data BikePointPartial = BikePointPartial
  { 
    ltd :: Double
  , lng :: Double      
  , cName :: String  
  } deriving (Show, Eq, Generic)

-- | Represents a single property associated with a bike point.
data BikePointProperty = BikePointProperty
  { 
    category  :: Maybe String            
  , key :: Maybe String                 
  , sourceSystemKey :: Maybe String     
  , value :: Maybe String               
  , modified :: Maybe UTCTime           
  } deriving (Show, Eq, Generic)

-- | Represents information for a bike point properties .
data BikePointDetails = BikePointDetails
  { 
    bike_point_id :: String,
    bikeProperties :: [BikePointProperty]           
         
  } deriving (Show, Eq, Generic)

data KeyValueBikePointDetails = KeyValueBikePointDetails
  { 
    key_bp :: String,
    value_bp :: String           
         
  } deriving (Show, Eq, Generic)
-- | Wrapper type for multiple bike points.
data BikePoints = BikePoints {
    bikePointProperties :: [BikePoint]
} deriving (Show, Generic)

-- | Represents detailed information for a bike point occupancy, including all properties.

data BikePointOccupancy = BikePointOccupancy
  { 
    bpo_id             :: String
  , name              ::  String
  , bikesCount        ::  Int
  , emptyDocks        ::  Int
  , totalDocks        :: Int
  , standardBikesCount :: Int
  , eBikesCount       :: Int
  } deriving (Show, Eq, Generic)

-- | Represents all detailed information for a bike point in database.

data AllBikePointDetails = AllBikePointDetails {
    bpnt_id :: String         
  , bp_url :: String  
  , bp_lat :: Double
  , bp_lon :: Double      
  , bp_commonName :: String  
  , properties :: [BikePointProperty],
    occupancy :: Maybe BikePointOccupancy

  }deriving (Show, Eq, Generic)


