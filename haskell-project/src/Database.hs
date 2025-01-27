{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant return" #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-|
Module      : Database
Description : Database management for Bike Point application
Copyright   : (c) Your Name, 2024
License     : MIT
This module provides functions for managing the database in the Bike Point application.
It includes functionality for initializing the database, creating tables, and
performing CRUD operations for bike points and their properties.

Key Features:
  - Initialize and manage SQLite database schema.
  - Insert bike points and associated properties into the database.
  - Query bike points and properties from the database.

Example usage:

@
-- Initialize the database
conn <- initialiseDB

-- Create tables
createTables conn

-- Insert a bike point with properties
insertBikePointWithProperties conn bikePoint
@
-}
module Database (
    initialiseDB,
    createTables,
    insertBikePointWithProperties,
    insertAllBikePoints,
    queryBikePoints,
    queryAllBpEntries,
    queryBikePointKeyValueDetails,
    saveInsertBikeOccupancy,
    queryBikePointOccupancyComparison,
    findNearbyBikePoints,
    queryBikeAvailability
) where
import Control.Exception (try,catch, SomeException)
import Types
import Database.SQLite.Simple
    ( execute,
      execute_,
      open,
      query,
      query_,
      field,
      SQLData,
      FromRow(..),
      Connection,
      ToRow(..),
      Only(..) )
import Database.SQLite.Simple.Internal
import Control.Applicative
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow 
import Prelude hiding (id)
import Data.Time (UTCTime)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.String (IsString)


-- | Defines how to map database rows to `BikePointProperty` records.
instance FromRow BikePointProperty where
  fromRow = BikePointProperty <$> field <*> field <*> field <*> field <*> field

-- | Defines how to map database rows to `BikePoint` records.
instance FromRow BikePoint where
 fromRow :: RowParser BikePoint
 fromRow = BikePoint <$> field <*> field <*> field <*> field <*> field <*> pure []

-- | Defines how to map database rows to `BikePointPartial` records.
instance FromRow BikePointPartial where
 fromRow = BikePointPartial <$> field <*> field <*> field

-- | Defines how to map database rows to `BikePointPartial` records.
instance FromRow KeyValueBikePointDetails where
 fromRow = KeyValueBikePointDetails <$> field <*> field
-- | Defines how to serialize `BikePointProperty` records into database rows.
instance ToRow BikePointProperty where
    toRow :: BikePointProperty -> [SQLData]
    toRow (BikePointProperty category key sourceSystemKey value modified)
        = toRow (category, key, sourceSystemKey, value, modified)
instance FromRow BikePointOccupancy where
  fromRow = BikePointOccupancy
    <$> field <*> field<*> field<*> field<*> field  <*> field<*> field

-- | Initializes the SQLite database connection.
-- | Logs an error and terminates the program if the connection fails.
initialiseDB :: IO Connection
initialiseDB = do
    putStrLn "Initializing database..."
    open "transport_in_london.sqlite"
      `catch` \e -> do
          putStrLn $ "Failed to open database: " ++ show (e :: SomeException)
          error "Database initialization failed."

-- | CREATE TABLES 
-- | Creates the required database tables if they do not exist.
createTables :: Connection -> IO ()
createTables conn = do
    execute_ conn
        "CREATE TABLE IF NOT EXISTS BikePoints (\
        \ bp_id VARCHAR(50) PRIMARY KEY, \
        \ url VARCHAR(255) NOT NULL, \
        \ lat FLOAT NOT NULL, \
        \ lon FLOAT NOT NULL, \
        \ common_name VARCHAR(255) NOT NULL \
        \ )"
    putStrLn "Table BikePoint created successfully..."
    execute_ conn
        "CREATE TABLE IF NOT EXISTS BikePointProperties (\
        \ bike_point_id VARCHAR(50) NOT NULL, \
        \ category VARCHAR(50) NOT NULL, \
        \ key VARCHAR(50) NOT NULL, \
        \ source_system_key VARCHAR(50) NOT NULL, \
        \ value VARCHAR(255) NOT NULL, \
        \ modified DATETIME NOT NULL, \
        \ FOREIGN KEY (bike_point_id) REFERENCES BikePoints(bp_id) \
        \ )"
    putStrLn "Table BikePointProperties created successfully..."

    execute_ conn
       "CREATE TABLE IF NOT EXISTS BikePointOccupancy (\
        \ bpo_id VARCHAR(50) NOT NULL, \
        \ name VARCHAR(50) NOT NULL, \
        \ bikesCount INT NOT NULL, \
        \ emptyDocks INT, \
        \ totalDocks INT, \
        \ standardBikesCount INT, \
        \ eBikesCount INT, \
        \ FOREIGN KEY (bpo_id) REFERENCES BikePoints(bp_id) \
        \ )"
    putStrLn "Table BikePointOccupancy created successfully..."

--INSERTIONS
--- | Inserts a bike point along with its properties into the database.
insertBikeProperty :: Connection -> String -> BikePointProperty -> IO ()
insertBikeProperty conn parentId record = do
    let action = execute conn
            "INSERT INTO BikePointProperties (bike_point_id, category, key, source_system_key, value, modified) VALUES (?, ?, ?, ?, ?, ?)"
            (parentId, category record, key record, sourceSystemKey record, value record, modified record)
    action `catch` \(e :: SomeException) -> 
        putStrLn $ "Failed to insert BikePointProperty: " ++ show e

-- | Inserts a bike point along with its properties into the database.
insertBikePointWithProperties :: Connection -> BikePoint -> IO ()
insertBikePointWithProperties conn bikePoint = do
    let action = execute conn
            "INSERT INTO BikePoints (bp_id, url, lat, lon, common_name) VALUES (?,?,?,?,?)"
            (bp_id bikePoint, url bikePoint, lat bikePoint, lon bikePoint, commonName bikePoint)
    action `catch` (\(e :: SomeException) -> 
        putStrLn $ "Failed inserting  BikePoint: " ++ show e)

    putStrLn $ "Ready to insert BikePoint properties " 
    mapM_ (\prop -> insertBikeProperty conn (bp_id bikePoint) prop) (additionalProperties bikePoint)

-- | Inserts multiple bike points along with their properties into the database.
insertAllBikePoints :: Connection -> [BikePoint] -> IO ()
insertAllBikePoints conn bikePoints = mapM_ (insertBikePointWithProperties conn) bikePoints

-- | Inserts multiple bike occupancies along with their properties into the database.
insertBikeOccupancy :: Connection -> BikePointOccupancy -> IO ()
insertBikeOccupancy conn record = do
    execute conn
       "INSERT INTO BikePointOccupancy (bpo_id, name, bikesCount, emptyDocks, totalDocks, standardBikesCount, eBikesCount) VALUES (?, ?, ?, ?, ?, ?, ?)"
        ( bpo_id record
        , name record
        , bikesCount record
        , emptyDocks record
        , totalDocks record
        , standardBikesCount record
        , eBikesCount record
        )
    putStrLn "BikePointOccupancy record inserted successfully."

-- | Saves BikePointOccupancy or throw errors.
saveInsertBikeOccupancy :: Connection -> BikePointOccupancy -> IO ()
saveInsertBikeOccupancy conn record = do
    result <- try (insertBikeOccupancy conn record) :: IO (Either SomeException ())
    case result of
        Left ex -> putStrLn $ "Error in inserting BikePointOccupancy: " ++ show ex
        Right _ -> return ()

--QUERYS
-- | Retrieves all bike points and their associated properties from the database.
queryBikePoints :: Connection -> IO [AllBikePointDetails]
queryBikePoints conn = do
    bikePoints <- query_ conn "SELECT bp_id, url, lat, lon, common_name FROM BikePoints"
    bikePoints <- mapM (getAllBikePointsData conn) bikePoints
    return bikePoints

-- | Retrieves a bike point's properties and constructs a complete `AllBikePointDetails` record(Used in dump logic).
getAllBikePointsData :: Connection -> (String, String, Double, Double, String) -> IO AllBikePointDetails
getAllBikePointsData conn (bpId, url, lat, lon, name) = do
    properties <- query conn
        "SELECT category, key, source_system_key, value, modified FROM BikePointProperties WHERE bike_point_id = ?"
        [bpId]

    occupancy <- query conn
        "SELECT * FROM BikePointOccupancy WHERE bpo_id = ?"
        [bpId]
    let additionalProps = map toProperty properties
    let occupancyData = case occupancy of
            [] -> Nothing  -- No occupancy data found
            (x:_) -> Just x  -- Take the first occupancy record    -- let occupancyProps = map toBikePointOccupancyProperty occupancy
    return $ AllBikePointDetails
        { bpnt_id = bpId
        , bp_url = url
        , bp_lat = lat
        , bp_lon = lon
        , bp_commonName = name
        , properties = additionalProps,
         occupancy = occupancyData
        }

-- | Get all records in BikePoints.
queryAllBpEntries :: Connection -> IO [BikePointPartial]
queryAllBpEntries conn = do
    putStrLn "Looking for Bike Point records..."
    let sql = "SELECT lat, lon,common_name  FROM BikePoints"
    query_ conn sql

-- | Query details for a given station.
queryBikePointKeyValueDetails :: Connection -> String -> IO [(String, String)]
queryBikePointKeyValueDetails conn stationName = do
    putStrLn $ "Looking for " ++ stationName ++ " entries..."
    let sql = "SELECT key,value FROM BikePoints inner join BikePointProperties on BikePoints.bp_id == BikePointProperties.bike_point_id WHERE common_name like ?"
    query conn sql ["%"++ stationName ++"%"]

-- | Converts a tuple of property data into a `BikePointProperty` record.
toProperty :: (String, String, String, String, UTCTime) -> BikePointProperty
toProperty (category, key, sourceSystemKey, value, modified) =
    BikePointProperty
        { category = Just category
        , key = Just key
        , sourceSystemKey = Just sourceSystemKey
        , value = Just value
        , modified = Just modified
        }

-- Query number of bikes and empty docs from occupancy and bank properties for comparison
queryBikePointOccupancyComparison :: Connection -> Int -> IO [(Int, Int, String)]
queryBikePointOccupancyComparison conn bikesCountValue = do
    let sql = "SELECT \
              \ o.emptyDocks, o.bikesCount, p.value \
              \ FROM BikePointOccupancy AS o \
              \ INNER JOIN BikePointProperties AS p \
              \ ON o.bpo_id = p.bike_point_id \
              \ WHERE o.bikesCount >= ? \
              \   AND (p.key = 'NbEmptyDocks' OR p.key = 'NbDocks')"
    query conn sql [bikesCountValue]

-- | Queries the availability of bikes for a given bike point name.
queryBikeAvailability :: Connection -> String -> IO (Maybe Int)
queryBikeAvailability conn commonName = do
    let sql =
            "SELECT bpp.value \
            \FROM BikePoints bp \
            \INNER JOIN BikePointProperties bpp ON bp.bp_id = bpp.bike_point_id \
            \WHERE bp.common_name = ? \
            \AND bpp.key = 'NbBikes'"
    results <- query conn sql [commonName] :: IO [Only String]

    -- Convert the String result to an Int
    return $ case results of
        (Only value:_) -> case reads value of
            [(n, "")] -> Just n
            _ -> Nothing
        _ -> Nothing



-- New code of radius
-- | Calculates the distance between two points using the Haversine formula
calculateDistance :: Double -> Double -> Double -> Double -> Double
calculateDistance lat1 lon1 lat2 lon2 = do
    let r = 6371.0 -- Earth's radius in kilometers
        dlat = toRadians (lat2 - lat1)
        dlon = toRadians (lon2 - lon1)
        a = sin (dlat/2) * sin (dlat/2) +
            cos (toRadians lat1) * cos (toRadians lat2) *
            sin (dlon/2) * sin (dlon/2)
        c = 2 * atan2 (sqrt a) (sqrt (1-a))
    r * c
  where
    toRadians deg = deg * pi / 180.0

-- | Finds bike points within a specified radius of given coordinates
findNearbyBikePoints :: Connection -> Double -> Double -> Double -> IO [(String, String, Double)]
findNearbyBikePoints conn targetLat targetLon radius = do
    -- Get all bike points
    results <- query_ conn "SELECT bp_id, common_name, lat, lon FROM BikePoints" :: IO [(String, String, Double, Double)]
    
    -- Filter and calculate distances
    let nearbyPoints = filter (\(_, _, lat, lon) -> 
            calculateDistance targetLat targetLon lat lon <= radius)
            results
    
    -- Return results with distance
    return [(id, name, calculateDistance targetLat targetLon lat lon)
            | (id, name, lat, lon) <- nearbyPoints]





