{-|
Module      : Main
Description : Bike Point Management Application Main

This module serves as the entry point for the Bike Point Management application.
It provides various commands for interacting with bike point data, such as
creating a database, loading data from an API, exporting it to JSON, and
generating an HTML map.

Key Features:
  - Initialize and manage the database schema for bike points.
  - Load bike point data from a public API.
  - Export bike point data to a JSON file.
  - Generate an interactive HTML map using Leaflet.js.

Example usage:

@
-- Initialize the database and create necessary tables
stack run -- create

-- Load data from the API and save to the database
stack run -- loaddata

-- Export data to a JSON file
stack run -- dumpdata

-- Generate an interactive HTML map
stack run -- map
@

Commands:
  - `create`     - Initialize the database and create tables.
  - `loaddata`   - Download bike point data from the API and save it to the database.
  - `dumpdata`   - Export bike point data to a JSON file.
  - `<query> <query_arguments>` - Perform the queries .
-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main (main) where

import System.Environment
import System.IO
import qualified Data.ByteString.Lazy.Char8 as L8
import Network.HTTP.Types (status200)
import System.Process (callCommand)
import Network.Wai (responseFile, Application)
import Network.Wai.Handler.Warp (run)
import Data.Aeson (encode)
import Control.Exception (catch, SomeException)
import qualified Data.ByteString.Lazy as B
import Data.List (intercalate)
import Text.Read (readMaybe)
import Control.Monad (forM_)
import Types
import Lib
import Fetch ( download )
import Parse ( parseBikePointProperties, dumpToJsonFile,parseBikePointOccupancies)
import Database
    ( createTables,
      initialiseDB,
      insertAllBikePoints,
      queryBikePoints, queryAllBpEntries,queryBikePointKeyValueDetails,saveInsertBikeOccupancy,queryBikePointOccupancyComparison,findNearbyBikePoints,queryBikeAvailability)
import Text.Printf (printf)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["create"] -> handleErrors $ do
            conn <- initialiseDB
            createTables conn
            hSetBuffering stdout NoBuffering
        ["loaddata"] -> handleErrors $ do
            conn <- initialiseDB
            print "Downloading..."
            json <- download "https://api.tfl.gov.uk/bikepoint"
            print "Parsing..."
            case (parseBikePointProperties json) of
                Left err -> print err
                Right recs -> do
                    print "Saving on DB..."
                    let bikePointsApi = take 25 recs
                    insertAllBikePoints conn bikePointsApi
                    let occupancies = take 19 bikePointsApi
                    let ids = map bp_id occupancies
                    let connectedIds = intercalate "," ids
                    bpoJson <- download $ "https://api.tfl.gov.uk/Occupancy/BikePoints/ " ++ connectedIds
                    print "Parsing Bike Point Occupancies..."
                    case (parseBikePointOccupancies bpoJson) of
                      Left bpoError -> print bpoError
                      Right bpoRes -> do
                        mapM_ (saveInsertBikeOccupancy conn) bpoRes
                        print "Saving Bike Point Occupancies..."
        ["dumpdata"] -> handleErrors $ do
             let filePath = "all_data.json"
             conn <- initialiseDB
             bikePoints <- queryBikePoints conn
             dumpToJsonFile filePath bikePoints
             putStrLn $ "Data has been written to " ++ filePath
        ["bikePoint", stationName] -> handleErrors $ do
            conn <- initialiseDB
            bikePointDetails <- queryBikePointKeyValueDetails conn stationName
            print bikePointDetails
            putStrLn $ "Details for the station " ++ stationName
        ["checkDataForNoOfBikes", noOfBikes] -> handleErrors $ do
            conn <- initialiseDB
            let bike_num_value = read noOfBikes
            result <- queryBikePointOccupancyComparison conn bike_num_value
            print result
            putStrLn $ "Details for the given num of bikes " ++ show noOfBikes
        ["bikes", stationName] -> handleErrors $ do
            conn <- initialiseDB
            result <- queryBikeAvailability conn stationName
            case result of
                Just count -> putStrLn $ "Number of available bikes at " ++ stationName ++ ": " ++ show count
                Nothing -> putStrLn $ "Could not find bike availability for station: " ++ stationName
        ["nearby", latStr, lonStr, radiusStr] -> handleErrors $ do
            let maybeLat = readMaybe latStr :: Maybe Double
                maybeLon = readMaybe lonStr :: Maybe Double
                maybeRadius = readMaybe radiusStr :: Maybe Double
            case (maybeLat, maybeLon, maybeRadius) of
                (Just lat, Just lon, Just radius) -> do
                    conn <- initialiseDB
                    results <- findNearbyBikePoints conn lat lon radius
                    if null results
                        then putStrLn "No bike points found within the specified radius."
                        else do
                            putStrLn $ "Bike points within " ++ show radius ++ "km of (" ++ 
                                     show lat ++ ", " ++ show lon ++ "):"
                            forM_ results $ \(id, name, dist) -> do
                                putStrLn $ "ID: " ++ id ++ 
                                         "\nName: " ++ name ++ 
                                         "\nDistance: " ++ printf "%.2f" dist ++ "km\n"
                _ -> print "Invalid input. Please provide valid numbers for latitude, longitude, and radius."
        ["map"] -> handleErrors $ do
             conn <- initialiseDB
             bikePoints <- queryAllBpEntries conn
             generateHTML bikePoints "bike_points_map.html"
        _ -> syntaxError


handleErrors :: IO () -> IO ()
handleErrors action = action `catch` handler
  where
    handler :: SomeException -> IO ()
    handler e = putStrLn $ "An error occurred: " ++ show e

syntaxError :: IO ()
syntaxError = putStrLn $
    "Usage: stack run -- [command]\n\n" ++
    "Commands:\n" ++
    "  create     - Setup the database\n" ++
    "  loaddata   - Download bike point data and save to the database\n" ++
    "  dumpdata   - Export bike point data to JSON\n" ++
    "  bikes <station-name> - Get number of available bikes at a station\n" ++
    "  nearby <lat> <lon> <radius> - Find bike points within radius (km) of coordinates\n" ++
    "  bikePoint <stationName> - Find bike points details for given station\n" ++ 
    "  checkDataForNoOfBikes <noOfBikes>  Compare occupancy data with properties\n" ++
    "  map - Generate an HTML map of bike points\n"