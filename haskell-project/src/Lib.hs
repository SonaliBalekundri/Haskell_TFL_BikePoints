
module Lib
    ( 
      generateHTML
    ) where
import Types

-- | Removes single quotes from a string.
-- | Example:
-- | @
-- | let result = removeQuotes "London'Transport"
-- | print result -- Output: "LondonTransport"
-- | @
removeQuotes :: String -> String
removeQuotes = filter (/= '\'')

-- | Generates an interactive HTML map displaying bike points.
-- |
-- | Parameters:
-- | - `bikePoints`: A list of `BikePointPartial` records, each containing latitude,
-- |   longitude, and a name.
-- | - `filePath`: The file path where the HTML file will be saved.
-- |
-- | Example:
-- | @
-- | generateHTML bikePoints "bike_points_map.html"
-- | @
generateHTML :: [BikePointPartial] -> FilePath -> IO ()
generateHTML bikePoints filePath = do
    let avgLatitude = sum (map ltd bikePoints) / fromIntegral (length bikePoints)
    let avgLongitude = sum (map lng bikePoints) / fromIntegral (length bikePoints)
    let centerCoordinates = "[" ++ show avgLatitude ++ ", " ++ show avgLongitude ++ "]"

    let markers = unlines $ map (\bp ->
            "L.marker([" ++ show (ltd bp) ++ ", " ++ show (lng bp) ++ "])" ++
            ".addTo(map)" ++
            ".bindPopup('<strong>"++ removeQuotes (cName bp) ++"</strong><br>Lat: " ++ show (ltd bp) ++ ", Lon: " ++ show (lng bp) ++ "');"
            ) bikePoints

    let htmlContent = unlines
            [ "<!DOCTYPE html>"
            , "<html>"
            , "<head>"
            , "    <title>Bike Points Distribution</title>"
            , "    <link rel=\"stylesheet\" href=\"https://unpkg.com/leaflet/dist/leaflet.css\" />"
            , "    <script src=\"https://unpkg.com/leaflet/dist/leaflet.js\"></script>"
            , "</head>"
            , "<body>"
            , "    <div id=\"map\" style=\"height: 100vh;\"></div>"
            , "    <script>"
            , "        // Initialize the Map"
            , "        const map = L.map('map').setView(" ++ centerCoordinates ++ ", 13);"
            , "        L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png').addTo(map);"
            , ""
            , "        // Add Markers In Map"
            , markers
            , "    </script>"
            , "</body>"
            , "</html>"
            ]

    -- Write the HTML content to a file
    writeFile filePath htmlContent
    putStrLn $ "HTML file generated at: " ++ filePath
