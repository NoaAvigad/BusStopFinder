{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module TranslinkAPI where

import Data.Maybe
import Data.Aeson
import Data.Aeson.Types
import Data.Text hiding (foldr)
import Network.HTTP
import Network.HTTP.Base
import Network.HTTP.Headers
import Network.Stream
import Network.URI
import GHC.Generics
import qualified Data.ByteString.Lazy as B

apiKey = "qwRjMjGRk9PMqJklKOPK" 


data LatLon = LatLon Float Float deriving (Show)

type Radius = Float

type Accessible = Bool 
type Name = [Char]
type Distance = Float
type Route = [Char]
data BusStop = BusStop Name LatLon Distance [Route] Accessible deriving (Show) 

data Stop = 
    Stop { stopNumber :: Int
        , name :: Text
        , bayNumber :: Text
        , city :: Text
        , onStreet :: Text
        , atStreet :: Text
        , latitude :: Float
        , longitude :: Float
        , wheelChairAccess :: Int
        , distance :: Float
        , routes :: Text
    } deriving (Show, Generic)

instance FromJSON Stop where
  parseJSON (Object v) = 
    Stop <$> v .: "StopNo" 
         <*> v .: "Name" 
         <*> v .: "BayNo"
         <*> v .: "City"
         <*> v .: "OnStreet"
         <*> v .: "AtStreet"
         <*> v .: "Latitude"
         <*> v .: "Longitude"
         <*> v .: "WheelchairAccess"
         <*> v .: "Distance"
         <*> v .: "Routes"

instance ToJSON Stop

getBusStopJSON :: LatLon -> Radius -> IO B.ByteString
getBusStopJSON (LatLon lat lon) radius = do
    -- TODO: What if this fails?
    let uri = fromJust (parseURI ("http://api.translink.ca/rttiapi/v1/stops?" ++
                                "apikey=" ++ apiKey ++ 
                                "&lat=" ++ show lat ++ 
                                "&long=" ++ show lon))
    let jsonHeader = mkHeader HdrContentType "application/json"
    let request = Request {
        rqURI=uri,
        rqMethod=GET,
        rqHeaders=[jsonHeader],
        rqBody=""
    } 
    simpleHTTP request >>= getResponseBody

-- Makes a JSON request using the user location and radius and stores the result of the request into a JSON file called "stops.json"
storeBusStopList :: IO()
storeBusStopList = do
    jsonString <- getBusStopJSON userLocation 2000
    B.writeFile "stops.json" jsonString

-- Converts between a Maybe [Stop] to a [Stop]
checkBusStops :: Maybe [Stop] -> [Stop]
checkBusStops busStops 
    | isJust busStops = fromJust busStops
    | otherwise = []

queryBusStop :: (Eq t) => Stop -> (Stop -> t) -> t -> Bool
queryBusStop stop f paramVal
    | f stop == paramVal = True
    | otherwise = False

-- Gets all of the stops with the given stop number and prints the list of stops to the screen
queryBusStopByStopNumber :: p -> Int -> IO()
queryBusStopByStopNumber paramName paramVal = do
    byteString <- B.readFile "stops.json"
    let busStops = decode byteString :: Maybe [Stop]
    let busStoplist = checkBusStops busStops
    let val = foldr (\x acc -> if (stopNumber x == paramVal) then x : acc else acc) [] busStoplist
    putStrLn (show val)

-- temporary solution until we implement fetching of current location
userLocation = LatLon 49.279171 (-122.919808)

-- Returns a Maybe LatLon
-- Is Just LatLon if a valid specification specified
-- Nothing if an invalid location specified
getLatLon :: [Char] -> Maybe LatLon
getLatLon poi
    | poi == "ICICS" = Just (LatLon 49.260986 (-123.248064))
    | poi == "city hall" = Just (LatLon 49.263248 (-123.114934))
    | poi == "nest" = Just (LatLon 49.266502 (-123.249666))
    | poi == "BCIT" = Just (LatLon 49.249149 (-123.001068))
    | poi == "SFU" = Just (LatLon 49.279171 (-122.919808))
    | poi == "my location" = Just userLocation
    | otherwise = Nothing


-- TODO: We can probably either move this section to main, or just remove it entirely
--main = do
--putStrLn "Please enter a location:"
--pointOfInterest <- getLine
---- get lat lon of the entered poi
--let possibleLatLon = getLatLon pointOfInerest
--if isJust possibleLatLon then
--    putStrLn $ show (fromJust possibleLatLon)
--else
--    putStrLn $ "Invalid location specified"


{- just for reference - ignore for now
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
response <- httpLBS "http://httpbin.org/get"

putStrLn $ "The status code was: " ++
        show (getResponseStatusCode response)
print $ getResponseHeader "Content-Type" response
L8.putStrLn $ getResponseBody response
-}
