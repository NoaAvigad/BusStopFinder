{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module GeoLocation where

import Network.HTTP
import Data.Aeson
import GHC.Generics
import System.IO
import Data.ByteString.Lazy.Char8

data LatLon =
    LatLon {
        lat :: Float,
        lon :: Float
    } deriving (Show)

instance FromJSON LatLon where
    parseJSON (Object v) = LatLon 
            <$> v .: "latitude"
            <*> v .: "longitude"

-- | Gets the users current location based on their IP
getGeoLocation :: (IO (Maybe LatLon))
getGeoLocation = do
    responseJSON <- (simpleHTTP (getRequest "http://freegeoip.net/json/")) >>= getResponseBody 
    let latlon = decode (pack responseJSON) :: Maybe LatLon 
    return latlon




