module Main where

import TranslinkAPI
import GeoLocation

getCurrentLocationFromUser :: IO (Maybe LatLon)
getCurrentLocationFromUser = do
    putStrLn "Where are you?"
    -- Print out some locations for the user to choose from
    putStr "(1) I don't know, find out for me!"
    putStr (locationListAsString knownLocations 1)
    locationIndexStr <- getLine
    if not (isJust (readMaybe locationIndexString :: Maybe Int)) then
        putStrLn "Invalid number!"
        return Nothing
    let locationIndex = read locationIndexStr :: Int
    if locationIndex == 1 then
        getGeoLocation
    else
        if (locationIndex > (length knownLocations)) then
            return knownLocations !! locationIndex
        else
            putStrLn "Invalid location specified!"
            return Nothing

main :: IO ()
main = do
    putStrLn "Where are you?"
    putStrLn (show knownLocations)
    location <- getLine
    putStrLn "Nope, that ain't gonna fly!"
