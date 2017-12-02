module Main where

import TranslinkAPI
import GeoLocation
import Logo

import System.IO
import Text.Read
import Data.Maybe
import Data.Typeable

locationListAsString :: [Location] -> Int -> String
locationListAsString [] _ = ""
locationListAsString ((Location name _):xs) index = "(" ++ (show index) ++ ") " ++ name
                                            ++ "\n" ++ locationListAsString xs (index+1)
-- Gets the current location from the user
getCurrentLatLonFromUser :: IO (Maybe LatLon)
getCurrentLatLonFromUser = do
    putStrLn "Where are you? (choose a number)"
    -- Print out some locations for the user to choose from
    putStrLn "(0) I don't know, find out for me!"
    putStr (locationListAsString knownLocations 1)
    locationIndexStr <- getLine
    let locationIndex = fromJust (readMaybe locationIndexStr :: Maybe Int)
    let currLocation = knownLocations !! (locationIndex-1)
    -- Check a valid int was given
    if isNothing (readMaybe locationIndexStr :: Maybe Int) then
        -- Invalid number given 
        return Nothing
    else if (locationIndex == 0) then
        -- User doesn't know where they are, try to find out for them
        getGeoLocation
    -- Lookup the location if the number is in our list
    else if locationIndex < (length knownLocations +1) then
        return (Just (locationLatLon currLocation))
    else
        -- Not a valid location choice
        return Nothing 

lookupBusStops :: IO ()
lookupBusStops = do 
    putStrLn "Where are you?"
    maybeCurrLatLon <- getCurrentLatLonFromUser
    let currLatLon = fromJust maybeCurrLatLon
    if isNothing maybeCurrLatLon then
        putStrLn "Sorry, invalid choice!"
    else do
        putStrLn "What radius do you want to look for bus stops in? (< 2000 due to Translink API restrictions): "
        radiusStr <- getLine
        let radius = fromJust (readMaybe radiusStr :: Maybe Float)
        if isJust (readMaybe radiusStr :: Maybe Float) && radius < 2000 then (do
            storeBusStopList currLatLon radius
            putStrLn "Saved requested bus stops, run me again to query on this search!"
            main)
        else (do
            putStrLn "Invalid radius!"
            main)

queryBusStops :: IO ()
queryBusStops = do
    -- TODO:
    -- Check if "stops.json" exists
    -- if it exists, the give the user some options to query on it
    -- otherwise, print an error
    putStrLn "This operation isn't supported yet!"
    main

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn translinkLogo
    putStrLn("What would you like to do?:")
    putStrLn("(1) Search for bus stops")
    putStrLn("(2) Query on last search")
    choice <- getLine
    if choice == "1" then
        lookupBusStops
    else if choice == "2" then
        queryBusStops
    else 
        putStrLn "Invalid Choice!"
