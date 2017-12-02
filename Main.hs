module Main where

import TranslinkAPI
import GeoLocation
import Logo

import System.IO
import Text.Read
import Data.Maybe
import Data.Typeable
import Data.Text hiding (splitOn, foldr, length)
import Data.List.Split

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
    if isNothing maybeCurrLatLon then (do
        putStrLn "Sorry, invalid choice!"
        mainMenu)
    else do
        putStrLn "What radius do you want to look for bus stops in? (< 2000 due to Translink API restrictions): "
        radiusStr <- getLine
        let radius = fromJust (readMaybe radiusStr :: Maybe Float)
        if isJust (readMaybe radiusStr :: Maybe Float) && radius < 2000 then (do
            storeBusStopList currLatLon radius
            putStrLn "Saved requested bus stops!"
            mainMenu)
        else (do
            putStrLn "Invalid radius!"
            mainMenu)

-- Generic function to perform some IO action with the list of bus stops once it is retrieved and error checked
performBusStopAction :: ([BusStop] -> IO ()) -> IO ()
performBusStopAction ioAction = do
    busStopList <- getBusStopsFromFile
    if length busStopList == 0 then do
        putStrLn "You have not loaded any bus stops! Please run a search before querying."
        mainMenu
    else do
        ioAction busStopList
        mainMenu

-- Given a list of bus stops prints out the bus stops
listBusStops :: [BusStop] -> IO ()
listBusStops busStopList = do
    putStrLn (show busStopList)

-- Given a list of bus stops performs a query on them
queryBusStops :: [BusStop] -> IO ()
queryBusStops busStopList = do
    putStrLn "What kind of query would you like to perform?"
    putStrLn "(1) By stop number"
    putStrLn "(2) By route"
    choice <- getLine
    if choice == "1" then
        queryBusStopByStopNumber busStopList
    else if choice == "2" then
        queryBusStopByRoute busStopList
    else 
        putStrLn "Invalid Choice!"

-- Queries the list of bus stops by the specific bus stop number
queryBusStopByStopNumber :: [BusStop] -> IO ()
queryBusStopByStopNumber busStopList = do
    putStrLn "Please enter a stop number to query:"
    stopNumberStr <- getLine
    let busStopNumber = fromJust (readMaybe stopNumberStr :: Maybe Int)
    if isJust (readMaybe stopNumberStr :: Maybe Int) then do
        let queriedStops = foldr (\x acc -> if (stopNumber x == busStopNumber) then x : acc else acc) [] busStopList
        putStrLn (show queriedStops)
    else do
        putStrLn "Invalid bus stop number!"

-- Queries the list of bus stops by the route 
queryBusStopByRoute :: [BusStop] -> IO ()
queryBusStopByRoute busStopList = do
    putStrLn "Please enter a route to query:"
    routeName <- getLine
    let queriedStops = foldr (\x acc -> if (existsInList routeName (splitOn ", " (unpack (routes x)))) then x : acc else acc) [] busStopList
    putStrLn (show queriedStops) 

-- Checks if an item exists within a list
existsInList :: Eq a => a -> [a] -> Bool
existsInList _ [] = False
existsInList item (h:t) 
    | h == item = True
    | otherwise = existsInList item t

mainMenu :: IO ()
mainMenu = do
    putStrLn("")
    putStrLn("What would you like to do?:")
    putStrLn("(1) Search for bus stops")
    putStrLn("(2) Query on last search")
    putStrLn("(3) List all stops from last search")
    putStrLn("(4) Exit")
    choice <- getLine
    if choice == "1" then
        lookupBusStops
    else if choice == "2" then
        performBusStopAction queryBusStops
    else if choice == "3" then 
        performBusStopAction listBusStops
    else if choice == "4" then 
        return ()
    else 
        putStrLn "Invalid Choice!"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn translinkLogo
    mainMenu
