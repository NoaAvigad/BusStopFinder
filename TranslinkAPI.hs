-- API Key: qwRjMjGRk9PMqJklKOPK


-- temporary solution until we implement fetching of current location
getUserLocation = [49.279171, -122.919808]

-- getLatLon returns [Lat,Lon] accordingly, if "my location" is passed in, the current location is fetched
-- if location passed in is unfamiliar, return an empty array
getLatLon :: [Char] -> [Double]
getLatLon poi
    | poi == "ICICS" = [49.260986, -123.248064]
    | poi == "city hall" = [49.263248, -123.114934]
    | poi == "nest" = [49.266502, -123.249666]
    | poi == "BCIT" = [49.249149, -123.001068]
    | poi == "SFU" = [49.279171, -122.919808]
    | poi == "my location" = getUserLocation
    | otherwise = []


main = do
putStrLn "Please enter a location:"
pointOfInterest <- getLine
-- get lat lon of the entered poi
let latLon = getLatLon pointOfInterest
putStrLn $ show (head latLon)
putStrLn $ show (latLon!!1)



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
