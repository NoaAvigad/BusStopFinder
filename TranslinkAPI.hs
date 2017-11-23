-- API Key: qwRjMjGRk9PMqJklKOPK



-- stack script --resolver lts-8.22
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple

main :: IO ()
main = do
response <- httpLBS "http://httpbin.org/get"

putStrLn $ "The status code was: " ++
        show (getResponseStatusCode response)
print $ getResponseHeader "Content-Type" response
L8.putStrLn $ getResponseBody response








--Accept'='application/json



-- let opts = defaults & header "Authorisation" .~ ["AUTH_INFO_STRING"]
-- & header "custom-header" .~ ["string"]
-- & param  "search_term"   .~ ["my","search"]
-- getWith opts "http://httpbin.org/someUrl"
