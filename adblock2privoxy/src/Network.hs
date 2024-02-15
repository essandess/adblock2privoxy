module Network (
    downloadHttp
)
where

import Network.HTTP.Conduit
import Data.Text.Lazy.Encoding
import Data.Text.Lazy
import Control.Exception

-- | A simpleHttp alternative that specifies bigger timeout and retries connection attempts
downloadHttp ::  Manager -> Int -> String -> IO String
downloadHttp manager retries url = do
                putStrLn $ "load " ++ url ++ " (" ++ show retries ++ " more attempts)..."
                req <- parseUrlThrow url  -- parseUrl
                let req' = req
                -- let req' = req {responseTimeoutMicro = Just 15000000}
                result <- try (responseBody <$> httpLbs req' manager)
                case result of
                    Left e@(HttpExceptionRequest _ (ConnectionFailure _)) ->
                    -- Left e@(FailedConnectionException _ _) ->
                      if retries > 0 then downloadHttp manager (retries - 1) url else throw e
                    Left e -> throw e
                    Right content -> return $ unpack.decodeUtf8 $ content
