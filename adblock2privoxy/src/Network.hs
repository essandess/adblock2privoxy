module Network (
    downloadHttp
)
where

import Network.HTTP.Conduit
import Control.Monad (liftM)
import Data.Text.Lazy.Encoding
import Data.Text.Lazy
import Control.Exception

-- | A simpleHttp alternative that specifies bigger timeout and retries connection attempts
downloadHttp ::  Manager -> Int -> String -> IO String
downloadHttp manager retries url = do
                putStrLn $ "load " ++ url ++ " (" ++ show retries ++ " more attempts)..."
                req <- parseUrl url
                let req' = req {responseTimeout = Just 15000000}
                result <- try $ liftM responseBody $ httpLbs req' manager
                case result of
                    Left e@(FailedConnectionException _ _) ->
                      if retries > 0 then downloadHttp manager (retries - 1) url else throw e
                    Left e -> throw e
                    Right content -> return $ unpack.decodeUtf8 $ content
