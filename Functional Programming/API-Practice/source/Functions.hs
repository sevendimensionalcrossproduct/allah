-- Functions.hs
{-# LANGUAGE OverloadedStrings #-}

module Functions where

import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import Data.Text.Lazy (Text)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Aeson (ToJSON, FromJSON, parseJSON, encode, eitherDecode, withObject, (.:), toJSON, object, (.=), (.:))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void)
import Web.Scotty.Trans (ActionT)


data User = User { userId :: Integer, userName :: Text }

instance ToJSON User where
  toJSON user = object ["userId" .= userId user, "userName" .= userName user]

instance FromJSON User where
  parseJSON = withObject "User" $ \v ->
    User <$> v .: "userId" <*> v .: "userName"

publicDirectoryPath :: IO FilePath
publicDirectoryPath = getCurrentDirectory >>= \currentDir -> return (currentDir </> "public")

-- Read JSON file
readJson :: FilePath -> ActionT IO [User]
readJson jsonPath = liftIO (
  either
    (\err -> putStrLn ("Error decoding JSON file: " ++ err) >> return [])
    return . eitherDecode =<< BS.readFile jsonPath)


-- Write users to the JSON file
writeJson :: FilePath -> [User] -> ActionT IO ()
writeJson jsonPath newData = void(liftIO(BS.writeFile jsonPath (encode newData)))

-- Update IDs
updateIds :: Integer -> [User] -> [User]
updateIds _ [] = []
updateIds startId (user:users) = user { userId = startId } : updateIds (startId + 1) users

-- Wipe JSON file
wipeJson :: FilePath -> ActionT IO ()
wipeJson jsonPath = void (liftIO(BS.writeFile jsonPath "[]"))
