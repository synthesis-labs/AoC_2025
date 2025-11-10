module Util.AOCHttpUtils (fetchData) where

import Control.Monad (unless)
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy.Char8 qualified as LC8
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Model (AOCInputData, AOCYearDay)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hCookie)
import Network.HTTP.Types.Status (statusCode)
import System.Directory qualified as Dir
import System.Environment (getEnv)

fetchData :: AOCYearDay -> IO AOCInputData
fetchData (year, day) = do
  let dataDir = "data/"
      yearDir = dataDir <> show year <> "/"
      paddedDay = if day < 10 then "0" <> show day else show day
      local_file = paddedDay <> ".txt"
  _ <- do
    exists <- Dir.doesDirectoryExist dataDir
    unless exists $ Dir.createDirectory dataDir
  _ <- do
    exists <- Dir.doesDirectoryExist yearDir
    unless exists $ Dir.createDirectory yearDir
  _ <- do
    exists <- Dir.doesFileExist (yearDir <> local_file)
    unless exists $ downloadFile (year, day) (yearDir <> local_file)
  byteStrData <- B.readFile (yearDir <> local_file)
  pure $ (T.strip . TE.decodeUtf8) byteStrData

downloadFile :: AOCYearDay -> String -> IO ()
downloadFile (year, day) filename = do
  let url = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"
  cookie <- getEnv "COOKIE"
  req <- HTTP.parseRequest url
  let req0 = req {HTTP.requestHeaders = [(hCookie, C8.pack cookie)]}
  manager <- HTTP.newManager tlsManagerSettings
  resp <- HTTP.httpLbs req0 manager
  let body :: String = LC8.unpack $ HTTP.responseBody resp
  case statusCode (HTTP.responseStatus resp) of
    200 -> writeFile filename body
    _ -> error $ "Failed to download input for year " ++ show year ++ " day " ++ show day ++ ": " ++ body
