{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}

import Web.Scotty as S
import Control.Monad.IO.Class
--import Control.Monad (when)

import Database.Esqueleto
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.String
import Data.Time

import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Beams
    title String
    createdAt UTCTime
    deriving Show
|]

runDb :: SqlPersist (ResourceT (NoLoggingT IO)) a -> IO a
runDb query = runNoLoggingT . runResourceT . withSqliteConn "dev.sqlite3" . runSqlConn $ query

main :: IO ()
main = do
  runDb $ runMigration migrateAll
  scotty 3000 $ do
    S.get "/favicon.ico" $ do
      html "<h1>TODO: Favicon</h1>"

    S.get "/all" $ do
      beamList <- liftIO $ getNBeamList 2000
      html $ mconcat ["<h1>Scotty has used ALL these beams:</h1><p>", fromString beamList, "</p>"]

    S.get "/activate/:word" $ do
      beam <- param "word"
      beamList <- liftIO getBeamList
      _ <- liftIO $ saveBeam beam
      html $ mconcat ["<h1>Scotty, ", fromString beam, " me up!</h1>",
                     "<p>last three beams:", fromString beamList, "</p>"]

getBeamList :: IO String
getBeamList = htmlListify <$> getLastThreeBeamTitles

getNBeamList :: Int -> IO String
getNBeamList n = htmlListify <$> map (beamsTitle . entityVal) <$> getLastNBeams n

getLastThreeBeamTitles :: IO [String]
getLastThreeBeamTitles = map (beamsTitle . entityVal) <$> getLastThreeBeams

htmlListify :: (Data.String.IsString a, Monoid a) => [a] -> a
htmlListify ls = mconcat ["<ul>", mconcat $ map toListItem ls, "</ul>"]
  where toListItem i = mconcat ["<li>", i, "</li>"]

getLastNBeams :: Int -> IO [Entity Beams]
getLastNBeams n = (runDb $ selectList [] [LimitTo n, Desc BeamsCreatedAt])

getLastThreeBeams :: IO [Entity Beams]
getLastThreeBeams = getLastNBeams 3

saveBeam :: String -> IO (Key Beams)
saveBeam title = do
  now <- getCurrentTime
  runDb $ insert $ Beams title now

