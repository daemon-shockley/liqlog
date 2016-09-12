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
import Control.Monad

import Database.Esqueleto ()
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Data.String
import Data.Time
import Data.Text.Lazy ()

import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Beams
    title String
    createdAt UTCTime
    deriving Show
Star
    spent Bool
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

    S.get "/stars/all" $ do
      spent <- liftIO getSpentStars
      unspent <- liftIO getUnspentStars
      html $ mconcat ["<p>You have earned a total of ",
                      fromString $ show (length spent + length unspent),
                      " and have ",
                      fromString $ show (length unspent),
                      " to spend!</p>",
                      "<p>",
                      mconcat $ map linkToEarnNStars [1..6],
                      mconcat $ map linkToSpendNStars [1..6]
                      ]

    S.get "/stars/award/:numberEarned" $ do
      numberEarned <- param "numberEarned"
      _ <- liftIO $ earnStars numberEarned
      redirect "/stars/all"

    S.get "/stars/spend/:numberSpent" $ do
      numberSpent <- param "numberSpent"
      liftIO $ spendStars numberSpent
      redirect "/stars/all"

linkToEarnNStars :: (Data.String.IsString a, Monoid a) => Int -> a
linkToEarnNStars n = mconcat $ ["<p><a href='/stars/award/", fromString (show n), "'>Award ", fromString (show n), " Star</a></p>"]

linkToSpendNStars :: (Data.String.IsString a, Monoid a) => Int -> a
linkToSpendNStars n = mconcat $ ["<p><a href='/stars/spend/", fromString (show n), "'>Spend ", fromString (show n), " Star</a></p>"]

getSpentStars :: IO [Entity Star]
getSpentStars = runDb $ selectList [StarSpent ==. True] []

getUnspentStars :: IO [Entity Star]
getUnspentStars = runDb $ selectList [StarSpent !=. True] []

earnStars :: Int -> IO [Key Star]
earnStars n = replicateM n createStar
--pointful or pointfree?
-- earnStars = (flip replicateM) createStar

spendStars :: Int -> IO ()
spendStars n = do
  unspent <- getNUnspentStars n
  mapM_ spendStar unspent

getNUnspentStars :: Int -> IO [Entity Star]
getNUnspentStars n = runDb $ selectList [StarSpent !=. True] [LimitTo n]

spendStar :: Entity Star -> IO ()
spendStar star = runDb $ update starId [StarSpent =. True]
  where starId = entityKey star

createStar :: IO (Key Star)
createStar = do
  now <- getCurrentTime
  runDb $ insert $ Star False now


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

