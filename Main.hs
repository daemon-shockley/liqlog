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
import Database.Persist.Sqlite as SQ
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
                      "<form method='POST' action='/stars/award'>",
                      para "Number To Award: <input type='text' name='numberEarned'>",
                      para "<input type='submit' name='Award' value='Award'>",
                      "</form>",
                      "<form method='POST' action='/stars/spend'>",
                      para "Number To Spend: <input type='text' name='numberSpent'>",
                      para "<input type='submit' name='Spend' value='Spend'>",
                      "</form>",
                      htmlListify $ map linkToEarnNStars [1..6],
                      htmlListify $ map linkToSpendNStars [1..6]
                      ]

    S.get "/stars/award/:numberEarned" $ earnStarsController
    S.post "/stars/award" $ earnStarsController

    S.get "/stars/spend/:numberSpent" $ spendStarsController
    S.post "/stars/spend" $ spendStarsController

    S.get "/stars/destroy/:numberDestroyed" $ do
      numberDestroyed <- param "numberDestroyed"
      liftIO $ deleteStars numberDestroyed
      redirect "/stars/all"

para :: (IsString a, Monoid a) => a -> a
para s = mconcat ["<p>", s, "</p>"]

spendStarsController = do
  numberSpent <- param "numberSpent"
  liftIO $ spendStars numberSpent
  redirect "/stars/all"

earnStarsController = do
  numberEarned <- param "numberEarned"
  _ <- liftIO $ earnStars numberEarned
  redirect "/stars/all"

linkToEarnNStars :: (Data.String.IsString a, Monoid a) => Int -> a
linkToEarnNStars n = mconcat $ ["<a href='/stars/award/", fromString (show n), "'>Award ", fromString (show n), " Star</a>"]

linkToSpendNStars :: (Data.String.IsString a, Monoid a) => Int -> a
linkToSpendNStars n = mconcat $ ["<a href='/stars/spend/", fromString (show n), "'>Spend ", fromString (show n), " Star</a>"]

getSpentStars :: IO [Entity Star]
getSpentStars = runDb $ selectList [StarSpent ==. True] []

getUnspentStars :: IO [Entity Star]
getUnspentStars = runDb $ selectList [StarSpent !=. True] []

getNOldestStars n = runDb $ selectList [] [LimitTo n, Asc StarCreatedAt]

earnStars :: Int -> IO [Key Star]
earnStars n = replicateM n createStar
--pointful or pointfree?
-- earnStars = (flip replicateM) createStar

spendStars :: Int -> IO ()
spendStars n = do
  unspent <- getNUnspentStars n
  mapM_ spendStar unspent

deleteStars :: Int -> IO ()
deleteStars n = do
  old <- getNOldestStars n
  mapM_ deleteStar old

deleteStar :: Entity Star -> IO ()
deleteStar es = runDb $ SQ.delete starId
  where starId = entityKey es

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

