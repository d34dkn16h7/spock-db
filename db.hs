{-# LANGUAGE OverloadedStrings #-}

module DB where

--import qualified Database.Persist.Sqlite as SQ
import Data.Bson as B
import Database.MongoDB
import Control.Monad.IO.Class as MM

connectDB = connect (host localhost)
closeDB db = close db

--runDB :: MonadIO m => Pipe -> Action m a -> m a
runDB con act = access con master "leaderboard" act

insertScore :: MM.MonadIO m => Pipe -> Int -> Int -> m Value
insertScore db id score = runDB db $ insert "scoreTable" (lBoard id score)

getLeaderboard db = (runDB db $ find (select [] "scoreTable") {project = ["_id" =: (0 :: Int)]} >>= rest)

readInt :: String -> Int
readInt val = (read val) :: Int

lBoard :: Int -> Int -> [Field]
lBoard id score = ["playerId" =: id, "score" =: score]

printRows :: Show a => [a] -> IO ()
printRows [] = return () 
printRows (x:xs) = do
          print x
          printRows xs

localhost = "127.0.0.1"

m_map [] = return ()
m_map (x:xs) = look "playerId" x >>= print >> m_map xs