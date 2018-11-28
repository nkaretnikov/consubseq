{-# language GeneralizedNewtypeDeriving #-}
{-# language RecordWildCards #-}

module ConSubSeq where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.List
import Data.Ord
import Prelude hiding (Left, Right)
import System.Exit
import System.IO
import Text.PrettyPrint.ANSI.Leijen (displayIO, renderPretty)
import Text.Trifecta

-- XXX: Use 'Word' instead of 'Integer'?  Will need to truncate myself and
-- handle overflow.
newtype Section = Section
  { _fromSection :: String
  } deriving (Eq, Ord, Show, Hashable)

newtype Row = Row
  { _fromRow :: Integer
  } deriving (Eq, Ord, Show, Hashable)

newtype Left = Left
  { _fromLeft :: Integer
  } deriving (Eq, Ord, Show)

newtype Right = Right
  { _fromRight :: Integer
  } deriving (Eq, Ord, Show)

data Seat = Seat
  { _section :: Section
  , _row     :: Row
  , _left    :: Left
  , _right   :: Right
  } deriving (Eq, Ord, Show)

parseSeat :: Parser Seat
parseSeat = do
  section <- parseSection <* parseComma
  row     <- parseRow     <* parseComma
  left    <- parseLeft    <* parseComma
  right   <- parseRight
  if (_fromLeft left) > (_fromRight right)
  then fail $ "invalid range: ("
           ++ show (_fromLeft left)
           ++ ","
           ++ show (_fromRight right)
           ++ ")"
  else return $ Seat
    { _section = section
    , _row     = row
    , _left    = left
    , _right   = right
    }

parseComma :: Parser Char
parseComma = char ','

parseSection :: Parser Section
parseSection = Section <$> (some $ satisfy isUpper)  -- XXX: should it be ASCII only?

parseRow :: Parser Row
parseRow = Row <$> decimal

parseLeft :: Parser Left
parseLeft = Left <$> decimal

parseRight :: Parser Right
parseRight = Right <$> decimal

hPrintSeat :: Handle -> (Section, Row) -> (Left, Right) -> IO ()
hPrintSeat h (sec,row) (l,r) =
  hPutStrLn h $ intercalate ","
    [       _fromSection sec
    , show (_fromRow     row)
    , show (_fromLeft    l)
    , show (_fromRight   r)
    ]

-- Preconditions for input:
-- * the list is sorted
-- * ranges are non-overlapping (and the list doesn't contain duplicates)
-- * ranges are inclusive
-- * left <= right
printRanges :: Handle -> (Section, Row) -> [(Left, Right)] -> IO ()
printRanges = go 0 Nothing
  where
    go :: Integer -> Maybe (Left, Right)
       -> Handle -> (Section, Row) -> [(Left, Right)] -> IO ()
    -- No more input.  Print the group bounds if available and there's more than
    -- one element in the group.
    go cnt p                hOut sr []         =
      when (cnt > 1) $ mapM_ (hPrintSeat hOut sr) p

    -- The first element in the group.  Print the element, start a new group,
    -- and continue.
    go _   Nothing          hOut sr ((l,r):xs) = do
      hPrintSeat hOut sr (l,r)
      go 1 (Just (l,r)) hOut sr xs

    go cnt p@(Just (l',r')) hOut sr ((l,r):xs) =
      if (_fromRight r') + 1 == _fromLeft l
      then do
        -- The element is part of the same group:
        -- * print the element
        -- * extend the existing group and continue.
        hPrintSeat hOut sr (l,r)
        go (cnt + 1) (Just (l',r)) hOut sr xs
      else do
        -- The element starts a new group:
        -- * print the old group bounds if there's more than one element in it
        -- * print the element
        -- * continue with the new group.
        when (cnt > 1) $ mapM_ (hPrintSeat hOut sr) p
        hPrintSeat hOut sr (l,r)
        go 1 (Just (l,r)) hOut sr xs

type SeatMap = HashMap (Section, Row) [(Left, Right)]

parse :: Handle -> String -> IO Seat
parse hErr s = case parseString parseSeat mempty s of
  Success res -> return res
  Failure e   -> do
    displayIO hErr $ renderPretty 1 80 $ _errDoc e
    exitFailure

seatInsert :: Seat -> SeatMap -> SeatMap
seatInsert Seat {..} m = M.insertWith (++) (_section, _row) [(_left, _right)] m

run :: Handle -> Handle -> Handle -> IO ()
run hIn hOut hErr = do
  ls <- (filter (/= "") . lines) <$> hGetContents hIn
  seatMap <-
    foldlM (\m l -> parse hErr l >>= \s -> return $ seatInsert s m)
    M.empty ls
  forM_ (sortBy (comparing fst) $ M.toList seatMap) $ \(k,v) -> do
    printRanges hOut k $ sort v
