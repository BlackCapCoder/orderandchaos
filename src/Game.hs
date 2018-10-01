{-# LANGUAGE LambdaCase #-}
module Game where

import qualified Data.Matrix as M
import qualified Data.Vector as V
import Data.List
import Data.Foldable
import Control.Monad


data Piece = X | O deriving (Eq, Ord, Enum, Show, Read)
type Board = M.Matrix (Maybe Piece)

board :: Board
board = M.matrix 6 6 (const Nothing)


winnerRow :: V.Vector (Maybe Piece) -> Maybe Piece
winnerRow = (>>= head) . find ((>=5).length) . group . V.toList

winner :: Board -> Maybe Piece
winner m = asum . map winnerRow $ concat
  [ flip M.getRow m <$> [1..6]
  , flip M.getCol m <$> [1..6]
  , pure $ M.getDiag m
  , pure $ V.fromList [ M.getElem x y m | x <- [1..6], y <- [6,5..1] ]
  ]

isFull :: Board -> Bool
isFull = V.all (/=Nothing) . M.getMatrixAsVector

-- get :: Board -> Int -> Int -> Maybe Piece
get b (x,y) = join $ M.safeGet x y b

-- put :: Board -> Int -> Int -> Piece -> Maybe Board
put b l p = do
  guard $ get b l == Nothing
  M.safeSet (pure p) l b


printBoard :: Board -> String
printBoard b = unlines $ map (unwords . map (\case Nothing -> "."; Just x -> show x)) $ M.toLists b

