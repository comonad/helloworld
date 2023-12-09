{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language PackageImports #-}
{-# language ViewPatterns #-}



module HiveGame where


import Data.Map.Strict as Map
import Data.List as List
import Data.IORef

data HiveGame = HiveGame
    { hivegame_playertypes :: Map Player PlayerType
    , hivegame_board :: IORef Board
    , hivegame_boardview :: IORef BoardView
    }


data Player = White | Black deriving (Eq,Ord,Enum,Bounded,Show)
data Insect = Ant | Bug | Cricket | Bee | Spider deriving (Eq,Ord,Enum,Bounded,Show)
data Stone = Stone !Player !Insect deriving (Eq,Ord,Show)


-- | (x+y) `mod` 2 == 0
data BoardPos = BoardPos Int Int

data PlayerType = Human | AI deriving (Eq,Ord,Enum,Bounded,Show)
data Board = Board
    { board_playing :: Player
    , board_home :: Map Stone Int
    , board_field :: Map BoardPos [Stone]
    }

data Move = Move Stone (Maybe BoardPos) BoardPos

data BoardView = BoardView (Player,PlayerType,[Move]) (Map BoardPos ([Stone],[Move]))


dragStone :: BoardPos -> HiveGame -> IO ()
dragStone _ _ = return ()
dropStone :: BoardPos -> HiveGame -> IO ()
dropStone _ _ = return ()

initBoardView :: Map Player PlayerType -> Board -> BoardView
initBoardView m Board{..} = BoardView (board_playing,m Map.! board_playing,[])
                                      ((,[])<$>board_field)

newGame :: IO HiveGame
newGame = do
    let starts1 = List.zip [Ant, Bug, Cricket, Bee, Spider] [3,2,2,1,2::Int]
        starts2 = [(Stone player insect,i)| player<-[minBound..maxBound], (insect,i)<- starts1]
        hivegame_playertypes = Map.fromList [(White,Human),(Black,AI)]
        board = Board White (Map.fromList starts2) Map.empty
    hivegame_boardview <- newIORef $ initBoardView hivegame_playertypes board
    hivegame_board <- newIORef board
    return HiveGame{..}

possibleMoves :: Board -> [Move]
possibleMoves _ = []
applyMove :: Move -> Board -> Board
applyMove _ hg = hg

