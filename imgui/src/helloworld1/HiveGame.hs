{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language PackageImports #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric #-}


module HiveGame where


import Control.Monad

import Data.Map.Strict as Map
import Data.List as List
import Data.IORef
import Data.Monoid
import Data.Semigroup

import GHC.Generics
import "universe-base" Data.Universe.Class as Universe
--import "universe-base" Data.Universe.Helpers (universeDef)
import "universe-base" Data.Universe.Generic as Universe

data HiveGame = HiveGame
    { hivegame_playertypes :: Map Player PlayerType
    , hivegame_board :: IORef Board
    , hivegame_boardview :: IORef BoardView
    }


data Player = White | Black deriving (Eq,Ord,Enum,Bounded,Show,Generic)
data Insect = Ant | Bug | Cricket | Bee | Spider deriving (Eq,Ord,Enum,Bounded,Show,Generic)
data Stone = Stone !Player !Insect deriving (Eq,Ord,Show,Generic)

instance Universe Player
instance Universe Insect
instance Universe Stone where universe = universeGeneric
instance Finite Player
instance Finite Insect
instance Finite Stone

-- | (x+y) `mod` 2 == 0
data BoardPos = BoardPos !Int !Int deriving (Eq,Ord,Show)

data PlayerType = Human | AI deriving (Eq,Ord,Enum,Bounded,Show,Generic)
instance Universe PlayerType
instance Finite PlayerType

data Move = Move !Stone !(Maybe BoardPos) !BoardPos
data Board = Board
    { board_playing :: Player
    , board_home :: Map Stone Int
    , board_field :: Map BoardPos [Stone]
    }

data VMove = VMove !Stone !BoardPos ![BoardPos]
data BoardView = BoardView (Player,PlayerType,Maybe VMove) (Map BoardPos ([Stone],Maybe VMove))


-- white is up, black is down, unless white is human and black is AI
initBoardView :: Map Player PlayerType -> Board -> BoardView
initBoardView m board@Board{..} = BoardView (board_playing,m Map.! board_playing,Nothing) view_field
    where
        (Min minx,Max maxx,Min miny,Max maxy) = mconcat [ (Min x,Max x,Min y,Max y) | BoardPos x y <- BoardPos (-5) 0 : BoardPos 5 0 : Map.keys board_field ]
        view_field :: Map BoardPos ([Stone],Maybe VMove)
        view_field = Map.fromList $ home <> field
        home :: [(BoardPos, ([Stone],Maybe VMove))]
        home = do
            let players = if m == Map.fromList[(White,Human),(Black,AI)]
                            then [Black, White]
                            else [White, Black]
            (player,y,xs) <- List.zip3 players [miny - 3,maxy + 3] [[maxx,maxx-2 .. maxx-8],[minx,minx+2 .. minx+8]]
            (insect,x) <- List.zip universe xs
            let stone = Stone player insect
            let n = board_home Map.! stone
            guard $ n > 0
            let bp = BoardPos x y
            let moveTo = [ bp' |  Move s Nothing bp' <- possibleMoves board, stone==s ]
            let vmove = guard (not $ List.null moveTo) >> Just (VMove stone bp moveTo)
            [(bp, (List.replicate n stone, vmove))]
        field :: [(BoardPos, ([Stone],Maybe VMove))]
        field = do
            (bp,stones@(stone:_)) <- Map.toList board_field
            let moveTo = [ bp' |  Move s (Just x) bp' <-possibleMoves board, x==bp, (s==stone) || error "BUG: impossible move!"]
            let vmove = guard (not $ List.null moveTo) >> Just (VMove stone bp moveTo)
            [(bp, (stones, vmove))]


newGame :: IO HiveGame
newGame = do
    let starts1 = List.zip [Ant, Bug, Cricket, Bee, Spider] [3,2,2,1,2::Int]
        starts2 = [(Stone player insect,i)| player<-universe, (insect,i)<- starts1]
        hivegame_playertypes = Map.fromList [(White,Human),(Black,AI)]
        board = Board White (Map.fromList starts2) Map.empty
    hivegame_boardview <- newIORef $ initBoardView hivegame_playertypes board
    hivegame_board <- newIORef board
    return HiveGame{..}

---------- ↓↓↓ TODO ↓↓↓ ----------

dragStone :: BoardPos -> HiveGame -> IO ()
dragStone _ _ = return ()
dropStone :: BoardPos -> HiveGame -> IO ()
dropStone _ _ = return ()

possibleMoves :: Board -> [Move]
possibleMoves _ = []
applyMove :: Move -> Board -> Board
applyMove _ hg = hg




