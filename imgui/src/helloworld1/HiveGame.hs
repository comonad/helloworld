{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language PackageImports #-}
{-# language ViewPatterns #-}
{-# language DeriveGeneric #-}
{-# language ParallelListComp #-}


module HiveGame where


import Control.Monad
import Control.Arrow

import Data.Map.Strict as Map
import Data.Set as Set
import Data.List as List
import Data.IORef
import Data.Monoid
import Data.Semigroup
import Data.Foldable as Foldable

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


minmaxBoardPos :: [BoardPos] -> (Min Int,Max Int,Min Int,Max Int)
minmaxBoardPos bps = (Min minx,Max maxx,Min miny,Max maxy)
    where
    (Min minx,Max maxx,Min miny,Max maxy) = mconcat [ (Min x,Max x,Min y,Max y) | BoardPos x y <- BoardPos 0 0 : bps ]


-- white is up, black is down, unless white is human and black is AI
-- white begins
initBoardView :: Map Player PlayerType -> Board -> BoardView
initBoardView m board@Board{..} = BoardView (board_playing,currentPlayerType,Nothing) view_field
    where
        currentPlayerType = m Map.! board_playing
        insectcount = List.length(universe::[Insect])
        (Min minx,Max maxx,Min miny,Max maxy) = minmaxBoardPos $ BoardPos (-insectcount) 0 : BoardPos insectcount 0 : Map.keys board_field
        view_field :: Map BoardPos ([Stone],Maybe VMove)
        view_field = Map.fromList $ home <> field
        blackIsUp :: Bool
        blackIsUp = m == Map.fromList[(White,Human),(Black,AI)]
        home :: [(BoardPos, ([Stone],Maybe VMove))]
        home = do
            let players = if blackIsUp
                            then [Black, White]
                            else [White, Black]
            (player,y,xs) <- List.zip3 players [miny - 5,maxy + 5] [[maxx,maxx-2 ..],[minx,minx+2 ..]]
            (insect,x) <- List.zip universe xs
            let stone = Stone player insect
            let n = board_home Map.! stone
            guard $ n > 0
            let bp = BoardPos x y
            let moveTo = [ bp' |  Move s Nothing bp' <- possibleMoves blackIsUp board, stone==s ]
            let vmove = do
                    guard (currentPlayerType == Human)
                    guard (not $ List.null moveTo)
                    Just (VMove stone bp moveTo)
            [(bp, (List.replicate n stone, vmove))]
        field :: [(BoardPos, ([Stone],Maybe VMove))]
        field = do
            (bp,stones@(stone:_)) <- Map.toList board_field
            let moveTo = [ bp' |  Move s (Just x) bp' <-possibleMoves blackIsUp board, x==bp, (s==stone) || error "BUG: impossible move!"]
            let vmove = do
                    guard (currentPlayerType == Human)
                    guard (not $ List.null moveTo)
                    Just (VMove stone bp moveTo)
            [(bp, (stones, vmove))]

starts2 :: Map Stone Int
starts2 = Map.fromList [(Stone player insect,i)| player<-universe, (insect,i)<- starts1]
    where
    starts1 = List.zip [Ant, Bug, Cricket, Bee, Spider] [3,2,3,1,2::Int]

newGame :: IO HiveGame
newGame = do
    let hivegame_playertypes = Map.fromList [(White,Human),(Black,Human)]
        board = Board White starts2 Map.empty
    hivegame_boardview <- newIORef $ initBoardView hivegame_playertypes board
    hivegame_board <- newIORef board
    return HiveGame{..}


-- BoardView (Player,PlayerType,Maybe VMove) (Map BoardPos ([Stone],Maybe VMove))
resetVMove :: BoardView -> BoardView
resetVMove bv@(BoardView (p,pt,Nothing) m) = bv
resetVMove (BoardView (p,pt,Just (VMove stone bp _)) m) = BoardView (p,pt,Nothing) m'
    where
        m' = Map.adjust (\(ss,mvmove)->(stone:ss,mvmove)) bp m


dragStone :: BoardPos -> HiveGame -> IO ()
dragStone bp HiveGame{..} = do
    atomicModifyIORef' hivegame_boardview $ (. resetVMove) $ \bv@(BoardView (p,pt,_) m) -> (,()) $
        case Map.lookup bp m of
            Nothing -> bv
            Just (_ :: [Stone],mv) -> BoardView (p,pt,mv) $ Map.adjust (first List.tail) bp m

dropStone :: BoardPos -> HiveGame -> IO ()
dropStone bp_to hivegame@(HiveGame{..}) = do
    bv@(BoardView (p,pt,mv) m) <- readIORef hivegame_boardview

    case mv of

        Just (VMove stone bp_from bps) | bp_to `List.elem` bps -> do
            let move = Move stone (Just bp_from) bp_to
            board <- atomicModifyIORef' hivegame_board (\b->(applyMove move b,b))
            let bv' = initBoardView hivegame_playertypes board
            writeIORef hivegame_boardview bv'
            case bv' of
                BoardView (_,AI,_) _ -> triggerAI hivegame
                _ -> return ()

        _ -> do
            writeIORef hivegame_boardview $ resetVMove bv



applyMove :: Move -> Board -> Board
applyMove (Move stone (Just bp_from) bp_to) board | bp_from==bp_to = board
applyMove (Move stone maybe_bp_from bp_to) board
    = endTurn $ putOnBoard stone bp_to $ takeFromBoard stone maybe_bp_from $ board

takeFromBoard :: Stone -> (Maybe BoardPos) -> Board -> Board
takeFromBoard stone (Just bp_from) board@(Board{..})
    | Map.member bp_from board_field
    = board{board_field=Map.update (\case { (_:s) -> Just s ; _ -> Nothing}) bp_from board_field}
takeFromBoard stone _ board@(Board{..})
    = board{board_home=Map.adjust pred stone board_home}

putOnBoard :: Stone -> BoardPos -> Board -> Board
putOnBoard stone bp_to board@(Board{..})
    = board{board_field=Map.alter (Just . maybe [stone] (stone:)) bp_to board_field}
endTurn :: Board -> Board
endTurn board@(Board{..}) = board{board_playing=opponent board_playing}

opponent :: Player -> Player
opponent White = Black
opponent Black = White

{-
BoardPos !Int !Int
Move !Stone !(Maybe BoardPos) !BoardPos
Stone !Player !Insect
Ant | Bug | Cricket | Bee | Spider

Board
    { board_playing :: Player
    , board_home :: Map Stone Int
    , board_field :: Map BoardPos [Stone]
    }
-}

---------- ↓↓↓ TODO ↓↓↓ ----------

data Direction = North | NorthEast | SouthEast | South | SouthWest | NorthWest
    deriving (Eq,Ord,Enum,Bounded,Show,Generic)
instance Universe Direction
instance Finite Direction

data Neighbours a = Neighbours a a a a a a
    deriving (Eq,Ord,Show,Functor,Foldable)

inDirection :: Neighbours a -> Direction -> a
inDirection (Neighbours x _ _ _ _ _) North = x
inDirection (Neighbours _ x _ _ _ _) NorthEast = x
inDirection (Neighbours _ _ x _ _ _) SouthEast = x
inDirection (Neighbours _ _ _ x _ _) South = x
inDirection (Neighbours _ _ _ _ x _) SouthWest = x
inDirection (Neighbours _ _ _ _ _ x) NorthWest = x

neighbour :: BoardPos -> Direction -> BoardPos
neighbour bp dir = (neighbours bp) `inDirection` dir
neighbours :: BoardPos -> Neighbours BoardPos
neighbours (BoardPos x y) = Neighbours (BoardPos (x  ) (y-2))
                                       (BoardPos (x+1) (y-1))
                                       (BoardPos (x+1) (y+1))
                                       (BoardPos (x  ) (y+2))
                                       (BoardPos (x-1) (y+1))
                                       (BoardPos (x-1) (y-1))

reachableAnt :: Map BoardPos a -> BoardPos -> [BoardPos]
reachableAnt field = undefined -- TODO

reachableOutside :: Player -> Map BoardPos [Stone] -> [BoardPos]
reachableOutside player field = undefined
    where
        bp0 :: BoardPos
        bp0 = case List.foldl1' (<>) [(Max (y,bp))|bp@(BoardPos x y)<-Map.keys field] of
                Max (_,BoardPos x y) -> BoardPos x (y+2)
        outsides :: [BoardPos]
        outsides = bp0:reachableAnt field bp0

        enemyFields :: Set BoardPos
        enemyFields = Map.keysSet $ Map.filter (\(Stone p _:_) -> p/=player) field
        isValid :: BoardPos -> Bool
        isValid bp = all (\e->Set.notMember e enemyFields) (neighbours bp)


-- | blackIsUp :: Bool
possibleMoves :: Bool -> Board -> [Move]
possibleMoves blackIsUp Board{board_playing=player,..} = strategy
    where

        target_from_home :: [BoardPos]
        target_from_home = case Map.toList board_field of
                            [] -> [BoardPos 0 0]
                            [_] -> [BoardPos 0 (y*u) | let y = if player==Black then 2 else -2, let u = if blackIsUp then -1 else 1]
                            _ -> reachableOutside player board_field
        strategy = case (isQueenOut,stonesOut) of
                    (True,_) -> undefined -- TODO no restrictions
                    (False,3) -> target_from_home >>= only_queen_out
                    (False,_) -> target_from_home >>= only_from_home
        only_queen_out :: BoardPos -> [Move]
        only_queen_out bp = [Move (Stone player Bee) Nothing bp]
        only_from_home :: BoardPos -> [Move]
        only_from_home bp = [ Move stone Nothing bp | (stone,n) <- Map.toList playerHome, n>0 ]

        isQueenOut :: Bool
        isQueenOut = (board_home Map.! Stone player Bee) == 0
        playerFilter :: Map Stone Int -> Map Stone Int
        playerFilter = Map.filterWithKey (\(Stone p _) _ -> p==player)
        playerHome :: Map Stone Int
        playerHome = playerFilter board_home
        stonesOut :: Int
        stonesOut = List.sum $ Map.elems $ Map.unionWith (-) (playerFilter starts2) playerHome






triggerAI :: HiveGame -> IO ()
triggerAI _ = return ()














