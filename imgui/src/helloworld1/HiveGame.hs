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
import Data.Maybe
import Data.Semigroup
import Data.Foldable as Foldable
import "mtl" Control.Monad.State.Strict as State
import qualified GHC.Base (liftA2)

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
    , board_gameEnded :: Maybe (Either Draw Player)
    }

data VMove = VMove !Stone !BoardPos ![BoardPos]
data BoardView = BoardView (Player,PlayerType,Maybe VMove) (Map BoardPos ([Stone],Maybe VMove))


minmaxBoardPos :: [BoardPos] -> (Min Int,Max Int,Min Int,Max Int)
minmaxBoardPos bps = (Min minx,Max maxx,Min miny,Max maxy)
    where
    (Min minx,Max maxx,Min miny,Max maxy) = mconcat [ (Min x,Max x,Min y,Max y) | BoardPos x y <- BoardPos 0 0 : bps ]


blackIsUp :: Map Player PlayerType -> Bool
blackIsUp playertypes = playertypes == Map.fromList[(White,Human),(Black,AI)]


-- white is up, black is down, unless white is human and black is AI
-- white begins
initBoardView :: [Move] -> Map Player PlayerType -> Board -> BoardView
initBoardView possMoves playertypes board@Board{..} = BoardView (board_playing,currentPlayerType,Nothing) view_field
    where
        currentPlayerType = playertypes Map.! board_playing
        insectcount = List.length(universe::[Insect])
        (Min (pred->minx),Max (succ->maxx),Min miny,Max maxy) = minmaxBoardPos $ BoardPos (1-insectcount) 0 : BoardPos (insectcount-1) 0 : Map.keys board_field
        view_field :: Map BoardPos ([Stone],Maybe VMove)
        view_field = Map.fromList $ home <> field
        home :: [(BoardPos, ([Stone],Maybe VMove))]
        home = do
            let players = if blackIsUp playertypes
                            then [Black, White]
                            else [White, Black]
            (player,y,xs) <- List.zip3 players [miny - 5,maxy + 5] [[maxx,maxx-2 ..],[minx,minx+2 ..]]
            (insect,x) <- List.zip universe xs
            let stone = Stone player insect
            let n = board_home Map.! stone
            guard $ n > 0
            let bp = BoardPos x y
            let moveTo = [ bp' |  Move s Nothing bp' <- possMoves, stone==s ]
            let vmove = do
                    guard (currentPlayerType == Human)
                    guard (not $ List.null moveTo)
                    Just (VMove stone bp moveTo)
            [(bp, (List.replicate n stone, vmove))]
        field :: [(BoardPos, ([Stone],Maybe VMove))]
        field = do
            (bp,stones@(stone:_)) <- Map.toList board_field
            let moveTo = [ bp' |  Move s (Just x) bp' <- possMoves, x==bp, (s==stone) || error "BUG: impossible move!"]
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
        board = Board White starts2 Map.empty Nothing
        possMoves = possibleMoves hivegame_playertypes board :: [Move]
    hivegame_boardview <- newIORef $ initBoardView possMoves hivegame_playertypes board
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

resetStone :: HiveGame -> IO ()
resetStone hivegame = atomicModifyIORef' (hivegame_boardview hivegame) $ (,()) . resetVMove

dropStone :: BoardPos -> HiveGame -> IO ()
dropStone bp_to hivegame@(HiveGame{..}) = do
    bv@(BoardView (p,pt,mv) m) <- readIORef hivegame_boardview

    case mv of

        Just (VMove stone bp_from bps) | bp_to `List.elem` bps -> do
            let move = Move stone (Just bp_from) bp_to
            (board,possMoves) <- do
                board <- atomicModifyIORef' hivegame_board $ (\b->(b,b)) . applyMove move
                let possMoves = possibleMoves hivegame_playertypes board :: [Move]
                if List.null possMoves
                then do
                    board <- atomicModifyIORef' hivegame_board $ (\b->(b,b)) . endTurn
                    let possMoves = possibleMoves hivegame_playertypes board :: [Move]
                    return (board,possMoves)
                else do
                    return (board,possMoves)

            let bv' = initBoardView possMoves hivegame_playertypes board
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
    = board{board_field=Map.update (\case { [_] -> Nothing ; (_:s)->Just s }) bp_from board_field}
takeFromBoard stone _ board@(Board{..})
    = board{board_home=Map.adjust pred stone board_home}

putOnBoard :: Stone -> BoardPos -> Board -> Board
putOnBoard stone bp_to board@(Board{..})
    = board{board_field=Map.alter (Just . maybe [stone] (stone:)) bp_to board_field}
endTurn :: Board -> Board
endTurn board@(Board{..}) = board{board_playing=opponent board_playing, board_gameEnded = gameEnded board_field}

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

rotateCWn :: Int -> Direction -> Direction
rotateCWn n dir = toEnum $ (fromEnum dir + n) `mod` 6

rotateCW :: Direction -> Direction
rotateCW = rotateCWn 1
rotateCCW :: Direction -> Direction
rotateCCW = rotateCWn 5

data Neighbours a = Neighbours a a a a a a
    deriving (Eq,Ord,Show,Functor,Foldable)

neighboursDirection :: Neighbours Direction
neighboursDirection = Neighbours North NorthEast SouthEast South SouthWest NorthWest

instance Applicative Neighbours where
  pure a = Neighbours a a a a a a
  (<*>) (Neighbours aa bb cc dd ee ff) (Neighbours a b c d e f) = Neighbours (aa a)(bb b)(cc c)(dd d)(ee e)(ff f)
  liftA2 x (Neighbours aa bb cc dd ee ff) (Neighbours a b c d e f) = Neighbours (x aa a)(x bb b)(x cc c)(x dd d)(x ee e)(x ff f)
  (*>) _ fb = fb
  (<*) fa _ = fa

zipNeighboursWith :: (a -> b -> c) -> Neighbours a -> Neighbours b -> Neighbours c
zipNeighboursWith = GHC.Base.liftA2
zipNeighbours :: Neighbours a -> Neighbours b -> Neighbours (a,b)
zipNeighbours = GHC.Base.liftA2 (,)

mapWithDir :: (Direction -> a -> b) -> Neighbours a -> Neighbours b
mapWithDir x = GHC.Base.liftA2 x neighboursDirection

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


observeNeighbours :: Neighbours a -> Neighbours (a,a,a)
observeNeighbours ns = mapWithDir (\d ma -> (inDirection ns $ rotateCCW d,ma,inDirection ns $ rotateCW d)) ns


reachableCricket :: Map BoardPos a -> BoardPos -> [BoardPos]
reachableCricket field bp = catMaybes $ Foldable.toList jumpedOverNeighbours
    where
        stoneAt :: BoardPos -> Bool
        stoneAt x = x `Map.member` field
        isNotFree :: BoardPos -> Maybe BoardPos
        isNotFree n = if stoneAt n then Just n else Nothing
        knownNeighbours = isNotFree <$> neighbours bp :: Neighbours (Maybe BoardPos)

        jump :: Direction -> BoardPos -> BoardPos
        jump dir bp | stoneAt bp = jump dir $ neighbour bp dir
        jump dir bp = bp

        jumpedOverNeighbours :: Neighbours (Maybe BoardPos)
        jumpedOverNeighbours = mapWithDir (\d -> fmap (jump d)) knownNeighbours



reachableSpider :: Map BoardPos a -> BoardPos -> [BoardPos]
reachableSpider field bp0 = Set.toList . Set.fromList $ do
    bp1 <- reachableBee field bp0
    bp2 <- reachableBee field bp1
    guard $ bp2/=bp0
    bp3 <- reachableBee field bp2
    guard $ bp3/=bp0
    guard $ bp3/=bp1
    return bp3



reachableBug :: Map BoardPos [a] -> BoardPos -> [BoardPos]
reachableBug field bp = catMaybes $ Foldable.toList elevatorAtmostOnce'
    where
        heightAt :: BoardPos -> Int
        heightAt x = maybe 0 List.length $ x `Map.lookup` field
        h :: Int
        h = heightAt bp
        ns = neighbours bp :: Neighbours BoardPos
        observedNeighbours :: Neighbours (Int,Int,Int)
        observedNeighbours = observeNeighbours (heightAt <$> ns)
        elevatorAtmostOnce' :: Neighbours (Maybe BoardPos)
        elevatorAtmostOnce' = (fmap . const) <$> ns <*> elevatorAtmostOnce
        elevatorAtmostOnce :: Neighbours (Maybe ())
        elevatorAtmostOnce = f <$> observedNeighbours
            where
                f (a,b,c) | min a c > max b h = Nothing -- NoSqueezing
                          | (a,b,c,h) == (0,0,0,0) = Nothing
                          | otherwise = Just ()


-- TODO TODO TODO TODO TODO TODO TODO

reachableBee :: Map BoardPos a -> BoardPos -> [BoardPos]
reachableBee field bp = catMaybes $ Foldable.toList onlySlidingNoSqueezing
    where
        stoneAt :: BoardPos -> Bool
        stoneAt x = x `Map.member` field
        isFree :: BoardPos -> Maybe BoardPos
        isFree n = if stoneAt n then Nothing else Just n
        freeNeighbours = isFree <$> neighbours bp :: Neighbours (Maybe BoardPos)
        observedNeighbours :: Neighbours (Maybe BoardPos,Maybe BoardPos,Maybe BoardPos)
        observedNeighbours = observeNeighbours freeNeighbours
        onlySlidingNoSqueezing :: Neighbours (Maybe BoardPos)
        onlySlidingNoSqueezing = fmap f observedNeighbours
            where
                f (Nothing,Just a,Just _) = Just a
                f (Just _,Just a,Nothing) = Just a
                f _ = Nothing




reachableAnt :: Map BoardPos a -> BoardPos -> [BoardPos]
reachableAnt field bp = Set.toList . Set.delete bp . Set.fromList $ fmap fst $ List.takeWhile (/= end) $ List.tail $ List.iterate step end
    where
        stoneAt :: BoardPos -> Bool
        stoneAt x = x `Map.member` field
        end :: (BoardPos,Direction) -- BoardPos is empty, looking at a stone in Direction
        end = (bp,List.head [ d | d<-universe, stoneAt $ neighbour bp d ])

        isFree :: (BoardPos,Direction) -> Maybe BoardPos
        isFree (bp,d) = let n = neighbour bp d in if stoneAt n then Nothing else Just n

        step :: (BoardPos,Direction) -> (BoardPos,Direction)
        step (bp,d) = let e = rotateCW d
                          f = rotateCW e
                       in case (isFree (bp,e),isFree (bp,f)) of
                            (Nothing,_) -> (bp,e)
                            (Just _,Nothing) -> (bp,f)
                            (Just bp',_) -> (bp',rotateCCW d)


reachableOutside :: Player -> Map BoardPos [Stone] -> [BoardPos]
reachableOutside player field = List.filter isValid outsides
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




bridges :: Map BoardPos a -> Set BoardPos
bridges field = Set.fromList found_bridges
    where
        f :: BoardPos -> Maybe BoardPos
        f bp = const bp <$> Map.lookup bp field
        field_neighbours :: Map BoardPos (Neighbours (Maybe BoardPos))
        field_neighbours = Map.mapWithKey (\bp _ -> f <$> neighbours bp) field

        fi :: BoardPos -> (Neighbours (Maybe BoardPos)) -> (Neighbours (Maybe (BoardPos,Direction)))
        fi beach ns = flip mapWithDir ns \dir -> \case
                                                    Just _ -> Nothing
                                                    Nothing -> do
                                                        let d = rotateCW dir
                                                        case ns `inDirection` d of
                                                            Nothing -> Just (beach,d)
                                                            Just n -> Just (n,rotateCCW dir)
        field_island_cw :: Map BoardPos (Neighbours (Maybe (BoardPos,Direction)))
        field_island_cw = Map.mapWithKey fi field_neighbours
        field_island_cw' :: Map (BoardPos,Direction) (BoardPos,Direction)
        field_island_cw' = Map.fromList [ ((bp,dir),x)
                                        | (bp,ns) <- Map.toList field_island_cw
                                        , (dir,Just x) <- Foldable.toList $ mapWithDir (,) ns
                                        ]



        extractNext :: Ord a => a -> State (Map a a) (Maybe a)
        extractNext k = State.state $ Map.alterF (,Nothing) k

        extractRing_ :: Ord a => [a] -> a -> State (Map a a) [a]
        extractRing_ !rs !k = do
            ma <- extractNext k
            case ma of
                Nothing -> return (k:rs)
                Just !a -> extractRing_ (k:rs) a

        extractRing :: Ord a => State (Map a a) [a]
        extractRing = do
            (fmap fst->mk) <- State.gets Map.lookupMin
            maybe (return []) (extractRing_ []) mk

        extractRings :: forall a. Ord a => Map a a -> [[a]]
        extractRings = List.unfoldr f
            where
                f :: Map a a -> Maybe ([a], Map a a)
                f m = case State.runState extractRing m of
                        ([],_) -> Nothing
                        x -> Just x

        rings :: [[(BoardPos,Direction)]]
        rings = extractRings field_island_cw'
        rings' :: [[BoardPos]]
        rings' = [ if List.head r' == List.last r' then List.tail r' else r' | r<-rings , let r' = fmap List.head $ List.group $ fst <$> r ]
        found_bridges :: [BoardPos]
        found_bridges = do
            r<-rings'
            g<-List.group $ List.sort r
            List.take 1 $ List.tail g


movesOnField :: Player -> Map BoardPos [Stone] -> [Move]
movesOnField player field = do
    -- no boardpos that are bridges, unless stones are stacked
    let bri = bridges field :: Set BoardPos
    (bp_from,stones@(stone:_)) <- Map.toList field
    case stones of
        (Stone p _:_) | p /= player -> []
        [_] | Set.member bp_from bri -> []
        _ -> [()]
    --
    let field' = Map.update (\case { [_] -> Nothing ; (_:s)->Just s }) bp_from field

    bp_to <- case stone of
                Stone _ Ant -> reachableAnt field' bp_from :: [BoardPos]
                Stone _ Bee -> reachableBee field' bp_from :: [BoardPos]
                Stone _ Cricket -> reachableCricket field' bp_from :: [BoardPos]
                Stone _ Spider -> reachableSpider field' bp_from :: [BoardPos]
                Stone _ Bug -> reachableBug field' bp_from :: [BoardPos]
                        -- TODO TODO TODO TODO TODO TODO TODO


    return $ Move stone (Just bp_from) bp_to





possibleMoves :: Map Player PlayerType -> Board -> [Move]
possibleMoves playertypes Board{board_playing=player,..} = if isEnd then [] else strategy
    where
        isEnd = isJust $ board_gameEnded

        target_from_home :: [BoardPos]
        target_from_home = case Map.toList board_field of
                            [] -> [BoardPos 0 0]
                            [_] -> [BoardPos 0 (y*u) | let y = if player==Black then 2 else -2, let u = if blackIsUp playertypes then -1 else 1]
                            _ -> reachableOutside player board_field
        strategy = case (isQueenOut,stonesOut) of
                    (True,_) -> (target_from_home >>= only_from_home) <> movesOnField player board_field
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


data Draw = Draw deriving (Eq,Ord,Enum,Bounded,Show,Generic)
gameEnded :: Map BoardPos [Stone] -> Maybe (Either Draw Player)
gameEnded m = case loosers of
                    [] -> Nothing
                    [p] -> Just $ Right p
                    [_,_] -> Just $ Left Draw
    where
        hasStone :: BoardPos -> Bool
        hasStone x = Map.member x m
        loosers :: [Player]
        loosers = [ player
                  | (bp,stones) <- Map.toList m
                  , (Stone player Bee) <- stones
                  , all hasStone $ neighbours bp
                  ]



triggerAI :: HiveGame -> IO ()
triggerAI _ = return ()














