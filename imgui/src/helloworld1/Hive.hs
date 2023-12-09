{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language PackageImports #-}
{-# language ViewPatterns #-}
{-# language ParallelListComp #-}


module Hive

where

import "JuicyPixels" Codec.Picture as JuicyPixels
import "JuicyPixels" Codec.Picture.Types as JuicyPixels

import Data.Vector.Storable (unsafeWith)
import Foreign.Storable (Storable)
import Data.Maybe
import Data.Typeable
import Data.List as List
import Data.Map.Strict as Map
import Control.Monad
import Control.Applicative
import System.IO.Unsafe (unsafePerformIO)
import Foreign

import Graphics.Rasterific as R
import Graphics.Rasterific.Texture as R
import Graphics.Rasterific.Transformations as R
import Graphics.Rasterific.Immediate as R
--import Graphics.Rasterific.Command as R -- hidden

import "FontyFruity" Graphics.Text.TrueType as F
import HiveGame
import Data.IORef
import Data.Monoid
import Data.Semigroup

-- 🐜🦗🐝🐞🕷🪲🪳🦟
--ant = "\x1F41C"
--ant = "\xD83D\xDC1C"
--ant = "\xf0\x9f\x90\x9c"

--x<-F.loadFontFile "LiberationMono-Regular.ttf"
--x<-F.loadFontFile "Roboto-Regular.ttf"
--x<-F.loadFontFile "HackNerdFont-Regular.ttf"
--x<-F.loadFontFile "NotoColorEmoji.ttf"

{-# NOINLINE fontWminsects #-}
fontWminsects :: Font
fontWminsects = unsafePerformIO $ do
    x<-F.loadFontFile "fonts/Wminsects1-nXKV.ttf"
    either fail return x






-- rescaled and centered to box:  -100,-100 to 100,100
boxed :: forall px. RenderablePixel px => Drawing px () -> Drawing px ()
boxed drawing = do
    let width=1
        height=1
        dpi=96
        --px = PixelRGBA8 0x0 0x0 0x0 255
        px = JuicyPixels.unpackPixel 0
        drawo = R.drawOrdersOfDrawing width height dpi px drawing :: [R.DrawOrder px]
        R.PlaneBound{_planeMinBound,_planeMaxBound} = mconcat $ R.planeBounds <$> drawo
    let V2 w h = _planeMaxBound - _planeMinBound
        maxwh = max w h
        s = 200/maxwh
        center = R.applyVectorTransformation (R.scale 0.5 0.5) (_planeMinBound + _planeMaxBound)
        movetocenter = R.translate (-center)
    R.withTransformation (R.scale s s <> movetocenter) drawing


boxedText :: RenderablePixel px => Float -> (Float,Float) -> Font -> String -> Drawing px ()
boxedText angle (x,y) font text = R.withTransformation (R.rotate (angle*pi/180) <> R.translate (R.V2 x y)) $ boxed $ R.printTextAt font (PointSize 100) (V2 0 0) text


-- -100,-100 to 100,100
antDrawing,queenDrawing,spiderDrawing,cricketDrawing,bugDrawing :: RenderablePixel px => Drawing px ()

antDrawing = boxedText (-27) (-7,0) fontWminsects "a"
bugDrawing = boxedText 90 (0,0) fontWminsects "z"
cricketDrawing = boxedText (-30) (0,0) fontWminsects "L"
queenDrawing = boxedText 0 (-5,-20) fontWminsects "g"
spiderDrawing = boxedText 0 (0,0) fontWminsects "P"



dr :: Stone -> Drawing PixelRGBA8 ()
dr (Stone player insect) = f x
    where
        f = case player of
                White -> stoneDrawing (PixelRGBA8 0xbf 0xbf 0xbf 255) (PixelRGBA8 0x0 0x0 0x0 255)
                Black -> stoneDrawing (PixelRGBA8 0x1f 0x1f 0x1f 255) (PixelRGBA8 0xcf 0xcf 0x7f 255)
        x = case insect of
                Ant -> antDrawing
                Bug -> bugDrawing
                Cricket -> cricketDrawing
                Bee -> queenDrawing
                Spider -> spiderDrawing

dr' :: [Stone] -> Drawing PixelRGBA8 ()
dr' [] = mempty
dr' (fmap dr->(s:ss)) = mconcat $ s : [ R.withTransformation (R.translate (R.V2 50 y) <> R.scale 0.17 0.17) (s<>targetMarking)
                                      | s<-ss | y<-[60,30,0,-30,-60]]

edgeColor = PixelRGBA8 0x7f 0x7f 0x7f 0xff
markingColor = PixelRGBA8 0x2f 0x2f 0xcf 0xff
stoneDrawing :: PixelRGBA8 -> PixelRGBA8 -> Drawing PixelRGBA8 () -> Drawing PixelRGBA8 ()
stoneDrawing stoneColor textColor item = id
        $   (R.withTexture (R.uniformTexture stoneColor)
            $ R.fill
            $ R.lineFromPath $ zipWith R.V2 [-50,50,99,50,-50,-99,-50] [-96,-96,0,96,96,0,-96]
            )
        <>  (R.withTexture (R.uniformTexture edgeColor)
            $ R.stroke 2 R.JoinRound (R.CapRound, R.CapRound)
            $ R.lineFromPath $ zipWith R.V2 [-50,50,99,50,-50,-99,-50] [-96,-96,0,96,96,0,-96]
            )
        <>  (R.withTexture (R.uniformTexture textColor)
            $ R.withTransformation (R.scale 0.7 0.7) item
            )

targetMarking :: Drawing PixelRGBA8 ()
targetMarking = (R.withTexture (R.uniformTexture markingColor)
            $ R.stroke 5 R.JoinRound (R.CapRound, R.CapRound)
            $ R.lineFromPath $ zipWith R.V2 [-50,50,99,50,-50,-99,-50] [-96,-96,0,96,96,0,-96]
            )

-- grey,black
hiveDrawing :: Drawing PixelRGBA8 () -> Drawing PixelRGBA8 ()
hiveDrawing = stoneDrawing (PixelRGBA8 0x7f 0x7f 0x7f 255) (PixelRGBA8 0x0 0x0 0x0 255)


type MousePos = R.Point
hiveImage :: HiveGame -> Int -> Int -> MousePos -> IO DynamicImage
hiveImage hivegame w h mp = do
    (BoardView (_::Player,_::PlayerType,maybeVMove::Maybe VMove) (m::Map BoardPos ([Stone],Maybe VMove))) <- readIORef (hivegame_boardview hivegame)
    let isDragging = isJust maybeVMove
        targets = case maybeVMove of
                    Nothing -> [ bp | (bp,(_,Just _ :: Maybe VMove)) <- Map.toList m]
                    Just (VMove _ _ bps) -> bps
    let stoneX = 150
        stoneY = 100
        drawing :: Drawing PixelRGBA8 ()
        drawing = mconcat $
                    [ R.withTransformation (R.translate (R.V2 (fromIntegral $ x*stoneX) (fromIntegral $ y*stoneY))) $ dr' stones
                    | (BoardPos x y,(stones,_ :: Maybe VMove)) <- Map.toList m
                    ] <>
                    [ R.withTransformation (R.translate (R.V2 (fromIntegral $ x*stoneX) (fromIntegral $ y*stoneY))) $ targetMarking
                    | (BoardPos x y) <- targets
                    ]
        (Min minx,Max maxx,Min miny,Max maxy) = minmaxBoardPos $ Map.keys m
        s :: Float
        s = (fromIntegral w / fromIntegral (stoneX * (maxx-minx) + 200)) `min` (fromIntegral h / fromIntegral (stoneY * (maxy-miny) + 200))
        trans =  R.scale s s <>
                 R.translate (R.V2 (fromIntegral $ 100-minx*stoneX) (fromIntegral $ 100-miny*stoneY))

    return $ ImageRGBA8 $ R.renderDrawing w h (PixelRGBA8 0x7f 0x7f 0x7f 255)
                        $ R.withTransformation trans
                        $ drawing


    --return $ ImageRGBA8 $ R.renderDrawing w h (PixelRGBA8 0x7f 0x7f 0x7f 255)
    --                    $ R.withTransformation (R.translate (R.V2 100 100))
    --                    $ dr (Stone Black Bee)












