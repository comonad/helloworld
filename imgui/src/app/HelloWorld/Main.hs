{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}
{-# language PackageImports #-}
{-# language ViewPatterns #-}
{-# language ParallelListComp #-}


module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import Control.Monad
import Control.Applicative
import qualified DearImGui as ImGui
import qualified DearImGui.Raw as ImGui_RAW
import qualified DearImGui.Raw.DrawList as ImGui_DL
import qualified DearImGui.OpenGL2 as ImGui_OGL2 (openGL2Init,openGL2Shutdown,openGL2NewFrame,openGL2RenderDrawData)
import qualified DearImGui.SDL.OpenGL as ImGui_SDL_OGL (sdl2InitForOpenGL)
import qualified DearImGui.SDL as ImGui_SDL (sdl2Shutdown,sdl2NewFrame,pollEventsWithImGui)
import qualified "OpenGLRaw" Graphics.GL as GL
import qualified SDL
import qualified SDL.Internal.Numbered as SDL_N
import qualified DearImGui.FontAtlas as ImGui_Font
import Data.Bits
import Data.List as List
import Data.IORef
import Data.Traversable
import Data.Foldable as Foldable
import Data.Function
import Data.Text as Text
import Data.Set as Set
import Data.Int
import Data.Word
import qualified Foreign
import qualified "JuicyPixels" Codec.Picture as JuicyPixels
import Canvas
import Data.Typeable
import qualified Data.StateVar as StateVar
-- TODO: game Orbito
-- TODO: game Hive
import Hive
import qualified HiveGame
import qualified "Rasterific" Graphics.Rasterific.Linear as R

main :: IO ()
main = do
    -- Initialize SDL
    SDL.initializeAll

    runManaged do
        -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
        window <- do
            let title = "Hello, Dear ImGui!"
            let config = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
                                           , SDL.windowResizable = True
                                           --, SDL.windowHighDPI = True
                                           }
            managed $ bracket (SDL.createWindow title config) SDL.destroyWindow

        -- Create an OpenGL context
        glContext <- managed $ bracket (SDL.glCreateContext window) SDL.glDeleteContext

        -- Create an ImGui context
        _ <- managed $ bracket ImGui.createContext ImGui.destroyContext

        model <- liftIO initModel

        -- Initialize ImGui's SDL2 backend
        _ <- managed_ $ bracket_ (ImGui_SDL_OGL.sdl2InitForOpenGL window glContext) ImGui_SDL.sdl2Shutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ ImGui_OGL2.openGL2Init ImGui_OGL2.openGL2Shutdown

        --liftIO $ StateVar.get SDL.swapInterval >>= print
        liftIO $ SDL.swapInterval StateVar.$= SDL.SynchronizedUpdates


        liftIO $ mainLoop window model



type Tabledata = [(Text,Text,Text)]
data MouseeventType
    = MouseeventMove !(SDL.V2 Int32) -- delta
    | MouseeventKeyPress !Int !Word8
    | MouseeventKeyRelease !Int !Word8
    | MouseeventWheel !(SDL.V2 Int32) !SDL.MouseScrollDirection
data Mouseevent = Mouseevent
    { me_pos :: !(SDL.Point SDL.V2 Int32)
    , me_timestamp :: SDL.Timestamp
    , me_type :: MouseeventType
    }
data Mousedata = Mousedata
    { md_pos :: !(SDL.Point SDL.V2 Int32)
    , md_keys :: !(Set Int)
    , md_events :: ![Mouseevent] -- ascending in timestamp
    }

mouseClicks :: Model -> [(Bool,SDL.Point SDL.V2 Int32)]
mouseClicks Model{mousedata=Mousedata{md_events}} = do
    Mouseevent{me_pos,me_type} <- md_events
    b <- case me_type of
            MouseeventKeyPress 1 _ -> [True]
            MouseeventKeyRelease 1 _ -> [False]
            _ -> []
    return (b,me_pos)

data Model = Model {tabledata :: IORef Tabledata, mousedata::Mousedata, tex1::Maybe Tex, model_font::ImGui_Font.Font, hivegame::HiveGame.HiveGame}
initModel :: IO (IORef Model)
initModel = do
    tabledata <- newIORef [("1","2","3"), ("4","5","6"), ("4","3","1")]
    let md = Mousedata (SDL.P $ SDL.V2 0 0) Set.empty []
    --(Just model_font) <- ImGui_Font.addFontFromFileTTF_ "HackNerdFont-Regular.ttf" 32
    let model_font = undefined
    hivegame <- HiveGame.newGame :: IO HiveGame.HiveGame
    newIORef $ Model tabledata md Nothing model_font hivegame

sortBySortSpec :: ImGui.TableSortingSpecs -> Tabledata -> Tabledata
sortBySortSpec sortSpec = sortBy comp
    where
        col = case ImGui.tableSortingColumn sortSpec of
                            0 -> \(a,_,_)->a
                            1 -> \(_,b,_)->b
                            2 -> \(_,_,c)->c
                            _ -> error "Bug: Tabledata has too many columns"
        rev = if ImGui.tableSortingReverse sortSpec then compare EQ else id
        comp a b = rev $ (compare `on` col) a b


instance Semigroup ImGui.ImVec2 where
    (<>) (ImGui.ImVec2 x1 y1) (ImGui.ImVec2 x2 y2) = ImGui.ImVec2 (x1+x2) (y1+y2)

img1 :: (Int,Int) -> JuicyPixels.Image JuicyPixels.PixelRGBA8
img1 (w,h) = JuicyPixels.generateImage f w h where f (fromIntegral->x) (fromIntegral->y) = JuicyPixels.PixelRGBA8 255 (y+x) (y*y+x*x) 255

mainLoop :: SDL.Window -> IORef Model -> IO ()
mainLoop window ioref_model = unlessQuit $ \mouseevts -> do
    -- Tell ImGui we're starting a new frame
    ImGui_OGL2.openGL2NewFrame
    ImGui_SDL.sdl2NewFrame
    ImGui.newFrame

    ImGui.withWindowOpen "Hive" do
        (windowpos::ImGui.ImVec2)<-ImGui.getWindowPos
        ImGui.withChildOpen "image renderer" (ImGui.ImVec2 0 0) False (  ImGui.ImGuiWindowFlags_NoMove
                                                                     .|. ImGui.ImGuiWindowFlags_NoBackground
                                                                     .|. ImGui.ImGuiWindowFlags_NoTitleBar
                                                                     .|. ImGui.ImGuiWindowFlags_NoScrollbar
                                                                      ) $ do
            (childwindowpos::ImGui.ImVec2)<-ImGui.getWindowPos
            (contentpos::ImGui.ImVec2)<-ImGui.getCursorPos
            size@(ImGui.ImVec2 sizeW sizeH) <- ImGui.getWindowSize

            model <- readIORef ioref_model
            let imvecToV2 :: ImGui.ImVec2 -> SDL.V2 Float
                imvecToV2 (ImGui.ImVec2 x y) = (SDL.V2 x y)
                sdl2r :: SDL.V2 Float -> R.V2 Float
                sdl2r (SDL.V2 x y) = (R.V2 x y)
            let rmpos :: SDL.Point SDL.V2 Int32 -> R.V2 Float
                rmpos (SDL.P v2) = sdl2r $ mpos / (SDL.V2 (sizeW-2) (sizeH-2))
                    where
                        mpos :: SDL.V2 Float
                        mpos = fmap fromIntegral v2 - imvecToV2 (childwindowpos <> contentpos <> ImGui.ImVec2 1 1)

            let mclicks = fmap rmpos <$> mouseClicks model :: [(Bool,R.V2 Float)]

            do
                hivegame<-hivegame<$>readIORef ioref_model
                img <- Hive.hiveImage hivegame (round sizeW-2) (round sizeH-2) (rmpos $ md_pos (mousedata model)) mclicks
                tex <- Canvas.toTexture (tex1 model) img
                atomicModifyIORef ioref_model \m -> (,()) $ m{tex1=Just tex}

            model <- readIORef ioref_model
            let Just (Tex user_texture_id (fromIntegral->width) (fromIntegral->height)) = tex1 model
            let openGLtextureID = Foreign.intPtrToPtr $ fromIntegral $ user_texture_id


            --Foreign.with size \sizePtr ->
            Foreign.with (ImGui.ImVec2 (sizeW-2) (sizeH-2)) \sizePtr ->
                Foreign.with (ImGui.ImVec2 0 0) \uv0Ptr ->
                  Foreign.with (ImGui.ImVec2 1 1) \uv1Ptr ->
                    Foreign.with (ImGui.ImVec4 1 1 1 1) \tintColPtr -> -- mask
                      Foreign.with (ImGui.ImVec4 1 1 0 1) \borderColPtr -> do -- yellow border
                        ImGui.image openGLtextureID sizePtr uv0Ptr uv1Ptr tintColPtr borderColPtr
                        --ImGui_RAW.imageButton openGLtextureID sizePtr uv0Ptr uv1Ptr (-1) borderColPtr tintColPtr
                        return ()

            (drawlist::ImGui_DL.DrawList)<-ImGui.getWindowDrawList
            let addRect lt rb color thickness = do
                            let rounding = 0
                            Foreign.with lt \ptr_lt -> Foreign.with rb \ptr_rb ->
                                ImGui_DL.addRect drawlist ptr_lt ptr_rb color rounding ImGui_RAW.ImDrawFlags_RoundCornersNone thickness
            let addCircleFilled center radius color = do
                            let num_segments = 0
                            Foreign.with center \ptr_center ->
                                ImGui_DL.addCircleFilled drawlist ptr_center radius color num_segments

            addRect (childwindowpos <> contentpos) (childwindowpos <> contentpos <> size) 0xffffffff 1
            let mp = case md_pos (mousedata model) of SDL.P (SDL.V2 x y) -> ImGui.ImVec2 (fromIntegral x) (fromIntegral y)
            addCircleFilled mp 16 0xffff0000






    let canvasSize = ImGui.ImVec2 256 256
    ImGui.setNextWindowContentSize $ (pure @IO canvasSize)
    ImGui.withWindowOpen "canvas" do
        (windowpos::ImGui.ImVec2)<-ImGui.getWindowPos
        (windowsize::ImGui.ImVec2)<-ImGui.getWindowSize
        (contentpos::ImGui.ImVec2)<-ImGui.getCursorPos
        (drawlist::ImGui_DL.DrawList)<-ImGui.getWindowDrawList
        --(drawlist::ImGui_DL.DrawList)<-ImGui.getForegroundDrawList


        let addCircle center radius color thickness = do
                        let num_segments = 0
                        Foreign.with center \ptr_center ->
                            ImGui_DL.addCircle drawlist ptr_center radius color num_segments thickness
        let addCircleFilled center radius color = do
                        let num_segments = 0
                        Foreign.with center \ptr_center ->
                            ImGui_DL.addCircleFilled drawlist ptr_center radius color num_segments
        let addRect lt rb color thickness = do
                        let rounding = 0
                        Foreign.with lt \ptr_lt -> Foreign.with rb \ptr_rb ->
                            ImGui_DL.addRect drawlist ptr_lt ptr_rb color rounding ImGui_RAW.ImDrawFlags_RoundCornersNone thickness


        addRect (windowpos <> contentpos <> ImGui.ImVec2 (-1) (-1)) (windowpos <> contentpos <> canvasSize) 0xffffffff 1



        model <- readIORef ioref_model
        let mp = case md_pos (mousedata model) of SDL.P (SDL.V2 x y) -> ImGui.ImVec2 (fromIntegral x) (fromIntegral y)
        addCircleFilled mp 16 0xffff0000

        return ()


    ImGui.withWindowOpen "mouseevts" do
        traverse_ (ImGui.text . Text.pack . show) mouseevts

    -- Build the GUI
    ImGui.withWindowOpen "Hello, ImGui!" do
        -- Add a text widget
        ImGui.text . Text.pack . show =<< ImGui.getCursorPos
        ImGui.text "Hello, ImGui!"
        f <- model_font <$> readIORef ioref_model
        --ImGui.withFont f $
        ImGui.text "Hello, ant! 🐜"

        ImGui.text . Text.pack . show =<< ImGui.getWindowPos

        -- Add a button widget, and call 'putStrLn' when it's clicked
        ImGui.button "Clickety Click" >>= \case
            False -> return ()
            True  -> putStrLn "Ow!"

        let columncount = 3
        (Model{tabledata, mousedata=md}) <- readIORef ioref_model
        let f = List.foldl1' (.|.) [ ImGui.ImGuiTableFlags_SizingStretchSame, ImGui.ImGuiTableFlags_Resizable
                                   , ImGui.ImGuiTableFlags_Reorderable
                                   , ImGui.ImGuiTableFlags_Hideable
                                   , ImGui.ImGuiTableFlags_Sortable, ImGui.ImGuiTableFlags_SortMulti
                                   ]

        ImGui.withTable (ImGui.defTableOptions{ImGui.tableFlags=f}) "table" columncount $ \isVisible -> do

            ImGui.tableSetupColumnWith (ImGui.defTableColumnOptions{ImGui.tableColumnInitWidthOrWeight=3}) "AAA"
            ImGui.tableSetupColumnWith (ImGui.defTableColumnOptions{ImGui.tableColumnInitWidthOrWeight=2}) "BB"
            ImGui.tableSetupColumnWith (ImGui.defTableColumnOptions{ImGui.tableColumnInitWidthOrWeight=1}) "C"
            ImGui.tableHeadersRow

            ImGui.withSortableTable \isDirty sortSpecs -> do
                when isDirty $ do
                    atomicModifyIORef tabledata $ (,()) . \td -> List.foldr sortBySortSpec td sortSpecs

            let showRow (a,b,c) = do
                    ImGui.tableNextRow
                    ImGui.tableNextColumn $ ImGui.text a
                    ImGui.tableNextColumn $ ImGui.text b
                    ImGui.tableNextColumn $ ImGui.text c

            readIORef tabledata >>= traverse_ showRow


    -- Show the ImGui demo window
    ImGui.showDemoWindow

    -- Render
    GL.glClear GL.GL_COLOR_BUFFER_BIT

    ImGui.render
    ImGui_OGL2.openGL2RenderDrawData =<< ImGui.getDrawData

    SDL.glSwapWindow window

    mainLoop window ioref_model

    where
    -- Process the event loop

    unlessQuit action = do
        evts <- ImGui_SDL.pollEventsWithImGui

        let modifyMousedata f = atomicModifyIORef ioref_model $ (,()) . \m -> m{mousedata = f (mousedata m)}
        modifyMousedata \m->m{md_events=[]}
        -- print $ SDL.eventTimestamp <$> evts -- evts are ascending in timestamp
        forM_ evts \case
            SDL.Event{eventTimestamp,eventPayload=SDL.MouseMotionEvent e,..} -> modifyMousedata \m->
                let me = Mouseevent { me_pos = (SDL.mouseMotionEventPos e)
                                    , me_timestamp = eventTimestamp
                                    , me_type = MouseeventMove (SDL.mouseMotionEventRelMotion e)
                                    }
                 in m{md_pos = (SDL.mouseMotionEventPos e),md_events=me:md_events m}
            SDL.Event{eventTimestamp,eventPayload=SDL.MouseButtonEvent e} -> modifyMousedata \m->
                let click = case SDL.mouseButtonEventMotion e of
                                SDL.Pressed -> (MouseeventKeyPress,Set.insert)
                                SDL.Released -> (MouseeventKeyRelease,Set.delete)
                    but = (fromIntegral . SDL_N.toNumber $ SDL.mouseButtonEventButton e)
                    me = Mouseevent { me_pos = (SDL.mouseButtonEventPos e)
                                    , me_timestamp = eventTimestamp
                                    , me_type = (fst click) but (SDL.mouseButtonEventClicks e)
                                    }
                 in m{md_pos = (SDL.mouseButtonEventPos e),md_keys=(snd click) but (md_keys m),md_events=me:md_events m}
            SDL.Event{eventTimestamp,eventPayload=SDL.MouseWheelEvent e} -> modifyMousedata \m->
                let me = Mouseevent { me_pos = (md_pos m)
                                    , me_timestamp = eventTimestamp
                                    , me_type = MouseeventWheel (SDL.mouseWheelEventPos e) (SDL.mouseWheelEventDirection e)
                                    }
                 in m{md_events=me:md_events m}
            _ -> return ()
        modifyMousedata \m->m{md_events=List.reverse $ md_events m}

        let mouseevts = evts >>= \ev -> case SDL.eventPayload ev of
                                            SDL.MouseMotionEvent e -> [ev]
                                            SDL.MouseButtonEvent e -> [ev]
                                            SDL.MouseWheelEvent e -> [ev]
                                            _ -> []
        let shouldQuit = Foldable.any isQuit evts
        if shouldQuit then pure () else action mouseevts


    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent


