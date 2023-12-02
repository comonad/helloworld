{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language NamedFieldPuns #-}
{-# language RecordWildCards #-}


module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import Control.Monad
import qualified DearImGui as ImGui
import qualified DearImGui.Raw as ImGui_RAW
import qualified DearImGui.Raw.DrawList as ImGui_DL
import qualified DearImGui.OpenGL2 as ImGui_OGL2 (openGL2Init,openGL2Shutdown,openGL2NewFrame,openGL2RenderDrawData)
import qualified DearImGui.SDL.OpenGL as ImGui_SDL_OGL (sdl2InitForOpenGL)
import qualified DearImGui.SDL as ImGui_SDL (sdl2Shutdown,sdl2NewFrame,pollEventsWithImGui)
import qualified Graphics.GL as GL
import qualified SDL
import qualified SDL.Internal.Numbered as SDL_N
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


main :: IO ()
main = do
    -- Initialize SDL
    SDL.initializeAll
    model <- initModel
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

        -- Initialize ImGui's SDL2 backend
        _ <- managed_ $ bracket_ (ImGui_SDL_OGL.sdl2InitForOpenGL window glContext) ImGui_SDL.sdl2Shutdown

        -- Initialize ImGui's OpenGL backend
        _ <- managed_ $ bracket_ ImGui_OGL2.openGL2Init ImGui_OGL2.openGL2Shutdown

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
    , md_events :: ![Mouseevent]
    }

data Model = Model {tabledata :: IORef Tabledata, mousedata::Mousedata}
initModel :: IO (IORef Model)
initModel = do
    tabledata <- newIORef [("1","2","3"), ("4","5","6"), ("4","3","1")]
    let md = Mousedata (SDL.P $ SDL.V2 0 0) Set.empty []
    newIORef $ Model tabledata md

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

mainLoop :: SDL.Window -> IORef Model -> IO ()
mainLoop window ioref_model = unlessQuit $ \mouseevts -> do
    -- Tell ImGui we're starting a new frame
    ImGui_OGL2.openGL2NewFrame
    ImGui_SDL.sdl2NewFrame
    ImGui.newFrame



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
        ImGui.text . Text.pack . show =<< ImGui.getWindowPos

        -- Add a button widget, and call 'putStrLn' when it's clicked
        ImGui.button "Clickety Click" >>= \case
            False -> return ()
            True  -> putStrLn "Ow!"

        let columncount = 3
        (Model tabledata md) <- readIORef ioref_model
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
        forM_ (List.reverse evts) \case
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

        let mouseevts = evts >>= \ev -> case SDL.eventPayload ev of
                                            SDL.MouseMotionEvent e -> [ev]
                                            SDL.MouseButtonEvent e -> [ev]
                                            SDL.MouseWheelEvent e -> [ev]
                                            _ -> []
        let shouldQuit = Foldable.any isQuit evts
        if shouldQuit then pure () else action mouseevts


    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent


