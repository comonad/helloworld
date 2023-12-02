{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import Control.Monad
import qualified DearImGui as ImGui
import qualified DearImGui.OpenGL2 as ImGui_OGL2 (openGL2Init,openGL2Shutdown,openGL2NewFrame,openGL2RenderDrawData)
import qualified DearImGui.SDL.OpenGL as ImGui_SDL_OGL (sdl2InitForOpenGL)
import qualified DearImGui.SDL as ImGui_SDL (sdl2Shutdown,sdl2NewFrame,pollEventWithImGui)
import qualified Graphics.GL as GL
import qualified SDL
import Data.Bits
import Data.List as List
import Data.IORef
import Data.Traversable
import Data.Foldable
import Data.Function
import Data.Text



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
data Model = Model {tabledata :: IORef Tabledata}
initModel :: IO (IORef Model)
initModel = do
    tabledata <- newIORef [("1","2","3"), ("4","5","6"), ("4","3","1")]
    newIORef $ Model tabledata

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

mainLoop :: SDL.Window -> IORef Model -> IO ()
mainLoop window model = unlessQuit do
  -- Tell ImGui we're starting a new frame
  ImGui_OGL2.openGL2NewFrame
  ImGui_SDL.sdl2NewFrame
  ImGui.newFrame

  -- Build the GUI
  ImGui.withWindowOpen "Hello, ImGui!" do
    -- Add a text widget
    ImGui.text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    ImGui.button "Clickety Click" >>= \case
      False -> return ()
      True  -> putStrLn "Ow!"

    let columncount = 3
    (Model tabledata) <- readIORef model
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

  mainLoop window model

  where
    -- Process the event loop
    unlessQuit action = do
      shouldQuit <- checkEvents
      if shouldQuit then pure () else action

    checkEvents = do
      ImGui_SDL.pollEventWithImGui >>= \case
        Nothing ->
          return False
        Just event ->
          (isQuit event ||) <$> checkEvents

    isQuit event =
      SDL.eventPayload event == SDL.QuitEvent


