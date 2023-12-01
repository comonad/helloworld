{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import qualified DearImGui as ImGui
import qualified DearImGui.OpenGL2 as ImGui_OGL2 (openGL2Init,openGL2Shutdown,openGL2NewFrame,openGL2RenderDrawData)
import qualified DearImGui.SDL.OpenGL as ImGui_SDL_OGL (sdl2InitForOpenGL)
import qualified DearImGui.SDL as ImGui_SDL (sdl2Shutdown,sdl2NewFrame,pollEventWithImGui)
import qualified Graphics.GL as GL
import qualified SDL

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

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (ImGui_SDL_OGL.sdl2InitForOpenGL window glContext) ImGui_SDL.sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ ImGui_OGL2.openGL2Init ImGui_OGL2.openGL2Shutdown

    liftIO $ mainLoop window


mainLoop :: SDL.Window -> IO ()
mainLoop window = unlessQuit do
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

  -- Show the ImGui demo window
  ImGui.showDemoWindow

  -- Render
  GL.glClear GL.GL_COLOR_BUFFER_BIT

  ImGui.render
  ImGui_OGL2.openGL2RenderDrawData =<< ImGui.getDrawData

  SDL.glSwapWindow window

  mainLoop window

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


