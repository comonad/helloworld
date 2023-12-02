{-# language PackageImports #-}
{-# language BlockArguments #-}

module Canvas (
    JuicyPixels.DynamicImage(..),
    Tex(..),
    toTexture,toTextureI,
    toDynamicImage
) where

import "JuicyPixels" Codec.Picture as JuicyPixels

import Data.Vector.Storable (unsafeWith)
import Foreign.Storable (Storable)
import Data.Typeable
import Data.List as List
import Control.Monad
import Control.Applicative
import qualified "OpenGLRaw" Graphics.GL as GL
import qualified "OpenGLRaw" Graphics.GL.Groups as GL
import qualified "OpenGLRaw" Graphics.GL.Types as GL (GLuint)
import qualified "OpenGLRaw" Graphics.GL.Functions as GL

import Foreign

data Tex = Tex GL.GLuint Int Int

toTexture :: JuicyPixels.DynamicImage -> IO Tex
toTexture di = do

    t <- alloca \ptr -> do
        GL.glGenTextures 1 ptr
        peek ptr

    GL.glEnable GL.GL_TEXTURE_2D
    GL.glBindTexture GL.GL_TEXTURE_2D t
    -- https://hackage.haskell.org/package/OpenGLRaw-3.3.4.1/docs/Graphics-GL-Functions.html#v:glTexParameteri
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER $ fromIntegral GL.GL_LINEAR
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER $ fromIntegral GL.GL_LINEAR
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S $ fromIntegral GL.GL_REPEAT
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T $ fromIntegral GL.GL_REPEAT

    let mipmapLevel = 0 :: GL.GLint
        border = 0 :: GL.GLint

    let gen :: Storable (PixelBaseComponent a) => Image a -> GL.GLenum -> GL.GLenum -> GL.GLenum -> IO Tex
        gen (Image width height dat)
            (pif :: GL.GLenum) -- https://hackage.haskell.org/package/OpenGLRaw-3.3.4.1/docs/Graphics-GL-Groups.html#InternalFormat
            (pixelformat :: GL.GLenum) -- https://hackage.haskell.org/package/OpenGLRaw-3.3.4.1/docs/Graphics-GL-Groups.html#PixelFormat
            (pixeltype :: GL.GLenum) -- https://hackage.haskell.org/package/OpenGLRaw-3.3.4.1/docs/Graphics-GL-Groups.html#PixelType
            = do
            unsafeWith dat $ \ptr ->
                    GL.glTexImage2D -- https://hackage.haskell.org/package/OpenGLRaw-3.3.4.1/docs/Graphics-GL-Functions.html#v:glTexImage2D
                        GL.GL_TEXTURE_2D
                        mipmapLevel
                        (fromIntegral GL.GL_RGBA)
                        (fromIntegral width :: GL.GLsizei) (fromIntegral height :: GL.GLsizei)
                        border
                        pixelformat
                        pixeltype
                        (castPtr ptr)
            return $ Tex t width height

    case di of
        ImageY8   i -> gen i GL.GL_R8          GL.GL_LUMINANCE      GL.GL_UNSIGNED_BYTE
        ImageY16  i -> gen i GL.GL_R16         GL.GL_LUMINANCE      GL.GL_UNSIGNED_SHORT
        ImageY32  i -> gen i GL.GL_R32I        GL.GL_LUMINANCE      GL.GL_UNSIGNED_INT
        ImageYF   i -> gen i GL.GL_R32F        GL.GL_LUMINANCE      GL.GL_FLOAT
        ImageYA8  i -> gen i GL.GL_LUMINANCE8_ALPHA8   GL.GL_LUMINANCE_ALPHA GL.GL_UNSIGNED_BYTE
        ImageYA16 i -> gen i GL.GL_LUMINANCE16_ALPHA16 GL.GL_LUMINANCE_ALPHA GL.GL_UNSIGNED_SHORT
        ImageRGB8   i -> gen i GL.GL_RGB8   GL.GL_RGB  GL.GL_UNSIGNED_BYTE
        ImageRGB16  i -> gen i GL.GL_RGB16  GL.GL_RGB  GL.GL_UNSIGNED_SHORT
        ImageRGBF   i -> gen i GL.GL_RGB32F GL.GL_RGB  GL.GL_FLOAT
        ImageRGBA8  i -> gen i GL.GL_RGBA8  GL.GL_RGBA GL.GL_UNSIGNED_BYTE
        ImageRGBA16 i -> gen i GL.GL_RGBA16 GL.GL_RGBA GL.GL_UNSIGNED_SHORT
        ImageYCbCr8 _ -> gen (convertRGB8  di) GL.GL_RGB8  GL.GL_RGB GL.GL_UNSIGNED_BYTE
        ImageCMYK8  _ -> gen (convertRGB8  di) GL.GL_RGB8  GL.GL_RGB GL.GL_UNSIGNED_BYTE
        ImageCMYK16 _ -> gen (convertRGB16 di) GL.GL_RGB16 GL.GL_RGB GL.GL_UNSIGNED_SHORT

toTextureI :: Typeable a => JuicyPixels.Image a -> IO Tex
toTextureI = toTexture . toDynamicImage


toDynamicImage :: Typeable a => JuicyPixels.Image a -> JuicyPixels.DynamicImage
toDynamicImage i = di
    where
        Just di = List.foldl1' (<|>) xs
        xs =    [ ImageY8      <$> cast i
                , ImageY16     <$> cast i
                , ImageY32     <$> cast i
                , ImageYF      <$> cast i
                , ImageYA8     <$> cast i
                , ImageYA16    <$> cast i
                , ImageRGB8    <$> cast i
                , ImageRGB16   <$> cast i
                , ImageRGBF    <$> cast i
                , ImageRGBA8   <$> cast i
                , ImageRGBA16  <$> cast i
                , ImageYCbCr8  <$> cast i
                , ImageCMYK8   <$> cast i
                , ImageCMYK16  <$> cast i
                ]



