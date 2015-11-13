{-# LANGUAGE TemplateHaskell #-}

module Utilities (
       contactFn
     , keyMapFn
     , animateFn
     , loadTextureFn
     , renderFn

)where

import Sub
import Graphics.GLUtil
import Control.Lens
import Graphics.Rendering.OpenGL
import Data.IORef
import System.IO
import Reactive.Banana.Frameworks
import Reactive.Banana
import qualified Graphics.UI.GLFW as GraphicsUI


keyMapFn :: GraphicsUI.KeyState -> GraphicsUI.KeyState -> GraphicsUI.KeyState
keyMapFn GraphicsUI.KeyState'Pressed  GraphicsUI.KeyState'Released  = GraphicsUI.KeyState'Pressed
keyMapFn GraphicsUI.KeyState'Pressed  GraphicsUI.KeyState'Pressed   = GraphicsUI.KeyState'Repeating
keyMapFn GraphicsUI.KeyState'Pressed  GraphicsUI.KeyState'Repeating = GraphicsUI.KeyState'Repeating
keyMapFn _                      _                       = GraphicsUI.KeyState'Released


contactFn :: AnimeObject -> AnimeObject -> Bool
contactFn object1 object2 = not $ xmax1 < (xmin2 + 30) || (xmin1 + 30) > xmax2 || ymax1 < (ymin2 + 30) || (ymin1 + 30 ) > ymax2
                       where (xmin1 , ymin1) = (object1 ^. positio . x , object1 ^. positio . y)
                             (xmax1 , ymax1) = (xmin1 + (object1 ^. size . x) , ymin1 + (object1 ^. size . y))
                             (xmin2 , ymin2) = (object2 ^. positio . x , object2 ^. positio . y)
                             (xmax2 , ymax2) = (xmin2 + (object2 ^. size . x) , ymin2 + (object2 ^. size . y)) 


animateFn :: AnimeObject -> AnimeObject
animateFn object = object2 
        where object1 = object & positio . x +~ (object ^. speed . x)
              object2 = object1 & positio . y +~ (object1 ^. speed . y)


loadTextureFn :: String -> IO TextureObject
loadTextureFn path = do
    object <- either error id <$> readTexture path
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Mirrored, ClampToEdge)
    return object


renderFn :: (Int , Int) -> AnimeObject -> IO ()
renderFn (width , height) object = do
    let xmax = fromIntegral width / 2.25
    let ymax = fromIntegral height / 2.25
    let (Vertex2 ver1 ver2) = object ^. positio
    let (Vector2 vec1 vec2) = object ^. size
    textureBinding Texture2D $= Just (object ^. textur)
    
    preservingMatrix . renderPrimitive Quads $ do
        nor 0 0 1
        tex 0 0 
        ver (ver1 / xmax-1)      ((ver2 + vec2)/ ymax-1) 0
        tex 0 1
        ver (ver1 / xmax-1)      (ver2 / ymax-1)     0
        tex 1 1
        ver ((ver1 + vec1) / xmax-1) (ver2 / ymax-1)      0
        tex 1 0
        ver ((ver1 + vec1) / xmax-1) ((ver2 + vec2) / ymax-1)  0
    where
    ver x y z = vertex (Vertex3 x y z :: Vertex3 GLfloat)
    nor x y z = normal (Normal3 x y z :: Normal3 GLfloat)
    tex x y   = texCoord (TexCoord2 x y :: TexCoord2 GLfloat)
    