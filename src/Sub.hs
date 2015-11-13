{-# LANGUAGE TemplateHaskell #-}

module Sub (
         AnimeObject(..)
       , textur
       , positio
       , speed
       , size
       , x
       , y
     
)where

import Graphics.Rendering.OpenGL
import Control.Lens


-- datatype representing an animated object
data AnimeObject = AnimeObject {
                                _textur   :: TextureObject
                               ,_positio  :: Vertex2 GLfloat
                               ,_speed    :: Vector2 GLfloat
                               ,_size     :: Vector2 GLfloat
                               }


makeLenses ''AnimeObject

--lenses for x and y co-ordinates of Vertex2 and Vector2 datatypes

class XCoord v where
    x :: Lens (v a) (v a) a a

class YCoord type1 where
    y :: Lens (type1 a) (type1 a) a a

instance XCoord Vertex2 where
    x = lens (\(Vertex2 x _) -> x) (\(Vertex2 _ y) x1 -> Vertex2 x1 y) -- getter and setter

instance XCoord Vector2 where
    x = lens (\(Vector2 x _) -> x) (\(Vector2 _ y) x1 -> Vector2 x1 y) -- getter and setter

instance YCoord Vertex2 where
    y = lens (\(Vertex2 _ y) -> y) (\(Vertex2 x _) y1 -> Vertex2 x y1) -- getter and setter

instance YCoord Vector2 where
    y = lens (\(Vector2 _ y) -> y) (\(Vector2 x _) y1 -> Vector2 x y1) -- getter and setter