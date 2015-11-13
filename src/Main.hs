{-# LANGUAGE TemplateHaskell #-}

module Main where

import Sub
import Utilities

import Control.Lens
import Graphics.Rendering.OpenGL
import Data.IORef
import Reactive.Banana.Frameworks
import Reactive.Banana
import System.IO
import Graphics.GLUtil
import qualified Graphics.UI.GLFW as GraphicsUI
import System.Exit
import Control.Monad

--datatype representing Game state
data GameState = GameOver | GameRunning


                              
--datatype representing the state of the objects in the game
data StateDef = StateDef {  _text     :: AllTextures
                         , _state     :: GameState
                         , _stud      :: AnimeObject
                         , _prof      :: [AnimeObject]
                         , _ground    :: AnimeObject
                         , _skyline   :: AnimeObject
                         , _roadblock :: [AnimeObject]
                         }

--datatype representing all the textures
data AllTextures = AllTextures { _textureStudent1  :: TextureObject
                               , _textureStudent2  :: TextureObject
                               , _textureStudent3  :: TextureObject
                               , _textureStudent4  :: TextureObject
                               , _textureprof1     :: TextureObject
                               , _textureprof2     :: TextureObject
                               , _textureprof3     :: TextureObject
                               , _textureprof4     :: TextureObject
                               , _textureskyline   :: TextureObject
                               , _texground        :: TextureObject
                               , _textureroadblock :: TextureObject
                               , _gameOver         :: TextureObject
                               }


makeLenses ''StateDef
makeLenses ''AllTextures



main :: IO ()
main = do
    flag <- GraphicsUI.init
    --initializing the opengl graphics
    let failureInit = not flag
    if failureInit
        then exitFailure
        else do
        window <- GraphicsUI.createWindow 540 520 "Where's my Assignment!" Nothing Nothing
        --creates a window of the specified preferences
        case window of 
           Nothing -> (GraphicsUI.terminate >> exitFailure)
           Just window1 -> do
                   GraphicsUI.makeContextCurrent window
                   statesIORef <- initializeFn
                   --using newAddHandler to create an event that I can fire from outside the event network
                   (addHandler, fire) <- newAddHandler
                   -- "fire" is an IO action that takes a value and fires an event with that value corresponding to the context
                   network <- initNetworkFn statesIORef addHandler
                   actuate network


                   blendFunc $= (DstAlpha, OneMinusSrcAlpha)
                   blend $= Enabled
                   texture Texture2D $= Enabled
                   clearColor $= Color4 0.4045865 0.77 0.8058625 1.5
                   normalize $= Enabled
                   shadeModel $= Smooth
                   mainLoop fire window1 statesIORef

                   GraphicsUI.destroyWindow window1
                   --type of window created is GraphicsUI.Window
                   GraphicsUI.terminate
                   exitSuccess



-- initializes all the textures from their respective files
initializFn :: IO AllTextures
initializFn = do
    --IO is a functor and an applictive functor
    let textureStudent1  = loadTextureFn "../res/stud-01.png"
    --return type is IO a
    let textureStudent2  = loadTextureFn "../res/stud-02.png"
    let textureStudent3  = loadTextureFn "../res/stud-03.png"
    let textureStudent4  = loadTextureFn "../res/stud-04.png"
    let textureprof1     = loadTextureFn "../res/prof-01.png"
    let textureprof2     = loadTextureFn "../res/prof-02.png"
    let textureprof3     = loadTextureFn "../res/prof-03.png"
    let textureprof4     = loadTextureFn "../res/prof-04.png"
    let texturesky       = loadTextureFn "../res/skyline.png"
    let texground        = loadTextureFn "../res/ground.png"
    let textureroadblock = loadTextureFn "../res/roadblock.png"
    let gameOver         = loadTextureFn "../res/gameOverWallpaper.png"
    AllTextures <$> textureStudent1 <*> textureStudent2 <*> textureStudent3 <*> textureStudent4 <*> textureprof1 <*> textureprof2 <*> textureprof3 <*> textureprof4 <*>  texturesky <*> texground <*> textureroadblock <*> gameOver
    

initializeFn :: IO (IORef StateDef) 
initializeFn = do 
    var <- initializFn 
    newIORef StateDef { _text    = var
                      , _state   = GameRunning
                      , _stud  = AnimeObject {
                                                _textur   = var ^. textureStudent1 -- using lenses as getters
                                               ,_positio  = Vertex2 80 400 
                                               ,_speed    = Vector2 0  0
                                               ,_size     = Vector2 66 84
                               } 
                      , _prof    =   []
                      , _ground  = AnimeObject {
                                                _textur   = var ^. texground
                                               ,_positio  = Vertex2 0 0
                                               ,_speed    = Vector2 (-0.65) 0
                                               ,_size     = Vector2 336 112
                               }
                      , _skyline = AnimeObject {
                                                _textur   = var ^. textureskyline
                                               ,_positio  = Vertex2 0 112
                                               ,_speed    = Vector2 (-0.31) 0
                                               ,_size     = Vector2 700 350
                               }
                      , _roadblock = []
                      }

initNetworkFn :: IORef StateDef -> AddHandler GraphicsUI.KeyState -> IO EventNetwork
initNetworkFn statesIORef addHandler = compile $ do
            eventStream <- fromAddHandler addHandler -- we get an event stream to work on
            
            --accumE apples the fn on the event stream inside the Moment context
            let eventCount = accumE 0 $ (\_ n -> succ n ) <$> eventStream
            --Used to count the number of events occured
            
            let keyStateValue = accumE GraphicsUI.KeyState'Released $ keyMapFn <$> eventStream
            --Used to keep track of KeyState value during event stream occurence using keyMapFn function 
            
            let animateStud n = do
                                  currentState <- readIORef statesIORef
                                  let value = mod (div n 83 ) 4
                                  let student1 = animateFn $ currentState ^. stud
                                  let student2 = case value of
                                                  0 -> currentState ^. stud & textur .~ (currentState ^. text.textureStudent1)
                                                  1 -> currentState ^. stud & textur .~ (currentState ^. text.textureStudent2)
                                                  2 -> currentState ^. stud & textur .~ (currentState ^. text.textureStudent3)
                                                  3 -> currentState ^. stud & textur .~ (currentState ^. text.textureStudent4)
                                  writeIORef statesIORef (currentState & stud .~ student2)


            
            
            let animateGround _ = do
                                  currentState <- readIORef statesIORef
                                  let ground1  = animateFn $ currentState ^. ground
                                  let ground2 = if (ground1 ^. positio . x) < negate (ground1 ^. size . x)
                                                  then ground1 & positio . x +~ (ground1 ^. size . x)
                                                  else ground1
                                  writeIORef statesIORef (currentState & ground .~ ground2)


            let animateSky _ = do
                                  currentState <- readIORef statesIORef
                                  let sky1  = animateFn $ currentState ^. skyline
                                  let sky2 = if (sky1 ^. positio . x) < negate (sky1 ^. size . x)
                                                 then sky1 & positio . x +~ (sky1 ^. size . x)
                                                  else sky1
                                  writeIORef statesIORef (currentState & skyline .~ sky2)

            let moveProf n = do
                                  currentState <- readIORef statesIORef
                                  let prof1 = map animateFn $ currentState ^. prof
                                  let prof2 = filter (\ varr -> varr ^. positio . x > - 80) prof1
                                  professor <- if(mod n 2986 /= 0)
                                                  then return prof2
                                                  else do
                                                      let prof3 = AnimeObject {
                                                                                _textur   = currentState ^. text . textureprof1
                                                                               ,_positio = Vertex2 640     110
                                                                               ,_speed    = Vector2 (-0.65) 0 
                                                                               ,_size     = Vector2 50      79
                                                                              }
                                                      return $ prof3 : prof2
                                  writeIORef statesIORef (currentState & prof .~ professor)





            let animateRoadblock n = do
                                  currentState <- readIORef statesIORef
                                  let block1 = map animateFn $ currentState ^. roadblock
                                  let block2 = filter (\ varr -> varr ^. positio . x > - 80) block1
                                  --let flag = (mod n 1200 == 0)
                                  block <- if(mod n 1215 /= 0)
                                                  then return block2
                                                  else do
                                                      let block3 = AnimeObject {
                                                                                 _textur   = currentState ^. text . textureroadblock
                                                                               , _positio  = Vertex2 640     110
                                                                               , _speed    = Vector2 (-0.65) 0 
                                                                               , _size     = Vector2 65      90
                                                                               }
                                                      return $ block3 : block2
                                  writeIORef statesIORef (currentState & roadblock .~ block)


            
            let jumpStudent keyStateValue = do
                                  currentState <- readIORef statesIORef
                                  let stud1 = animateFn $ if (keyStateValue == GraphicsUI.KeyState'Pressed && currentState ^. stud. positio . y <= 113)-- || keyState == KeyRepeating)
                                                    then currentState ^. stud & speed . y .~ 1.9
                                                    else currentState ^. stud & speed . y -~ 0.012
                                  let stud2 = stud1 & positio . y %~ (max 112 . min (480 - (stud1 ^. size . y)))--setting wid appln of fn,keeping bird within screen vertical bounds
                                  writeIORef statesIORef (currentState & stud .~ stud2)


            let contactTest _ = do
                                  currentState <- readIORef statesIORef
                                  let student   = currentState ^. stud
                                  let professor = currentState ^. prof
                                  let rblock = currentState ^. roadblock
                                  if (any (\x -> x==True) (map (contactFn student) professor) || any (\x -> x==True) (map (contactFn student) rblock))
                                      then writeIORef statesIORef (currentState & state .~ GameOver)
                                      else return ()

            --fromAddHandler :: AddHandler a -> NetworkDescription t (Event t a)
--    To handle output events use reactimate, pass it an event stream containing...
-- ...IO actions and whenever an event from that stream occurs the action is executed. 

            reactimate $ fmap animateStud eventCount
            reactimate $ fmap animateGround eventStream
            reactimate $ fmap animateSky eventStream
            reactimate $ fmap animateRoadblock eventCount
            reactimate $ fmap jumpStudent keyStateValue
            reactimate $ fmap contactTest eventStream
            reactimate $ fmap moveProf eventCount




mainLoop :: (Handler GraphicsUI.KeyState) -> GraphicsUI.Window -> IORef StateDef -> IO ()
mainLoop fire window statesIORef = do
    flag <- (GraphicsUI.windowShouldClose window)
    unless flag $ do
        (width, height) <- GraphicsUI.getFramebufferSize window
        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
        clear [ColorBuffer]
        
        (GraphicsUI.getKey window GraphicsUI.Key'Space) >>= fire 
        --fires an event, when SpaceBar key is pressed , sends KeyPressed

        currentState <- readIORef statesIORef
        let var = currentState ^. state
        case var of
            GameOver -> do
                  renderFn (width, height) (currentState ^. skyline & textur .~ (currentState ^. text . gameOver) & (size . x .~ 540) & (size . y .~ 520) & (speed . x .~ 0) & (positio . x .~ -20) & (positio . y .~ -20))


            GameRunning -> do
                  renderFn (width, height) (currentState ^. skyline)
                  renderFn (width, height) (currentState ^. skyline & positio . x +~ (currentState ^. skyline . size .x))
                  renderFn (width, height) (currentState ^. skyline & positio . x +~ (2 * currentState ^. skyline . size . x))
                  renderFn (width, height) (currentState ^. skyline & positio . x +~ (3 * currentState ^. skyline. size . x))
                  renderFn (width, height) (currentState ^. ground)
                  renderFn (width, height) (currentState ^. ground & positio . x +~ (currentState ^. ground . size .x))
                  renderFn (width, height) (currentState ^. ground & positio . x +~ (2 * currentState ^. ground . size . x))
                  renderFn (width, height) (currentState ^. ground & positio . x +~ (3 * currentState ^. ground. size . x))
                  renderFn (width, height) (currentState ^. stud)
                  mapM_ (renderFn (width, height)) (currentState ^. prof)
                  mapM_ (renderFn (width, height)) (currentState ^. roadblock)

                  --renderFn (currentState ^. ground)
                  --renderFn (currentState ^. ground)

           


        GraphicsUI.swapBuffers window
        GraphicsUI.pollEvents
        mainLoop fire window statesIORef