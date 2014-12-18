{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2014 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
--
----------------------------------------------------------------------------

module Photon.Interface.Game.Run (
    -- * Running games
    runGame
  ) where

import Control.Applicative
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TVar ( TVar, modifyTVar, newTVarIO, readTVar
                                   , writeTVar )
import Control.Monad ( (=<<), forM_ )
import Control.Monad.Free ( Free(..) )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Trans.Either ( runEitherT )
import Control.Monad.Trans.Journal ( evalJournalT, sink )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Data.Foldable ( traverse_ )
import Data.Traversable ( traverse )
import Data.List ( intercalate )
import Graphics.UI.GLFW as GLFW
import Numeric.Natural ( Natural )
import Photon.Core.Entity ( Entity )
import Photon.Core.Light ( Light )
import Photon.Core.Loader ( Load(..) )
import Photon.Core.Material ( Material )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Projection ( Projection )
import Photon.Interface.Game.Command ( GameCmd, Game )
import qualified Photon.Interface.Game.Command as GC ( GameCmd(..) )
import Photon.Interface.Game.Event
import Photon.Interface.Game.Shaders ( lightVS, lightFS )
import Photon.Render.Camera ( GPUCamera(..), gpuCamera )
import Photon.Render.GL.Shader ( ShaderType(..), Uniform, Uniformable )
import Photon.Render.Light ( GPULight(..), gpuLight )
import Photon.Render.Material ( GPUMaterial(..), gpuMaterial )
import Photon.Render.Mesh ( GPUMesh(..), gpuMesh )
import Photon.Render.Shader ( GPUProgram(..), gpuProgram )
import Photon.Utils.Log ( Log(..), LogCommitter(..), LogType(..) )
import Photon.Utils.TimePoint
import Prelude ( Either(Either) )
import Prelude hiding ( Either(Left,Right) )

data GameDriver = GameDriver {
    drvRegisterMesh     :: Mesh -> IO GPUMesh
  , drvRegisterMaterial :: Material -> IO GPUMaterial
  , drvRegisterLight    :: Light -> IO GPULight
  , drvRegisterCamera   :: Projection -> Entity -> IO GPUCamera
  , drvLoadObject       :: (Load a) => String -> IO (Maybe a)
  , drvRenderMeshes     :: GPUMaterial -> [(GPUMesh,Entity)] -> IO ()
  , drvSwitchLightOn    :: GPULight -> Entity -> IO ()
  , drvLook             :: GPUCamera -> IO ()
  , drvLog              :: LogType -> String -> IO ()
  , drvTime             :: IO TimePoint
  }

-- |Helper function to show 'GLSL.Version' type, because they didn’t pick the
-- one from "Data.Version"…
showGLFWVersion :: Version -> String
showGLFWVersion (Version major minor rev) = intercalate "." $ map show [major,minor,rev]

-------------------------------------------------------------------------------
-- Run game

-- |Run a game session. This is the entry point of a photon-powered game. It
-- spawns a standalone window in windowed or fullscreen mode. If you want to
-- embed **photon** in a /GUI/ container, you shouldn’t use 'runGame'.
--
-- You’ll be asked for an event poller. This is optional; if you don’t want
-- any specific events, just use @return []@. If you do, you’ll be placed in
-- 'IO' so that you can do whatever you want, like socket-based communication
-- or anything 'IO'-related.
--
-- Nevertheless, **photon** does generate core events. You have to react to
-- them if you want your application to correctly behave. That’s done via an
-- /event handler/ which type is 'EventHandler u a', where 'u' is your event
-- type and 'a' your application.
--
-- The application runs in a special isolated monad, 'Game'. That type gives
-- you everything you need for game-development. Feel free to read the 'Game'
-- documentation for further understanding.
runGame :: Natural -- ^ Width of the window
        -> Natural -- ^ Height of the window
        -> Bool -- ^ Should the window be fullscreen?
        -> String -- ^ Title of the window
        -> IO [u] -- ^ User-spefic events poller
        -> EventHandler u a -- ^ Event handler
        -> (a -> Game a) -- ^ Your application logic
        -> a -- ^ Initial application
        -> IO ()
runGame w h fullscreen title pollUserEvents eventHandler step app = do
    initiated <- GLFW.init
    if initiated then do
      glfwVersion <- fmap showGLFWVersion getVersion
      print (Log InfoLog CoreLog $ "GLFW " ++ glfwVersion ++ " initialized!")
      windowHint (WindowHint'ContextVersionMajor 3)
      windowHint (WindowHint'ContextVersionMinor 3)
      createWindow (fromIntegral w) (fromIntegral h) title Nothing Nothing >>= \win -> case win of
        Just window -> makeContextCurrent win >> runWithWindow w h fullscreen window pollUserEvents eventHandler step app
        -- TODO: display OpenGL information
        Nothing -> print (Log ErrorLog CoreLog "unable to create window :(")
      print (Log InfoLog CoreLog "bye!")
      terminate
      else do
        print (Log ErrorLog CoreLog "unable to init :(")

runWithWindow :: Natural -> Natural -> Bool -> Window -> IO [u] -> EventHandler u a -> (a -> Game a) -> a -> IO ()
runWithWindow w h fullscreen window pollUserEvents eventHandler step initializedApp = do
    -- transaction variables
    events <- newTVarIO []
    mouseXY <- newTVarIO (0,0)

    -- callbacks
    setKeyCallback window (Just $ handleKey events)
    setMouseButtonCallback window (Just $ handleMouseButton events)
    setCursorPosCallback window (Just $ handleMouseMotion mouseXY events)
    setWindowCloseCallback window (Just $ handleWindowClose events)
    setWindowFocusCallback window (Just $ handleWindowFocus events)

    -- pre-process
    getCursorPos window >>= atomically . writeTVar mouseXY

    -- game
    gdrv <- gameDriver w h fullscreen
    case gdrv of
      Nothing -> print (Log ErrorLog CoreLog "unable to create game driver")
      Just drv -> run_ drv events initializedApp
  where
    run_ drv events app = do
      -- poll user events then GLFW ones and sink shared events
      userEvs <- fmap (map UserEvent) pollUserEvents
      GLFW.pollEvents
      evs <- fmap (userEvs++) . atomically $ readTVar events <* writeTVar events []
      -- rout events to game and interpret it; if it has to go on then simply loop
      traverse (interpretGame drv) (routeEvents (step app) evs) >>= maybe (return ()) (run_ drv events)
    routeEvents app evs = case evs of
      [] -> Just app
      (e:es) -> case eventHandler e of
        Just step' -> routeEvents (app >>= step') es
        Nothing -> Nothing

-------------------------------------------------------------------------------
-- Game interpreter

-- |This function generates the 'GameDriver'. It uses **OpenGL** to get all the
-- required functions. The width and the height of the window are required in
-- order to be able to generate framebuffers, textures or any kind of object
-- viewport-related.
--
-- If the window’s dimensions change, the game driver should be recreated.
gameDriver :: Natural -> Natural -> Bool -> IO (Maybe GameDriver)
gameDriver width height fullscreen = do
  gdrv <- runEitherT $ do
    -- create light program here
    lightProgram <- evalJournalT $
      gpuProgram [(VertexShader,lightVS),(FragmentShader,lightFS)] <* sink print
    let
      sem :: (Uniformable a) => String -> IO (Uniform a)
      sem = programSemantic lightProgram
    liftIO $ do
      -- map light program’s semantics here as well
      projViewU <- sem "projView"
      modelU <- sem "model"
      eyeU <- sem "eye"
      matDiffAlbU <- sem "matDiffAlb"
      matSpecAlbU <- sem "matSpecAlb"
      matShnU <- sem "matShn"
      ligPosU <- sem "ligPos"
      ligColU <- sem "ligCol"
      ligPowU <- sem "ligPow"
      ligRadU <- sem "ligRad"
      startTime <- timePoint
      return $
        GameDriver
          gpuMesh
          gpuMaterial
          gpuLight
          gpuCamera
          (\name -> runMaybeT . evalJournalT $ load name <* sink print)
          (\mat meshes -> do
              runMaterial mat matDiffAlbU matSpecAlbU matShnU
              forM_ meshes $ \(gpum,ent) -> renderMesh gpum modelU ent
            )
          (\gpul ent -> runLight gpul ligColU ligPowU ligRadU ligPosU ent)
          (\gpuc -> runCamera gpuc projViewU eyeU)
          (\lt msg -> print $ Log lt UserLog msg)
          (fmap (\t -> t - startTime) timePoint)
  either (\e -> print e >> return Nothing) (return . Just) gdrv

-- |Game interpreter. This function turns the pure 'Game a' structure into
-- 'IO a'.
interpretGame :: GameDriver -> Game a -> IO a
interpretGame drv = interpret_
  where
    interpret_ g = case g of
      Pure x -> return x
      Free g' -> case g' of
        GC.RegisterMesh m f -> drvRegisterMesh drv m >>= interpret_ . f
        GC.LoadObject name f -> drvLoadObject drv name >>= interpret_ . f
        GC.RegisterMaterial m f -> drvRegisterMaterial drv m >>= interpret_ . f
        GC.RegisterLight l f -> drvRegisterLight drv l >>= interpret_ . f
        GC.SwitchLightOn g ent nxt -> drvSwitchLightOn drv g ent >> interpret_ nxt
        GC.RegisterCamera proj ent f -> drvRegisterCamera drv proj ent >>= interpret_ . f
        GC.Look cam nxt -> drvLook drv cam >> interpret_ nxt
        GC.Log lt msg nxt -> drvLog drv lt msg >> interpret_ nxt
        GC.Time f -> drvTime drv >>= interpret_ . f

-------------------------------------------------------------------------------
-- Callbacks
handleKey :: TVar [Event u] -> Window -> GLFW.Key -> Int -> GLFW.KeyState -> ModifierKeys -> IO ()
handleKey events _ k _ s _ = atomically . modifyTVar events $ (++ keys)
  where
    keys = case s of
      KeyState'Pressed   -> key KeyPressed
      KeyState'Released  -> key KeyReleased
      KeyState'Repeating -> key KeyReleased
    key st = case k of
        Key'Unknown      -> []
        Key'Space        -> r Space
        Key'Apostrophe   -> r Apostrophe
        Key'Comma        -> r Comma
        Key'Minus        -> r Minus
        Key'Period       -> r Period
        Key'Slash        -> r Slash
        Key'0            -> r Zero
        Key'1            -> r One
        Key'2            -> r Two
        Key'3            -> r Three
        Key'4            -> r Four
        Key'5            -> r Five
        Key'6            -> r Six
        Key'7            -> r Seven
        Key'8            -> r Eight
        Key'9            -> r Nine
        Key'Semicolon    -> r Semicolon
        Key'Equal        -> r Equal
        Key'A            -> r A
        Key'B            -> r B
        Key'C            -> r C
        Key'D            -> r D
        Key'E            -> r E
        Key'F            -> r F
        Key'G            -> r G
        Key'H            -> r H
        Key'I            -> r I
        Key'J            -> r J
        Key'K            -> r K
        Key'L            -> r L
        Key'M            -> r M
        Key'N            -> r N
        Key'O            -> r O
        Key'P            -> r P
        Key'Q            -> r Q
        Key'R            -> r R
        Key'S            -> r S
        Key'T            -> r T
        Key'U            -> r U
        Key'V            -> r V
        Key'W            -> r W
        Key'X            -> r X
        Key'Y            -> r Y
        Key'Z            -> r Z
        Key'LeftBracket  -> r LeftBracket
        Key'Backslash    -> r Backslash
        Key'RightBracket -> r RightBracket
        Key'GraveAccent  -> r GraveAccent
        Key'World1       -> r World1
        Key'World2       -> r World2
        Key'Escape       -> r Escape
        Key'Enter        -> r Enter
        Key'Tab          -> r Tab
        Key'Backspace    -> r Backspace
        Key'Insert       -> r Insert
        Key'Delete       -> r Delete
        Key'Right        -> r Right
        Key'Left         -> r Left
        Key'Down         -> r Down
        Key'Up           -> r Up
        Key'PageUp       -> r PageUp
        Key'PageDown     -> r PageDown
        Key'Home         -> r Home
        Key'End          -> r End
        Key'CapsLock     -> r CapsLock
        Key'ScrollLock   -> r ScrollLock
        Key'NumLock      -> r NumLock
        Key'PrintScreen  -> r PrintScreen
        Key'Pause        -> r Pause
        Key'F1           -> r F1
        Key'F2           -> r F2
        Key'F3           -> r F3
        Key'F4           -> r F4
        Key'F5           -> r F5
        Key'F6           -> r F6
        Key'F7           -> r F7
        Key'F8           -> r F8
        Key'F9           -> r F9
        Key'F10          -> r F10
        Key'F11          -> r F11
        Key'F12          -> r F12
        Key'F13          -> r F13
        Key'F14          -> r F14
        Key'F15          -> r F15
        Key'F16          -> r F16
        Key'F17          -> r F17
        Key'F18          -> r F18
        Key'F19          -> r F19
        Key'F20          -> r F20
        Key'F21          -> r F21
        Key'F22          -> r F22
        Key'F23          -> r F23
        Key'F24          -> r F24
        Key'F25          -> r F25
        Key'Pad0         -> r Pad0
        Key'Pad1         -> r Pad1
        Key'Pad2         -> r Pad2
        Key'Pad3         -> r Pad3
        Key'Pad4         -> r Pad4
        Key'Pad5         -> r Pad5
        Key'Pad6         -> r Pad6
        Key'Pad7         -> r Pad7
        Key'Pad8         -> r Pad8
        Key'Pad9         -> r Pad9
        Key'PadDecimal   -> r PadDecimal
        Key'PadDivide    -> r PadDivide
        Key'PadMultiply  -> r PadMultiply
        Key'PadSubtract  -> r PadSubtract
        Key'PadAdd       -> r PadAdd
        Key'PadEnter     -> r PadEnter
        Key'PadEqual     -> r PadEqual
        Key'LeftShift    -> r LeftShift
        Key'LeftControl  -> r LeftControl
        Key'LeftAlt      -> r LeftAlt
        Key'LeftSuper    -> r LeftSuper
        Key'RightShift   -> r RightShift
        Key'RightControl -> r RightControl
        Key'RightAlt     -> r RightAlt
        Key'RightSuper   -> r RightSuper
        Key'Menu         -> r Menu
      where
        r x = [CoreEvent . KeyEvent $ st x]

handleMouseButton :: TVar [Event u] -> Window -> GLFW.MouseButton -> GLFW.MouseButtonState -> ModifierKeys -> IO ()
handleMouseButton events _ b s _ = atomically . modifyTVar events $ (++ [CoreEvent $ MouseButtonEvent mouseEvent])
  where
    mouseEvent = case s of
      MouseButtonState'Pressed -> ButtonPressed button
      MouseButtonState'Released -> ButtonReleased button
    button = case b of
      MouseButton'1 -> MouseLeft
      MouseButton'2 -> MouseRight
      MouseButton'3 -> MouseMiddle
      MouseButton'4 -> Mouse4
      MouseButton'5 -> Mouse5
      MouseButton'6 -> Mouse6
      MouseButton'7 -> Mouse7
      MouseButton'8 -> Mouse8

handleMouseMotion :: TVar (Double,Double) -> TVar [Event u] -> Window -> Double -> Double -> IO ()
handleMouseMotion xy' events _ x y = do
    (x',y') <- atomically (readTVar xy')
    atomically . modifyTVar events $ (++ [CoreEvent . MouseMotionEvent $ MouseMotion x y (x-x') (y-y')])

handleWindowClose :: TVar [Event u] -> Window -> IO ()
handleWindowClose events _ = atomically . modifyTVar events $ (++ map CoreEvent [WindowEvent Closed,SystemEvent Quit])

handleWindowFocus :: TVar [Event u] -> Window -> FocusState -> IO ()
handleWindowFocus events _ f = atomically . modifyTVar events $ (++ [CoreEvent $ WindowEvent focusEvent])
  where
    focusEvent = case f of
      FocusState'Focused -> FocusGained
      FocusState'Defocused -> FocusLost