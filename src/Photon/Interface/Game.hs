{-# LANGUAGE ExistentialQuantification #-}

module Photon.Interface.Game where

import Control.Monad.Free
import Data.List.NonEmpty ( NonEmpty(..) )
import Photon.Core.Entity ( Entity )
import Photon.Core.Light ( Light )
import Photon.Core.Loader ( Load )
import Photon.Core.Material ( Material )
import Photon.Core.Mesh ( Mesh )
import Photon.Core.Projection ( Projection )
import Photon.Render.Camera ( GPUCamera )
import Photon.Render.Light ( GPULight )
import Photon.Render.Material ( GPUMaterial )
import Photon.Render.Mesh ( GPUMesh )

newtype Key = Key { unKey :: Char } deriving (Eq,Ord,Read,Show)

data KeyState
  = KeyPressed Key
  | KeyReleased Key
    deriving (Eq,Read,Show)

data MouseButton
  = MouseLeft
  | MouseMiddle
  | MouseRight
    deriving (Eq,Ord,Read,Show)

data MouseMotion = MouseMotion {
    mouseX  :: Double
  , mouseY  :: Double
  , mouseRX :: Double
  , mouseRY :: Double
  } deriving (Eq,Read,Show)

data WindowState
  = Closed
  | Opened
  | FocusLost
  | FocusGained
    deriving (Eq,Read,Show)

data SystemState
  = Quit
    deriving (Eq,Read,Show)

data Event
  = KeyEvent KeyState
  | MouseButtonEvent [MouseButton]
  | MouseMotionEvent MouseMotion
  | WindowEvent WindowState
  | SystemEvent SystemState
    deriving (Eq,Read,Show)

type EventHandler a = NonEmpty Event -> a -> Maybe a

data GameAction n
  = RegisterMesh Mesh (GPUMesh -> n)
  | forall a. (Load a) => LoadObject String (a -> n)
  | RegisterMaterial Material (GPUMaterial -> n)
  | RenderMeshes GPUMaterial [GPUMesh] n
  | RegisterLight Light (GPULight -> n)
  | SwitchLightOn GPULight n
  | RegisterCamera Projection Entity (GPUCamera -> n)
  | Look GPUCamera n
  
instance Functor GameAction where
  fmap f a = case a of
    RegisterMesh m g -> RegisterMesh m (f . g)
    LoadObject n g -> LoadObject n (f . g)
    RegisterMaterial m g -> RegisterMaterial m (f . g)
    RenderMeshes mat mshs n -> RenderMeshes mat mshs (f n)
    RegisterLight l g -> RegisterLight l (f . g)
    SwitchLightOn l n -> SwitchLightOn l (f n)
    RegisterCamera proj view g -> RegisterCamera proj view (f . g)
    Look c n -> Look c (f n)

type Game = Free GameAction

registerMesh :: Mesh -> Game GPUMesh
registerMesh m = Free (RegisterMesh m Pure)

load :: (Load a) => String -> Game a
load name = Free (LoadObject name Pure)

registerMaterial :: Material -> Game GPUMaterial
registerMaterial m = Free (RegisterMaterial m Pure)

renderMeshes :: GPUMaterial -> [GPUMesh] -> Game ()
renderMeshes mat mshs = Free . RenderMeshes mat mshs $ Pure ()

registerLight :: Light -> Game GPULight
registerLight l = Free (RegisterLight l Pure)

switchLightOn :: GPULight -> Game ()
switchLightOn l = Free . SwitchLightOn l $ Pure ()

registerCamera :: Projection -> Entity -> Game GPUCamera
registerCamera proj view = Free (RegisterCamera proj view Pure)

look :: GPUCamera -> Game ()
look c = Free . Look c $ Pure ()

-- TODO: replace all resulting a with Game a
runGame :: IO [Event] -> EventHandler a -> (a -> a) -> (a -> IO ()) -> a -> IO ()
runGame pollEvents handler logic sink = run
  where
    run app = pollEvents >>= forwardEvents app >>= maybe (return ()) runLogic
    forwardEvents app events = return $ case events of
      [] -> Just app
      (x:xs) -> handler (x :| xs) app
    runLogic = sequence_ . sequence [sink,run] . logic
