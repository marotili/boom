{-# LANGUAGE Arrows                #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |

module Game where

import           Debug.Trace
import           Input                      hiding (Move)
import qualified Input                      as I

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.STM     (TQueue, TVar, atomically,
                                             newTQueueIO, newTVarIO, readTQueue,
                                             readTVar, tryReadTQueue,
                                             writeTQueue, writeTVar)
import           Control.DeepSeq
import           Control.DeepSeq.Generics   (genericRnf)
import           Control.Lens
import           Control.Monad.Free
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import qualified Control.Wire               as W
import qualified Control.Wire.Unsafe.Event  as W
import           GHC.Generics
import           HSys                       hiding (InputWire, WorldWire, move,
                                             rotate)
import qualified HSys                       as Sys

import qualified Graphics.UI.GLFW           as GLFW

import           System.Random

import           Render.Halo

import qualified Data.Map.Strict            as Map
import           Data.Maybe

data Wall = Wall { _wallHitpoints :: Int } deriving (Show, Generic)
instance NFData Wall where rnf = genericRnf
newWall = Wall { _wallHitpoints = 1 }

-- | an outside entity (user input, artificial intelligence) uses commands
-- for now only one entity (the player) exists
data BoomCommandState = BoomCommandState
    { _bcsMoveDirection :: !(Maybe (Float, Float))
    , _bcsLookAt        :: !(Maybe (Float, Float))
    , _bcsWantsShoot    :: !Bool
    } deriving (Show, Generic)

instance NFData BoomCommandState where rnf = genericRnf

makeLenses ''BoomCommandState

buildCommandState :: BoomCommands a -> BoomCommandState -> BoomCommandState
buildCommandState commands startState =
    execState (buildCommandState' commands) (startState & bcsWantsShoot .~ False)
    where
      buildCommandState' (Free (I.Move dir n)) = do
        bcsMoveDirection .= Just dir
        buildCommandState' n
      buildCommandState' (Free (StopMove n)) = do
        bcsMoveDirection .= Nothing
        buildCommandState' n
      buildCommandState' (Free (LookAt dir n)) = do
        bcsLookAt .= Just dir
        buildCommandState' n
      buildCommandState' (Free (Shoot n)) = do
        bcsWantsShoot .= True
        buildCommandState' n
      buildCommandState' (Pure a) = return a



data BoomWorld = BoomWorld { _bDefaultWorld :: !DefaultWorld
                           , _bWalls        :: !(Map.Map (Int, Int) Wall)
                           , _bCommandState :: !BoomCommandState
                           -- , _bEntityWires  :: !(WorldWire EntityId ())
                           } deriving (Show, Generic)

instance NFData BoomWorld where rnf = genericRnf

makeLenses ''BoomWorld

data BoomWorldDelta next =
    DefaultDelta !(DefaultWorldDelta next)
    | DestroyWall !(forall s. WallId s) !next

instance Functor BoomWorldDelta where
    fmap f (DefaultDelta n) = DefaultDelta (fmap f n)
    fmap f (DestroyWall id n) = DestroyWall id (f n)

newtype WallId s = WallId (Int, Int)
getWall :: BoomWorld-> (Int, Int) -> Maybe (WallId s)
getWall world coords = if Map.member coords (world^.bWalls)
                       then Just (WallId coords)
                       else Nothing


instance DeriveDefaultWorldPred BoomWorld HTrue
instance DeriveDefaultWorld BoomWorld where
         getDefaultWorld bw = bw^.bDefaultWorld
         getDefaultDelta _ f = hoistFree DefaultDelta f
         getDefaultWorldL = bDefaultWorld

newBoomWorld :: BoomWorld
newBoomWorld = execState (applyDelta initialCommands) emptyWorld
    where emptyWorld = BoomWorld { _bDefaultWorld = newDefaultWorld
                                 , _bWalls = Map.insert (2, 0) newWall Map.empty
                                 , _bCommandState = BoomCommandState
                                                    { _bcsMoveDirection = Nothing
                                                    , _bcsWantsShoot = False
                                                    , _bcsLookAt = Nothing
                                                    }

                                 }
          initialCommands :: Free (Delta BoomWorld) ()
          initialCommands = do
            defaultWorld (undefined::BoomWorld) $ do
              eId0 <- spawnMovable "Player" (0, 0) 0
              Just eId <- getEntityByName "Player"
              movable <- mkMovable eId
              traceShow ("Test", eId0 == eId, movable) $ return ()

instance World BoomWorld where
         type Delta BoomWorld = BoomWorldDelta
         applyDelta (Free (DefaultDelta defaultWorldDelta)) = do
           applyDelta' (undefined::BoomWorld) defaultWorldDelta
         applyDelta (Free (DestroyWall _ n)) = applyDelta n
         applyDelta (Pure _) = return ()

data Player =
     PlayerOne
     | PlayerTwo
     deriving (Eq, Ord, Show, Generic)

instance NFData Player where rnf = genericRnf

type WorldWire a b = Sys.WorldWire BoomWorld a b

data Game = Game
     { _gameWorlds        :: !(Map.Map Player BoomWorld)
     , _gameWires         :: !(Map.Map Player (WorldWire () ()))
     , _gameRenderManager :: !RenderManager
     , _gameRenderer      :: !Renderer
     } deriving (Generic)


instance NFData RenderManager where rnf x = x `seq` ()
instance NFData Renderer where rnf x = x `seq` ()
instance NFData (W.Wire (W.Timed W.NominalDiffTime ()) () (RWST UserInput (BoomCommands ()) () Identity) () ()) where rnf x = x `seq` ()
instance NFData (W.Wire (W.Timed W.NominalDiffTime ()) () (RWS BoomWorld (Free BoomWorldDelta ()) ()) () ()) where rnf x = x `seq` ()

instance NFData Game where rnf = genericRnf

makeLenses ''Game

instance Show Game where
    show g = show (g^.gameWorlds)

initPlayer :: RenderControl ()
initPlayer = do
  player1MainRu <- getSpriteRenderUnit "Main1"
  player2MainRu <- getSpriteRenderUnit "Main2"

  mainBody <- getSprite "boom" "blue_mid"
  feet <- getSprite "boom" "blue_buttom"
  gun <- getSprite "boom" "blue_gun"

  addSprite player1MainRu mainBody "Player1MainBody" (0, 0) 0
  addSprite player2MainRu mainBody "Player2MainBody" (0, 0) 0

  addSprite player1MainRu feet "Player1Feet" (0, 0) 0
  addSprite player2MainRu feet "Player2Feet" (0, 0) 0

  addSprite player1MainRu gun "Player1Gun" (0, 0) 0
  addSprite player2MainRu gun "Player2Gun" (0, 0) 0

-- setPlayer1Data :: (Float, Float) -> (Float, Float) -> RenderControl ()
-- setPlayer1Data pos (feetRot, mainRot) = do
--   player1MainRu <- getSpriteRenderUnit "Main1"
--   player2MainRu <- getSpriteRenderUnit "Main2"

--   setPosition player1MainRu "Player1Feet" pos
--   setRotation player1MainRu "Player1Feet" feetRot

--   setPosition player1MainRu "Player1MainBody" pos
--   setRotation player1MainRu "Player1Gun" mainRot

--   setPosition player1MainRu "Player1Gun" pos
--   setRotation player1MainRu "Player1Gun" mainRot

--   setPosition player2MainRu "Player2Feet" pos
--   setRotation player2MainRu "Player2Feet" feetRot

--   setPosition player2MainRu "Player2MainBody" pos
--   setRotation player2MainRu "Player2Gun" mainRot

--   setPosition player2MainRu "Player2Gun" pos
--   setRotation player2MainRu "Player2Gun" mainRot

buildRenderCommands :: Free BoomWorldDelta () -> Free BoomWorldDelta () -> RenderControl ()
buildRenderCommands freeWorld1 freeWorld2 = do
  player1MainRu <- getSpriteRenderUnit "Main1"
  player2MainRu <- getSpriteRenderUnit "Main2"
  world1 player1MainRu freeWorld1
  world2 player2MainRu freeWorld2
    where
      world1 player1MainRu (Free (DefaultDelta (Move 1 (Position pos) n))) = do
        move player1MainRu "Player1Feet" pos
        move player1MainRu "Player1Gun" pos
        move player1MainRu "Player1MainBody" pos
        world1 player1MainRu n
      world1 _ _ = return ()

      world2 ru (Free (DefaultDelta (Move 1 (Position pos) n))) = do
        move ru "Player2Feet" pos
        move ru "Player2Gun" pos
        move ru "Player2MainBody" pos

      world2 _ _ = return ()

initRender :: Game -> RenderControl ()
initRender game = do
  cam0 <- newCamera "MainCam" 512 1024
  cam1 <- newCamera "Player1Cam" 512 512
  cam2 <- newCamera "Player2Cam" 512 512

  viewport0 <- newViewport "MainViewport" 0 0 512 1024
  viewport1 <- newViewport "Player1Viewport" 0 0 512 512
  viewport2 <- newViewport "Player2Viewport" 0 512 512 512

  backgroundRu <- newSpriteRenderUnit "BackgroundRU"
  player1FloorRu <- newSpriteRenderUnit "Floor1"
  player2FloorRu <- newSpriteRenderUnit "Floor2"
  player1MainRu <- newSpriteRenderUnit "Main1"
  player2MainRu <- newSpriteRenderUnit "Main2"
  bulletRu <- newSpriteRenderUnit "Bullets"

  mapM (setViewport viewport0) [backgroundRu, bulletRu]
  mapM (setViewport viewport1) [player1FloorRu, player1MainRu]
  mapM (setViewport viewport2) [player2FloorRu, player2MainRu]

  mapM (setCamera cam0) [backgroundRu, bulletRu]
  mapM (setCamera cam1) [player1FloorRu, player1MainRu]
  mapM (setCamera cam2) [player2FloorRu, player2MainRu]

  mainWall <- getSprite "boom" "wallBasic"
              
  let g = mkStdGen 0
  let coords = evalState gen g 
               

  traceShow coords $ mapM_ (\(x, y) -> 
         addSprite player1FloorRu mainWall ("w" ++ show (x*32, y*32)) (fromIntegral $ x*32, fromIntegral $ y*32) 0
                   ) coords

  initPlayer
  
  where 
    gen :: State StdGen [(Int, Int)]
    gen = mapM (\_ -> do
                s <- get
                let (x, newS) = randomR (-10, 10::Int) s
                let (y, newS') = randomR (-10, 10::Int) newS
                put newS'
                return (x, y)
             ) [0..50::Int]

playerWantsToMove :: WorldWire () (W.Event ())
playerWantsToMove = W.mkGenN $ \_ -> do
                      moveDir <- asks $ \w -> w ^. bCommandState . bcsMoveDirection
                      return $ case moveDir of Just _ -> (Right (W.Event ()), playerWantsToMove)
                                               Nothing -> (Left (), playerWantsToMove)

moveDirection :: WorldWire () (Float, Float)
moveDirection = W.mkGenN $ \_ -> do
                  moveDir <- asks $ \w -> w^.bCommandState . bcsMoveDirection
                  let ret = (\(x, y) -> (x*100, y*100)) $ fromMaybe (0, 0) moveDir in ret `seq` return (Right ret, moveDirection)

spawnBullet :: WorldWire () ()
spawnBullet = W.mkGenN $ \_ -> do
                spawn <- asks $ \w -> w^.bCommandState . bcsWantsShoot

                if spawn then
                    tell $ ((defaultWorld (undefined::BoomWorld) $ do
                        Just playerId <- getEntityByName "Player"
                        Position pos <- getStuff playerId
                        Rotation rot <- getStuff playerId
                        id <- spawnMovable "Bullet" pos rot
                        return ()) :: Free BoomWorldDelta ())
                else return ()
                return (Right (), spawnBullet)

                       
moveWire = W.mkGen $ \ds (dir, eId) -> do
             _ <- W.stepWire (Sys.move (Position dir)) ds (Right eId)
             return (Right (), moveWire)

gameWire :: WorldWire () ()
gameWire = proc _ -> do
             _ <- movement -< ()
             _ <- spawnBullets -< ()
             returnA -< ()
    where
      movement = void (moveWire) W.. W.liftA2 (,) moveDirection (getMovable 1) W.. void playerWantsToMove
           W.--> void W.until W.. fmap (\e -> ((), e)) playerWantsToMove
           W.--> waitOneUpdate W.--> gameWire

      spawnBullets = W.pure ()

liftW :: WorldMonad BoomWorld a -> WorldWire () a
liftW m = W.mkGenN $ \_ -> do
            a <- m
            return $ a `seq` (Right a, liftW m)

readE :: BoomWorld -> EntityId -> WorldMonad BoomWorld (Maybe (EntityComp BoomWorld Position s))
readE = readEntity (undefined::Position)

getMovable :: EntityId -> WorldWire () ((MovableEntity s))
getMovable eId = fmap fromJust $ liftW (readE (undefined::BoomWorld) eId)

newGame :: GLFW.Window -> IO Game
newGame win = do
  (renderer, renderManager) <- runStateT (newRenderer win) newRenderManager
  let game = Game { _gameWorlds = Map.empty
                        , _gameWires = Map.empty
                        , _gameRenderManager = renderManager
                        , _gameRenderer = renderer
                        }


  game' <- flip execStateT game $ do
                  gameWorlds.at PlayerOne .= Just newBoomWorld
                  gameWorlds.at PlayerTwo .= Just newBoomWorld

                  gameWires.at PlayerOne .= Just gameWire
                  gameWires.at PlayerTwo .= Just gameWire

                  newGame <- get
                  newGameRenderer <- lift $ execStateT (runRenderControl (initRender newGame)) (newGame^.gameRenderer)
                  gameRenderer .= newGameRenderer

  print (game'^.gameWorlds)
  return game'

runGame win = do
  eventsChan <- newTQueueIO :: IO (TQueue Event)
  GLFW.setErrorCallback               $ Just $ errorCallback           eventsChan
  GLFW.setWindowPosCallback       win $ Just $ windowPosCallback       eventsChan
  GLFW.setWindowSizeCallback      win $ Just $ windowSizeCallback      eventsChan
  GLFW.setWindowCloseCallback     win $ Just $ windowCloseCallback     eventsChan
  GLFW.setWindowRefreshCallback   win $ Just $ windowRefreshCallback   eventsChan
  GLFW.setWindowFocusCallback     win $ Just $ windowFocusCallback     eventsChan
  GLFW.setWindowIconifyCallback   win $ Just $ windowIconifyCallback   eventsChan
  GLFW.setFramebufferSizeCallback win $ Just $ framebufferSizeCallback eventsChan
  GLFW.setMouseButtonCallback     win $ Just $ mouseButtonCallback     eventsChan
  GLFW.setCursorPosCallback       win $ Just $ cursorPosCallback       eventsChan
  GLFW.setCursorEnterCallback     win $ Just $ cursorEnterCallback     eventsChan
  GLFW.setScrollCallback          win $ Just $ scrollCallback          eventsChan
  GLFW.setKeyCallback             win $ Just $ keyCallback             eventsChan
  GLFW.setCharCallback            win $ Just $ charCallback            eventsChan

  GLFW.swapInterval 1


  let session = newSession
  -- stdGen <- getStdGen
  -- (a, stdGen') <- randomR (0, 100) stdGen
  game <- newGame win
  loop eventsChan win (userInputWire1, userInputWire2) newUserInput session game
  where
    loop !eventsChan !win !(!uIW1, !uIW2) !oldUserInputData !session !game = do
        GLFW.pollEvents
        (_, userInputData, _) <- runRWST (processEvents eventsChan) win oldUserInputData

        -- get input
        -- assign commands

        (dt, session') <- stepSession session
        let (newUIW1, newUIW2, !userCommands1, !userCommands2) = runInput uIW1 uIW2 dt userInputData

        print (userCommands1, userCommands2)

        -- | Update game
        let Just world1' = game^.gameWorlds.at PlayerOne
        let world1 = world1' & bCommandState %~ (buildCommandState userCommands1)
        let Just world2' = game^.gameWorlds.at PlayerTwo
        let world2 = world2' & bCommandState %~ (buildCommandState userCommands2)
        let Just wire1 = game^.gameWires.at PlayerOne
        let Just wire2 = game^.gameWires.at PlayerTwo
        let ((_, !newWire1), _, !freeList1) = runWire dt world1 wire1
        let ((_, !newWire2), _, !freeList2) = runWire dt world2 wire2

        let !renderControls = buildRenderCommands freeList1 freeList2

        let !newWorld1 = execState (applyDelta freeList1) world1
        let !newWorld2 = execState (applyDelta freeList2) world2

        let !game' = game & gameWires.at PlayerOne .~ Just newWire1
                         & gameWires.at PlayerTwo .~ Just newWire2
                         & gameWorlds.at PlayerOne .~ Just newWorld1
                         & gameWorlds.at PlayerTwo .~ Just newWorld2
                    -- gameWorlds.at PlayerOne .~

        !newGameRenderer <- execStateT (runRenderControl renderControls) (game'^.gameRenderer)
        let !game'' = game' & gameRenderer .~ newGameRenderer

        -- | update game
        render (game''^.gameRenderer)

        newUIW1 `deepseq` newUIW2 `deepseq` userInputData `deepseq`
                 game'' `deepseq` (loop eventsChan win (newUIW1, newUIW2) userInputData session' game'')


processEvent :: Event -> RWST GLFW.Window () UserInput IO ()
processEvent ev =
    case ev of
      (EventError e s) -> do
        lift $ print $ "Error :" ++ show (e, s)
        win <- ask
        lift $ GLFW.setWindowShouldClose win True

      (EventWindowSize _ width height) -> return ()
          --modify $ \s -> s { stateCam = cameraUpdateProjection (fromIntegral width) (fromIntegral height) (stateCam s) }

          --state <- get
          --let rdr = asks stateRenderer state
          --renderer' <- lift $ StrictState.execStateT
           --             (runRenderControl $ do
          --                 cam <- getCamera "MainCam"
          --                 modifyCamera cam (cameraUpdateProjection (fromIntegral width) (fromIntegral height))
          --              ) rdr
          -- modify $ \s -> s { stateRenderer = renderer' }

      (EventFramebufferSize _ width height) -> return ()
          -- modify $ \s -> s
          --   { stateWindowWidth  = width
          --   , stateWindowHeight = height
          --   }
          -- adjustWindow

      (EventMouseButton _ mb mbs _) -> do
              if mbs == GLFW.MouseButtonState'Pressed
                 then inputMouseButtonDown mb
                 else inputMouseButtonUp mb

      (EventCursorPos _ x y) ->
          inputUpdateMousePos (x, y)

      (EventKey win k scancode ks mk) -> do
          when (ks == GLFW.KeyState'Pressed) $ do
              when (k == GLFW.Key'Escape) $
                lift $ GLFW.setWindowShouldClose win True
              inputKeyDown k

          when (ks == GLFW.KeyState'Released) $
              inputKeyUp k
      _ -> return ()


--processEvents :: MonadT m => m IO ()
processEvents tc = do
    --tc <- asks envEventsChan
    me <- lift $ atomically $ tryReadTQueue tc
    case me of
      Just e -> do
        processEvent e
        processEvents tc
      Nothing -> return ()


data Event =
    EventError           !GLFW.Error !String
  | EventWindowPos       !GLFW.Window !Int !Int
  | EventWindowSize      !GLFW.Window !Int !Int
  | EventWindowClose     !GLFW.Window
  | EventWindowRefresh   !GLFW.Window
  | EventWindowFocus     !GLFW.Window !GLFW.FocusState
  | EventWindowIconify   !GLFW.Window !GLFW.IconifyState
  | EventFramebufferSize !GLFW.Window !Int !Int
  | EventMouseButton     !GLFW.Window !GLFW.MouseButton !GLFW.MouseButtonState !GLFW.ModifierKeys
  | EventCursorPos       !GLFW.Window !Double !Double
  | EventCursorEnter     !GLFW.Window !GLFW.CursorState
  | EventScroll          !GLFW.Window !Double !Double
  | EventKey             !GLFW.Window !GLFW.Key !Int !GLFW.KeyState !GLFW.ModifierKeys
  | EventChar            !GLFW.Window !Char
  deriving Show

--------------------------------------------------------------------------------

-- Each callback does just one thing: write an appropriate Event to the events
-- TQueue.

errorCallback           :: TQueue Event -> GLFW.Error -> String                                                            -> IO ()
windowPosCallback       :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowSizeCallback      :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
windowCloseCallback     :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowRefreshCallback   :: TQueue Event -> GLFW.Window                                                                     -> IO ()
windowFocusCallback     :: TQueue Event -> GLFW.Window -> GLFW.FocusState                                                  -> IO ()
windowIconifyCallback   :: TQueue Event -> GLFW.Window -> GLFW.IconifyState                                                -> IO ()
framebufferSizeCallback :: TQueue Event -> GLFW.Window -> Int -> Int                                                       -> IO ()
mouseButtonCallback     :: TQueue Event -> GLFW.Window -> GLFW.MouseButton   -> GLFW.MouseButtonState -> GLFW.ModifierKeys -> IO ()
cursorPosCallback       :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
cursorEnterCallback     :: TQueue Event -> GLFW.Window -> GLFW.CursorState                                                 -> IO ()
scrollCallback          :: TQueue Event -> GLFW.Window -> Double -> Double                                                 -> IO ()
keyCallback             :: TQueue Event -> GLFW.Window -> GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys            -> IO ()
charCallback            :: TQueue Event -> GLFW.Window -> Char                                                             -> IO ()

errorCallback           tc e s            = atomically $ writeTQueue tc $ EventError           e s
windowPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventWindowPos       win x y
windowSizeCallback      tc win w h        = atomically $ writeTQueue tc $ EventWindowSize      win w h
windowCloseCallback     tc win            = atomically $ writeTQueue tc $ EventWindowClose     win
windowRefreshCallback   tc win            = atomically $ writeTQueue tc $ EventWindowRefresh   win
windowFocusCallback     tc win fa         = atomically $ writeTQueue tc $ EventWindowFocus     win fa
windowIconifyCallback   tc win ia         = atomically $ writeTQueue tc $ EventWindowIconify   win ia
framebufferSizeCallback tc win w h        = atomically $ writeTQueue tc $ EventFramebufferSize win w h
mouseButtonCallback     tc win mb mba mk  = atomically $ writeTQueue tc $ EventMouseButton     win mb mba mk
cursorPosCallback       tc win x y        = atomically $ writeTQueue tc $ EventCursorPos       win x y
cursorEnterCallback     tc win ca         = atomically $ writeTQueue tc $ EventCursorEnter     win ca
scrollCallback          tc win x y        = atomically $ writeTQueue tc $ EventScroll          win x y
keyCallback             tc win k sc ka mk = atomically $ writeTQueue tc $ EventKey             win k sc ka mk
charCallback            tc win c          = atomically $ writeTQueue tc $ EventChar            win c

