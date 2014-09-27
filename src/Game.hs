{-# LANGUAGE Arrows                #-}
{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |

module Game where
import System.Exit        
import           GameSound
import           Data.IORef
import           Debug.Trace
import qualified Input as I
       
import Collision
import           Input hiding (Move, StopMove, LookAt, StopLookAt)
       
                 
import qualified Graphics.Rendering.OpenGL as GL
import           Graphics.Rendering.OpenGL (($=))
import           Control.Concurrent.Async
import           Control.Arrow
import           Control.Exception
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM     (TQueue, TVar, atomically,
                                             newTQueueIO, newTVarIO, readTQueue,
                                             readTVar, tryReadTQueue, modifyTVar,
                                             writeTQueue, writeTVar)
import           Control.DeepSeq
import           Control.DeepSeq.Generics (genericRnf)
import           Control.Lens hiding (both)
import qualified Control.Lens as L
import           Control.Monad.Free
import           Control.Monad.Reader
import           Control.Monad.RWS.Strict
import           Control.Monad.State.Strict
import qualified Control.Wire as W
import qualified Control.Wire.Unsafe.Event as W
import           FRP.Sodium hiding (merge, accum)
import qualified FRP.Sodium as S
import           GHC.Generics
import           HSys                       hiding (InputWire, WorldWire, move,
                                             rotate, EventMove, EventRotate, EventSpawn, applyDelta)
import qualified HSys as Sys
import           System.Mem

import qualified Graphics.UI.GLFW as GLFW

import           System.Random

import           Render.Halo
import qualified Render.Halo as H

import qualified Data.Map.Strict as Map
import           Data.Maybe
                 
import           CleanFRP
import Gameplay
       
import           Pipes
import qualified Pipes as P
import           Pipes.Concurrent
import qualified Pipes.Concurrent as P

data Wall = Wall { _wallHitpoints :: Int } deriving (Show, Generic)
newWall = Wall { _wallHitpoints = 1 }

  
syncEvents :: BoomCommands a -> BoomWorld -> S.Reactive ()
syncEvents commands w = syncEvent w commands 
  where
    syncEvent w (Free (I.Move (x, y) n)) = do
      w^.bInput.bwSendMoveEvent $ MoveEvent (x, y)
      syncEvent w n
    syncEvent w (Free (I.StopMove n)) = do
      w^.bInput.bwSendMoveEvent $ StopMoveEvent
      syncEvent w n
    syncEvent w (Free (I.LookAt dir n)) = do
      w^.bInput.bwSendLookAtEvent $ LookAt dir
      syncEvent w n
    syncEvent w (Free (I.StopLookAt n)) = do
      w^.bInput.bwSendLookAtEvent $ StopLookAt
      syncEvent w n
    syncEvent w (Free (Shoot n)) = do
      w^.bInput.bwSendFire $ FireEvent
      syncEvent w n
    syncEvent _ _ = return ()

-- initPlayer :: RenderControl EntityData
-- initPlayer = do
--   player1MainRu <- getSpriteRenderUnit "Main1"
--   player2MainRu <- getSpriteRenderUnit "Main2"

--   mainBody <- getSprite "boom" "blue_mid"
--   feet <- getSprite "boom" "blue_buttom"
--   gun <- getSprite "boom" "blue_gun"

--   p1Body <- addSprite player1MainRu mainBody (0, 0) 0
--   p2Body <- addSprite player2MainRu mainBody (0, 0) 0

--   p1Feet <- addSprite player1MainRu feet (0, 0) 0
--   p2Feet <- addSprite player2MainRu feet (0, 0) 0

--   p1Gun <- addSprite player1MainRu gun (0, 0) 0
--   p2Gun <- addSprite player2MainRu gun (0, 0) 0
  
--   return $ EntityData (p1Body, p1Feet, p1Gun) (p2Body, p2Feet, p2Gun) Map.empty

initRender :: Game -> RenderControl ()
initRender game = do
  cam0 <- newCamera "MainCam" (512, 1024)
  cam1 <- newCamera "Player1Cam" (512, 512)
  cam2 <- newCamera "Player2Cam" (512, 512)

  viewport0 <- newViewport "MainViewport" $ Viewport 0 0 512 1024
  viewport1 <- newViewport "Player1Viewport" $ Viewport 0 0 512 512
  viewport2 <- newViewport "Player2Viewport" $ Viewport 0 512 512 512

  backgroundRu <- newSpriteRenderUnit "BackgroundRU"
  player1FloorRu <- newSpriteRenderUnit "Floor1"
  player2FloorRu <- newSpriteRenderUnit "Floor2"
  player1MainRu <- newSpriteRenderUnit "Main1"
  player2MainRu <- newSpriteRenderUnit "Main2"
  bulletRu <- newSpriteRenderUnit "Bullets"
  
  mapM (flip setViewport $ viewport0) [backgroundRu, bulletRu]
  mapM (flip setViewport $ viewport1) [player1FloorRu, player1MainRu]
  mapM (flip setViewport $ viewport2) [player2FloorRu, player2MainRu]

  mapM (flip setCamera $ cam0) [backgroundRu, bulletRu]
  _ <- mapM (flip setCamera $ cam1) [player1FloorRu, player1MainRu]
  mapM (flip setCamera $ cam2) [player2FloorRu, player2MainRu]

  --mainWall <- getSprite "boom" "wallBasic"
  return ()
              
  -- let g = mkStdGen 0
  -- let coords = evalState gen g 
               

  -- mapM_ (\(x, y) -> 
  --        addSprite player1FloorRu mainWall(fromIntegral $ x*32, fromIntegral $ y*32) 0
  --                  ) coords

  -- mapM_ (\(x, y) -> addSprite player1FloorRu mainWall (fromIntegral x*32, fromIntegral y*32) 0) [(x, y) | x <- [-5..5::Int], y <- [-1, 0..1::Int]]
         
  -- where 
  --   gen :: State StdGen [(Int, Int)]
  --   gen = mapM (\_ -> do
  --               s <- get
  --               let (x, newS) = randomR (-10, 10::Int) s
  --               let (y, newS') = randomR (-10, 10::Int) newS
  --               put newS'
  --               return (x, y)
  --            ) [0..50::Int]

newGame :: GLFW.Window -> IO (Game)
newGame win = do
  (renderer, renderManager) <- runStateT (newRenderer win) newRenderManager
  let game = Game { _gameWorlds = Map.empty
--                         , _gameWires = Map.empty
                  , _gamePushWorld = Map.empty
                        , _gameRenderManager = renderManager
                        , _gameRenderer = renderer
                        , _gameEntityData = Nothing
                        , _gameEntityManager = EntityManager 10
                        }

  game' <- flip execStateT game $ do
                  (bw1, pushW1) <- lift $ newBoomWorld
                  (bw2, pushW2) <- lift $ newBoomWorld
                  gameWorlds.at PlayerOne .= Just (bw1)
                  gameWorlds.at PlayerTwo .= Just (bw2)
                  gamePushWorld.at PlayerOne .= Just pushW1
                  gamePushWorld.at PlayerTwo .= Just pushW2

                  -- gameWires.at PlayerOne .= Just gameWire
                  -- gameWires.at PlayerTwo .= Just gameWire

                  newGame <- get
                  (_, newGameRenderer) <- lift $ runStateT (runRenderControl (initRender newGame)) (newGame^.gameRenderer)
                  gameEntityData .= Nothing
                  gameRenderer .= newGameRenderer

  -- print (game'^.gameWorlds)
  return (game')
  
mkDoQuit :: InputFRP (S.Behavior Bool)
mkDoQuit = do
  escapeKey <- keyDown GLFW.Key'Escape
  lift $ hold False (once $ fmap (const True) escapeKey)
         
data LoopConstData a b c d = LoopConstData a b c d
     
type InputSink a = Event (Free BoomActions a)
     
liftA4 :: Applicative f => (a -> b -> c -> d -> e) -> f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d
       
constB = return . pure
       
instance Monoid Int where
         mempty = 0
         mappend a b = a + b
         
    
newtype Tick = Tick Float deriving (Show)
     
data Root = Root
  { _rOutputInput :: P.Output (Either (Player, Free BoomActions ()) Tick)
  , _rInputGame :: P.Input (Either (Player, Free BoomActions ()) Tick)
  , _rOutputGame :: Output (RenderControl ())
  , _rInputRender :: P.Input (RenderControl ())
  , _rGame :: TVar Game
  , _rKeyInput :: Sys.Input
  , _rStepKeyInput :: IO ()
  , _rQuit :: TVar Bool
  }

makeLenses ''Root
           
p1Movement = 
  let keys = [GLFW.Key'W, GLFW.Key'A, GLFW.Key'S, GLFW.Key'D, GLFW.Key'Space]
  in movement keys

p2Movement = controllerMovement
  -- let keys = [GLFW.Key'Up, GLFW.Key'Left, GLFW.Key'Down, GLFW.Key'Right, GLFW.Key'Space]
  -- in movement keys  
  
controllerMovement :: InputFRP (Event (Free BoomActions ()))
controllerMovement = do
  leftTrigger <- leftTriggerDirection
  let stopMoveE = (\_ -> liftF (I.StopMove ())) <$> filterE (\(x, y) -> abs x < 0.01 && abs y < 0.01) (updates leftTrigger)
  
  let move = (\dir -> liftF (I.Move dir ())) <$> filterE (\(x, y) -> abs x > 0.01 || abs y > 0.01) (updates leftTrigger)

  return $ mergeWith (>>) stopMoveE move
  
controllerLookat :: InputFRP (Event (Free BoomActions ()))
controllerLookat = do
  rightTrigger <- rightTriggerDirection
  let stopLook = (\_ -> liftF (I.StopLookAt ())) <$> filterE (\(x, y) -> abs x < 0.01 && abs y < 0.01) (updates rightTrigger)
  let lookAt = (\dir -> liftF (I.LookAt dir ())) <$> filterE (\(x, y) -> abs x > 0.01 || abs y > 0.01) (updates rightTrigger)
  return $ mergeWith (>>) stopLook lookAt
  
movement :: [GLFW.Key] -> InputFRP (Event (Free BoomActions ()))
movement keys = do
  [wDown, aDown, sDown, dDown, spaceDown] <- (mapM keyIsDown keys)
  [wUp, aUp, sUp, dUp, spaceUp] <- mapM keyIsUp keys
  
  let moveLeft = fmap (\b -> if b then (-1, 0) else (0, 0)) $ liftA2 (&&) aDown dUp
      moveRight = fmap (\b -> if b then (1, 0) else (0, 0)) $ liftA2 (&&) dDown aUp
      moveTop = fmap (\b -> if b then (0, 1) else (0, 0)) $ liftA2 (&&) wDown sUp
      moveBottom = fmap (\b -> if b then (0, -1) else (0, 0)) $ liftA2 (&&) sDown wUp
  
      -- move = liftA4 (\a b c d -> a `mappend` b `mappend` c `mappend` d) moveLeft moveRight moveTop moveBottom
  
  beh <- lift $ do
      w <- while (both wDown sUp) (0, 0) (constB (0, 1))
      s <- while (both sDown wUp) (0, 0) (constB (0, -1))
      a <- while (both aDown dUp) (0, 0) (constB (-1, 0))
      d <- while (both dDown aUp) (0, 0) (constB (1, 0))
      return $ liftA4 (\a b c d -> a `mappend` b `mappend` c `mappend` d) w a s d
  
  let e = updates beh
  let stopMoveE :: Event (Free BoomActions ())
      stopMoveE = replace (liftF (I.StopMove ())) $ S.filterE (==(0,0)) e
  let move :: Event (Free BoomActions ())
      move = fmap (\dir -> liftF $ I.Move dir ()) $ S.filterE (/=(0,0)) $ updates beh
  
  let eFire = fmap (const $ liftF (Shoot ())) $ updates spaceDown
  
  return $ mergeWith (>>) (mergeWith (>>) stopMoveE move) eFire
  
gameSync root = do
  game <- atomically . readTVar $ (root^.rGame)
  
  vDelta1 <- newIORef (deltaId)
  vDelta2 <- newIORef (deltaId)
  
  vReload <- newIORef False

  frp <- initGame vDelta1 vDelta2 (game^.gameWorlds.at PlayerOne . L.to fromJust) (game^.gameWorlds.at PlayerTwo . L.to fromJust)
  
  reloadEvent <- syncInput (root^.rKeyInput) $ keyDown GLFW.Key'R
  sync $ S.listen reloadEvent (\_ -> do
    writeIORef vReload True
    return ()
    )

  let oc1 = (execState (octreeUpdateInsert [(-1, (-100, -100), [(0, 0), (100, 0), (100, 100), (0, 100)])]) newOctree)
  let oc2 = (execState (octreeUpdateInsert [(-1, (-100, -100), [(0, 0), (100, 0), (100, 100), (0, 100)])]) newOctree)

  runEffect $ (fromInput (root^.rInputGame)) >-> gameLoop vDelta1 vDelta2 (root^.rGame) vReload frp (oc1, oc2) >-> toOutput (root^.rOutputGame)
  performGC
     
-- gameLoop :: Event ([BoomWorldDelta]) -> Output [BoomWorldDelta] -> Game -> Consumer (Either (Free BoomActions ()) (Tick)) IO ()
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

gameLoop vDelta1 vDelta2 vGame vReload frp (oc1, oc2) = do
         
  frp' <- lift $ do
    reload <- readIORef vReload
    if reload then do
        writeIORef vReload False
        game <- atomically . readTVar $ vGame
        newFrp <- reloadGame vDelta1 vDelta2 (game^.gameWorlds.at PlayerOne . L.to fromJust) (game^.gameWorlds.at PlayerTwo . L.to fromJust) frp
        return newFrp
    else
      return frp

  commands <- await

  game <- lift $ atomically $ readTVar vGame

  let Just world1' = game^.gameWorlds.at PlayerOne
  let Just world2' = game^.gameWorlds.at PlayerTwo
  world1 <- lift . sync $ sample world1'
  world2 <- lift . sync $ sample world2'
  
  case commands of
    Left (player, commands') -> do
      case player of 
        PlayerOne -> lift . sync $ syncEvents commands' world1
        PlayerTwo -> lift . sync $ syncEvents commands' world2

    Right (Tick dt) -> do
      lift . sync $ (world1^.bInput.bwSendTick) (4)
      lift . sync $ (world2^.bInput.bwSendTick) (4)
  
  delta1 <- lift $ readIORef vDelta1
  delta2 <- lift $ readIORef vDelta2
  lift $ writeIORef vDelta1 (deltaId)
  lift $ writeIORef vDelta2 (deltaId)

  -- (rc, pendingActions1, pendingActions2) <- lift $ catchAny (do
  (rc, pendingActions1, pendingActions2, oc1New, oc2New, em') <- lift $ (do

    let Just pushGame1 = game^.gamePushWorld.at PlayerOne
    let Just pushGame2 = game^.gamePushWorld.at PlayerTwo

    --let (delta1', em) = runState (flattenDelta delta1) (game^.gameEntityManager)
    -- let (delta2', em') = runState (flattenDelta delta2) em
  
    let (delta1', (em, renderer)) = runState (flattenDelta delta1) (game^.gameEntityManager, game^.gameRenderer)
    let (delta2', (em', renderer')) = runState (flattenDelta delta2) (em, renderer)

    let (_, oc1New, delta1Col) = runRWS (collisionSystem delta1') () oc1
    let (_, oc2New, delta2Col) = runRWS (collisionSystem delta2') () oc2

    let (bw1, bw2, rc) = applyDelta (delta1Col) (delta2Col) world1 world2

    let pA1 = bw1^.bPendingActions
    let pA2 = bw2^.bPendingActions

    let bw1' = bw1 & bPendingActions .~ []
    let bw2' = bw2 & bPendingActions .~ []

    atomically $ modifyTVar vGame (\g -> g & gameEntityManager .~ em')
    atomically $ modifyTVar vGame (\g -> g & gameRenderer .~ renderer')

    _ <- sync $ do
      -- apply changes
      pushGame1 bw1'
      pushGame2 bw2'

    return (rc, pA1, pA2, oc1New, oc2New, em')
    )
      -- return (return (), [], [])
  
  mapM (lift . sync) pendingActions1
  mapM (lift . sync) pendingActions2

  rc `seq` P.yield rc

  gameLoop vDelta1 vDelta2 vGame vReload frp' (oc1New, oc2New)
  
type RC = RenderControl ()
collectCommands :: Double -> RC -> Pipe RC RC IO ()
collectCommands start d = do
  s <- await
  Just end <- lift $ GLFW.getTime
  if realToFrac (end - start) > tickrate / 1000 then do
    let x = (d >> s) in x `seq` P.yield x
    collectCommands (end + (end - start) - 0.016) (Pure ())
  else do
    collectCommands start (d >> s)
  
         
-- renderLoop :: TVar Game -> Consumer (StateT Game RenderControl ()) IO ()
renderLoop win fontRenderer root = do
  inp <- await
  -- lift $ GLFW.getTime >>= print
  --let Just entData = game^.gameEntityData
  --let !renderControls = handleEvents entData inp (Pure ())
  
  --rc <- lift . atomically $ modifyTVar vGame (execStateT inp)
  game <- lift . atomically $ readTVar (root^.rGame)
  
  let rc = inp
  
  -- StateT Game RenderControl ()
  -- RenderControl -> StateT Renderer IO a
  -- (newGame, newGameRenderer) <- lift $ catchAny (do
  (newGameRenderer) <- lift $ (do
 
    Just t1 <- GLFW.getTime
    ((), newGameRenderer) <- runStateT (runRenderControl rc) (game^.gameRenderer)
    Just t2 <- GLFW.getTime
    render newGameRenderer
    Just t3 <- GLFW.getTime

    GL.viewport $= (GL.Position 0 0, GL.Size 1920 1080)
    renderText fontRenderer (newDefaultCamera 1920 1080)
    GLFW.swapBuffers win
    newGameRenderer `seq` return (newGameRenderer)

    ) -- $ \e -> do
      -- playFile "data/didntwork.wav"
      -- print ("Render loop", e)
      -- exitFailure
      -- return $ (game, game^.gameRenderer)
  -- lift $ print $ map (*1000) [t2 - t1, t3 - t2, t3 - t1]

  lift . atomically $ modifyTVar (root^.rGame) (\game -> game & gameRenderer .~ newGameRenderer)
  
  quit <- lift . atomically $ readTVar (root^.rQuit)

  unless quit $
    renderLoop win fontRenderer root 
  
tickrate :: Float
tickrate = 64
         
inputTickrate :: Float
inputTickrate = 16
         
inputStepper stepInput = do
  Just oldTime <- GLFW.getTime
  genTicks' oldTime 0
    where 
      genTicks' oldTime missing = do
        Just newTime <- GLFW.getTime

        let dt = realToFrac $ (newTime - oldTime)*1000 + fromIntegral missing
        let steps = map (const inputTickrate) [inputTickrate, inputTickrate*2..dt]
        let dt' = round dt :: Int

        threadDelay $ 1000 * (round inputTickrate - 1000*round (newTime - oldTime))
        stepInput

        genTicks' newTime (dt' `mod` round inputTickrate)
 

-- genTicks :: Producer Tick IO ()
genTicks stepInput = do
  Just oldTime <- lift GLFW.getTime
  genTicks' oldTime 0
    where 
      genTicks' oldTime missing = do
        Just newTime <- lift GLFW.getTime

        let dt = realToFrac $ (newTime - oldTime)*1000 + fromIntegral missing
        let steps = map (const tickrate) [tickrate, tickrate*2..dt]
        mapM (\s -> s `seq` P.yield . Tick $ s) steps
  
        let dt' = round dt :: Int

        lift $ threadDelay $ 1000 * (round tickrate - 1000*round (newTime - oldTime))
        --lift $ stepInput
        genTicks' newTime (dt' `mod` round tickrate)
  
loopInput var root = do
  f <- lift $ atomically $ readTQueue var
  f `seq` P.yield (Left f) >-> (toOutput (root^.rOutputInput))
  loopInput var root
  
inputSync root = do
    var <- newTQueueIO
    syncInput (root^.rKeyInput) $ do
        e2 <- p1Movement
        e <- p2Movement
        e3 <- controllerLookat
        lift $ S.listen e (\f -> do
          atomically $ writeTQueue var (PlayerOne, f)
          )
        lift $ S.listen e2 (\f -> do
          atomically $ writeTQueue var (PlayerTwo, f)
          )
        lift $ S.listen e3 (\f -> do
          atomically $ writeTQueue var (PlayerOne, f)
          )

    runEffect $ loopInput var root
    
tickSync r = do
    runEffect $ for (genTicks (r^.rStepKeyInput)) (P.yield . Right) >-> (toOutput (r^.rOutputInput))
    performGC
    
inputStepperSync r = do
    inputStepper (r^.rStepKeyInput)

runGame win = do
  GLFW.swapInterval 1
  (eT, pushT) <- sync S.newEvent

  (keyInput, stepInput) <- initInput win
  -- a <- syncInput input $ keyDown GLFW.Key'Escape
  doQuit <- syncInput keyInput mkDoQuit
  
  quit <- newTVarIO False

  sync $ S.listen (updates doQuit) $ \_ -> atomically $ writeTVar quit True
  
  (outputInput, inputGame) <- spawn Unbounded
  (outputGame, inputRender) <- spawn Unbounded
  print "Print this to avoid segmentation fault"
  (game) <- newGame win
  
  fm <- newFontManager
  font <- newFont fm "data/font.otf" 64
  textAtlas <- newFontAtlas font
  
  (fontRenderer, newRenderManager) <- runStateT (newFontRenderer textAtlas) (game^.gameRenderManager)
  let game' = game & gameRenderManager .~ newRenderManager
  
  g <- newTVarIO game'
   
  let r = Root { _rOutputInput = outputInput
               , _rInputGame = inputGame
               , _rOutputGame = outputGame
               , _rInputRender = inputRender
               , _rGame = g
               , _rKeyInput = keyInput
               , _rStepKeyInput = stepInput
               , _rQuit = quit
               }
 
  -- input loop
  _ <- async $ catchAny (inputStepperSync r) $ \e -> print e >> return ()
  i <- async $ catchAny (inputSync r) $ \e -> print e >> return ()
  _ <- async $ catchAny (tickSync r) $ \e -> print e >> return ()
  tId <- async $ catchAny (gameSync r) $ \e -> print e >> return ()
  Just t <- GLFW.getTime
  catchAny (runEffect $ fromInput (r^.rInputRender) >-> collectCommands t (Pure ()) >-> renderLoop win fontRenderer r) $ \e -> print e  >> return ()

  return ()

       
