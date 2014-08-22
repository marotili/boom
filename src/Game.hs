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
import           Data.IORef
import           Debug.Trace
import qualified Input as I
import           Input hiding (Move, StopMove)
       
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
       
import           Pipes
import qualified Pipes as P
import           Pipes.Concurrent

data Wall = Wall { _wallHitpoints :: Int } deriving (Show, Generic)
newWall = Wall { _wallHitpoints = 1 }

-- data BoomCommandState = BoomCommandState
--     { _bcsMoveDirection :: !(Maybe (Float, Float))
--     , _bcsLookAt        :: !(Maybe (Float, Float))
--     , _bcsWantsShoot    :: !Bool
--     } deriving (Show, Generic)

-- makeLenses ''BoomCommandState
   
syncEvents :: BoomCommands a -> BoomWorld -> S.Reactive ()
syncEvents commands w = syncEvent w commands 
  where
    syncEvent w (Free (I.Move (x, y) n)) = do
      w^.bInput.bwSendMoveEvent $ MoveEvent (fromIntegral x, fromIntegral y)
      syncEvent w n
    syncEvent w (Free (I.StopMove n)) = do
      w^.bInput.bwSendMoveEvent $ StopMoveEvent
      syncEvent w n
    syncEvent w (Free (LookAt dir n)) = do
      syncEvent w n
    syncEvent w (Free (Shoot n)) = do
      w^.bInput.bwSendFire $ FireEvent
      syncEvent w n
    syncEvent _ _ = return ()

-- buildCommandState :: BoomCommands a -> BoomCommandState -> BoomCommandState
-- buildCommandState commands startState =
--     execState (buildCommandState' commands) (startState & bcsWantsShoot .~ False)
--     where
--       buildCommandState' (Free (I.Move (x, y) n)) = do
--         bcsMoveDirection .= Just (fromIntegral x, fromIntegral y)
--         buildCommandState' n
--       buildCommandState' (Free (I.StopMove n)) = do
--         bcsMoveDirection .= Nothing
--         buildCommandState' n
--       buildCommandState' (Free (LookAt (x, y) n)) = do
--         bcsLookAt .= Just (fromIntegral x, fromIntegral y)
--         buildCommandState' n
--       buildCommandState' (Free (Shoot n)) = do
--         bcsWantsShoot .= True
--         buildCommandState' n
--       buildCommandState' (Pure a) = return a
    
-- data MoveEvent = MoveEvent (Float, Float) | StopMove
-- data FireEvent = FireEvent

-- data WorldEvents = WorldEvents
--  { _weMove :: Map.Map EntityId (Event MoveEvent)
--  , _weFire :: Map.Map EntityId (Event FireEvent)
--  }
-- newWorldEvents = WorldEvents Map.empty Map.empty 
-- makeLenses ''WorldEvents

-- data BoomWorld = BoomWorld { _bDefaultWorld :: !DefaultWorld
--                            , _bWalls        :: !(Map.Map (Int, Int) Wall)
                           -- , _bCommandState :: !BoomCommandState
                           -- , _bEntityWires  :: !(WorldWire EntityId ())
initPlayer :: RenderControl EntityData
initPlayer = do
  player1MainRu <- getSpriteRenderUnit "Main1"
  player2MainRu <- getSpriteRenderUnit "Main2"

  mainBody <- getSprite "boom" "blue_mid"
  feet <- getSprite "boom" "blue_buttom"
  gun <- getSprite "boom" "blue_gun"

  p1Body <- addSprite player1MainRu mainBody (0, 0) 0
  p2Body <- addSprite player2MainRu mainBody (0, 0) 0

  p1Feet <- addSprite player1MainRu feet (0, 0) 0
  p2Feet <- addSprite player2MainRu feet (0, 0) 0

  p1Gun <- addSprite player1MainRu gun (0, 0) 0
  p2Gun <- addSprite player2MainRu gun (0, 0) 0
  
  return $ EntityData (p1Body, p1Feet, p1Gun) (p2Body, p2Feet, p2Gun) Map.empty

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
initRender :: Game -> RenderControl EntityData
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
  mapM (flip setCamera $ cam1) [player1FloorRu, player1MainRu]
  mapM (flip setCamera $ cam2) [player2FloorRu, player2MainRu]

  mainWall <- getSprite "boom" "wallBasic"
              
  let g = mkStdGen 0
  let coords = evalState gen g 
               

  traceShow coords $ mapM_ (\(x, y) -> 
         addSprite player1FloorRu mainWall (fromIntegral $ x*32, fromIntegral $ y*32) 0
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

newGame :: GLFW.Window -> IO (Game)
newGame win = do
  (renderer, renderManager) <- runStateT (newRenderer win) newRenderManager
  let game = Game { _gameWorlds = Map.empty
--                         , _gameWires = Map.empty
                  , _gamePushWorld = Map.empty
                        , _gameRenderManager = renderManager
                        , _gameRenderer = renderer
                        , _gameEntityData = Nothing
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
                  (entityData, newGameRenderer) <- lift $ runStateT (runRenderControl (initRender newGame)) (newGame^.gameRenderer)
                  gameEntityData .= Just entityData
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
     
movement :: InputFRP (Event (Free BoomActions ()))
movement = do
  let keys = [GLFW.Key'W, GLFW.Key'A, GLFW.Key'S, GLFW.Key'D, GLFW.Key'Space]
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
  
newtype Tick = Tick Float deriving (Show)
     
gameSync vGame inputGame inputGame2 outputGame = do
  game <- atomically . readTVar $ vGame
  deltaEv <- sync $ enterTheGame (fromJust $ game^.gameWorlds.at PlayerOne)
  deltaEv2 <- sync $ enterTheGame (fromJust $ game^.gameWorlds.at PlayerTwo)

  vDelta1 <- newIORef (deltaId)
  vDelta2 <- newIORef (deltaId)

  unlisten <- sync $ do
    S.listen deltaEv (\delta -> do
            modifyIORef vDelta1 (\old -> old >> delta)
            )

  unlisten2 <- sync $ do
     S.listen deltaEv2 (\delta -> do
            modifyIORef vDelta2 (\d -> d >> delta)
            -- old <- readIORef vDelta2
            -- writeIORef vDelta2 (old >> delta)
      )

  runEffect $ (fromInput (inputGame `mappend` inputGame2)) >-> gameLoop vDelta1 vDelta2 vGame >-> toOutput outputGame
  performGC
     
-- gameLoop :: Event ([BoomWorldDelta]) -> Output [BoomWorldDelta] -> Game -> Consumer (Either (Free BoomActions ()) (Tick)) IO ()
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

gameLoop vDelta1 vDelta2 vGame = do
  commands <- await

  game <- lift $ atomically $ readTVar vGame

  let Just world1' = game^.gameWorlds.at PlayerOne
  let Just world2' = game^.gameWorlds.at PlayerTwo
  world1 <- lift . sync $ sample world1'
  world2 <- lift . sync $ sample world2'
  
  case commands of
    Left commands' -> do
      lift . sync $ syncEvents commands' world1
    Right (Tick dt) -> lift . sync $ (world1^.bInput.bwSendTick) (4)
  
  delta1 <- lift $ readIORef vDelta1
  delta2 <- lift $ readIORef vDelta2
  lift $ writeIORef vDelta1 (deltaId)
  lift $ writeIORef vDelta2 (deltaId)
  
  (rc, pendingActions1, pendingActions2) <- lift $ catchAny (do
    let Just pushGame1 = game^.gamePushWorld.at PlayerOne
    let Just pushGame2 = game^.gamePushWorld.at PlayerTwo

    sync $ do
      -- apply changes
      let (bw1, bw2, rc) = applyDelta delta1 delta2 world1 world2

      let pA1 = bw1^.bPendingActions
      let pA2 = bw2^.bPendingActions

      let bw1' = bw1 & bPendingActions .~ []
      let bw2' = bw2 & bPendingActions .~ []

      pushGame1 bw1'
      pushGame2 bw2'

      return (rc, pA1, pA2)

    ) $ \e -> do
      print e
      return (return (), [], [])
  
  mapM (lift . sync) pendingActions1
  mapM (lift . sync) pendingActions2

  rc `seq` P.yield rc

  gameLoop vDelta1 vDelta2 vGame
  
type RC = StateT Game RenderControl ()
collectCommands :: Double -> RC -> Pipe RC RC IO ()
collectCommands start d = do
  s <- await
  Just end <- lift $ GLFW.getTime
  if realToFrac (end - start) > tickrate / 1000 then do
    let x = (d >> s) in x `seq` P.yield x
    collectCommands (end + (end - start) - 0.016) (return ())
  else do
    collectCommands start (d >> s)
  
         
renderLoop :: TVar Game -> Consumer (StateT Game RenderControl ()) IO ()
renderLoop vGame = do
  inp <- await
  -- lift $ GLFW.getTime >>= print
  --let Just entData = game^.gameEntityData
  --let !renderControls = handleEvents entData inp (Pure ())
  
  --rc <- lift . atomically $ modifyTVar vGame (execStateT inp)
  game <- lift . atomically $ readTVar vGame
  
  let rc = execStateT inp game
  
  -- StateT Game RenderControl ()
  -- RenderControl -> StateT Renderer IO a

  Just t1 <- lift GLFW.getTime
  (newGame, newGameRenderer) <- lift $ runStateT (runRenderControl rc) (game^.gameRenderer)
  Just t2 <- lift GLFW.getTime
  lift $ render newGameRenderer
  Just t3 <- lift GLFW.getTime
  -- lift $ print $ map (*1000) [t2 - t1, t3 - t2, t3 - t1]

  lift . atomically $ modifyTVar vGame (\game -> newGame & gameRenderer .~ newGameRenderer)

  renderLoop vGame 
  
tickrate :: Float
tickrate = 16

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
        lift $ stepInput
        genTicks' newTime (dt' `mod` round tickrate)
  
loopInput var outputInput2 = do
  f <- lift $ atomically $ readTQueue var
  f `seq` P.yield (Left f) >-> (toOutput outputInput2)
  loopInput var outputInput2
  
inputSync keyInput outputInput2 = do
    var <- newTQueueIO
    syncInput keyInput $ do
        e <- movement
        lift $ S.listen e (\f -> do
          atomically $ writeTQueue var f
          )
    runEffect $ loopInput var outputInput2
    
tickSync outputInput stepInput = do
    runEffect $ for (genTicks stepInput) (P.yield . Right) >-> (toOutput outputInput)
    performGC

runGame win = do
  GLFW.swapInterval 1

  (eT, pushT) <- sync S.newEvent

  (keyInput, stepInput) <- initInput win
  -- a <- syncInput input $ keyDown GLFW.Key'Escape
  doQuit <- syncInput keyInput mkDoQuit
  
  (outputInput, inputGame) <- spawn Unbounded
  (outputInput2, inputGame2) <- spawn Unbounded
  (outputGame, renderInput) <- spawn Unbounded


  print "Print this to avoid segmentation fault"
  (game) <- newGame win
  
  g <- newTVarIO game
  
  -- input loop
  _ <- async $ inputSync keyInput outputInput2 
  _ <- async $ tickSync outputInput stepInput
   
  -- render loop
  -- _ <- async $ do
  --   runEffect $ fromInput renderInput >-> renderLoop game
  --   performGC
  
 
  -- game loop
  tId <- async $ gameSync g inputGame inputGame2 outputGame
  
  Just t <- GLFW.getTime
  runEffect $ fromInput renderInput >-> collectCommands t (return ()) >-> renderLoop g

  wait tId 
 
  -- sync $ do
  --   -- e <- keyDownFor 1 eT GLFW.Key'A eKey 
  --   Sk.listen e print

  -- let session = newSession
  -- stdGen <- getStdGen
  -- (a, stdGen') <- randomR (0, 100) stdGen
  -- print "initialized game"
  return ()
  


  -- Just startTime <- GLFW.getTime
  -- loop (LoopConstData win eKey doQuit pushMove1) game startTime pushT
  where
    loop cd@(LoopConstData win eKey doQuit pushMove1) !game lastTime pushT = do
        threadDelay 100000
        -- (dt, session') <- stepSession session

        -- | Update game
        -- let Just world1 = game^.gameWorlds.at PlayerOne
        -- let world1 = world1' & bCommandState %~ (buildCommandState userCommands1)
        -- let Just world2 = game^.gameWorlds.at PlayerTwo
        -- let world2 = world2' & bCommandState %~ (buildCommandState userCommands2)
  
        -- note: this recreates the entire frp framework
        --   we dont want to do this for const data in world1 (when event sources etc. dont change)
        -- evts <- sync $ runReaderT movementG world1
        -- unlisten <- sync $ S.listen evts $ \free -> do
        --   let (eventList1, !newWorld1) = runState (applyDelta free) world1
        --   -- let (eventList2, !newWorld2) = runState (applyDelta freeList2) world2
        --   print eventList1
        --sync $ pushMove1 (MoveEvent (50, 50))
        -- sync $ do
          -- deltaEvents <- (mkDeltaEvents world1)
          -- S.listen deltaEvents print
          
        -- sync $ (world1^.bInput.bwSendMoveEvent) (MoveEvent (50, 50))
        -- sync $ (world1^.bInput.bwSendTick) (0.5)
        -- -- unlisten
 
        -- let (freeList1, freeList2) = ([], [])

        -- let Just g = game^.gameEntityData

        -- -- let (eventList1, !newWorld1) = runState (applyDelta freeList1) world1
        -- -- let (eventList2, !newWorld2) = runState (applyDelta freeList2) world2
  
        -- -- let !renderControls = handleEvents g eventList1 eventList2

        -- let !game' = game
        -- -- let !game' = game & gameWires.at PlayerOne .~ Just newWire1
        -- --                  & gameWires.at PlayerTwo .~ Just newWire2
        -- --                  & gameWorlds.at PlayerOne .~ Just newWorld1
        -- --                  & gameWorlds.at PlayerTwo .~ Just newWorld2
        --             -- gameWorlds.at PlayerOne .~

        -- -- !newGameRenderer <- execStateT (runRenderControl renderControls) (game'^.gameRenderer)
        -- -- let !game'' = game' & gameRenderer .~ newGameRenderer
        -- let game'' = game'
  
        -- Just newTime <- GLFW.getTime
        -- let timeDelta = newTime - lastTime
        -- sync . pushT $ Time timeDelta

        -- -- | update game
        -- -- render (game''^.gameRenderer)

        -- quit <- sync . sample $ doQuit
        -- unless quit $ loop cd game'' newTime pushT

-- type BoomWorldFRP a = ReaderT BoomWorld Reactive a
-- type DeltaProgram a = Free BoomWorldDelta a
-- type DeltaProgramEvent = Event (DeltaProgram ())
     
mergeAll :: [Event a] -> Event a
mergeAll events = foldr (merge) never events
         
-- movementG :: BoomWorldFRP DeltaProgramEvent
-- movementG = do
--   moveEventSources <- view $ bWorldEvents .weMove . L.to Map.elems
--   let move = mergeAll moveEventSources
  
--   let free = fmap (\(MoveEvent dir) -> do 
--                                         mov <- defaultWorld (undefined::BoomWorld) $ do
--                                                                                         Just eId <- getEntityByName "Player"
--                                                                                         Just mov <- mkMovable eId
--                                                                                         return mov
--                                         Sys.setComponent (undefined::BoomWorld) mov (Position dir)) move
  
--   return $ coalesce (>>) free

-- fire :: BoomWorldFRP DeltaProgramEvent
-- fire = do
--   fireEventSources <- view $ bWorldEvents . weFire . L.to Map.elems
--   let spawnEntityEvent = mergeAll fireEventSources
--   let se = execute . fmap (\entityId -> do
--                             (eSource, pushE) <- newEvent
--                             return eSource) $ spawnEntityEvent
--   return $ fmap (\eSource -> do
--     eId <- defaultWorld (undefined::BoomWorld) $ spawnMovable "Player2" (100, 100) 0
--     addFireEventSource eId eSource) se

-- gameFRP :: BoomWorldFRP DeltaProgramEvent 
-- gameFRP = do
--   e1 <- movementG
--   e2 <- fire
--   return $ mergeWith (>>) e1 e2
  
