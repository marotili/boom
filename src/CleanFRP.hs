{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
module CleanFRP where

import           Control.Monad.Free
import           Control.Applicative
import           Control.Lens hiding (both, coerce)
import           Control.Monad.RWS.Strict hiding (listen)
import           Data.IORef
import           Debug.Trace
                 
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Collision
import           FRP.Sodium hiding (merge, accum)
import qualified FRP.Sodium as S
import           Control.Monad.State.Strict
import Render.Halo                
import qualified Render.Halo as H
                 
accum :: a -> Event (a -> a) -> Reactive (Behavior a)                 
accum z efa = S.accum z $ split . coalesce mappend . fmap (:[]) $ efa

type EntityId = Int
type Component a = Map.Map EntityId a
 
data MoveEvents =
  SetPosition
  | Move
 
data MoveEvent :: MoveEvents -> * where
  SetPositionEvent :: (Float, Float) -> MoveEvent SetPosition
  MoveEvent :: (Float, Float) -> MoveEvent Move
  StopMoveEvent :: MoveEvent Move
  
data FireEvent where
  FireEvent :: FireEvent

data Player =
     PlayerOne
     | PlayerTwo
     deriving (Eq, Ord, Show)

-- type WorldWire a b = Sys.WorldWire BoomWorld a b
data BoomWorldInput = BoomWorldInput
  { _bwMoveEvents :: Event (MoveEvent Move)
  , _bwSendMoveEvent :: MoveEvent Move -> Reactive ()
  , _bwFire :: Event FireEvent
  , _bwSendFire :: FireEvent -> Reactive ()
  , _bwTick :: Event Float
  , _bwSendTick :: Float -> Reactive ()
  }
  
newBoomWorldInput :: IO BoomWorldInput
newBoomWorldInput = sync $ do
  (me, sme) <- newEvent
  (fe, sfe) <- newEvent
  (te, ste) <- newEvent
  return $ BoomWorldInput me sme fe sfe te ste
  
newBoomWorld :: IO (Behavior BoomWorld, BoomWorld -> Reactive ())
newBoomWorld = do
  input <- newBoomWorldInput

  sync $ newBehavior $ BoomWorld (Map.fromList [(1, (0, 0))]) Map.empty Map.empty newOctree never input 2 []

data BoomWorld = BoomWorld
  { _bPositions :: Component (Float, Float)
  , _bRotations :: Component Float
  , _bBoundarySizes :: Component (Float, Float)
  , _bOctree :: GameOctree
  
  , _bCollision :: Event (EntityId, EntityId)
  
  , _bInput :: BoomWorldInput
  , _bNextEntityId :: EntityId
  
  , _bPendingActions :: [Reactive ()]
  }
  
data BoomWorldDelta' next =
  DeltaMove EntityId (Float, Float) next
  | DeltaRotate EntityId Float next
  | DeltaBoundary EntityId (Float, Float) next
  | DeltaSpawn (EntityId -> Reactive ()) (EntityId -> next)
  | DeltaRemoveEntity EntityId next
  | DeltaId next
  deriving (Functor) 
  
data BoomWorldDeltaApplied =
  DeltaMove' EntityId (Float, Float)
  | DeltaRotate' EntityId Float
  | DeltaBoundary' EntityId (Float, Float)
  | DeltaSpawn' EntityId
  | DeltaRemoveEntity' EntityId
  deriving (Show)
  
instance Show a => Show (BoomWorldDelta' a) where
  show ((DeltaMove eId pos n)) = "DeltaMove " ++ show (eId, pos) ++ "\n" ++ show n
  show ((DeltaRotate eId pos n)) = "DeltaRotate " ++ show (eId, pos) ++ "\n" ++ show n
  show ((DeltaBoundary eId pos n)) = "DeltaBoundary " ++ show (eId, pos) ++ "\n" ++ show n
  show ((DeltaSpawn _ g)) = "DeltaSpawn\n" ++ show (g 0)
  show ((DeltaRemoveEntity eId n)) = "DeltaRemoveEntity " ++ show (eId) ++ "\n" ++ show n
  show ((DeltaId n)) = show n

data EntityData = EntityData
  { _p1Sprites :: (SpriteInstanceId, SpriteInstanceId, SpriteInstanceId)
  , _p2Sprites :: (SpriteInstanceId, SpriteInstanceId, SpriteInstanceId)
  , _edBullets :: Map.Map EntityId SpriteInstanceId
  }
     
data Game = Game
     { _gameWorlds        :: !(Map.Map Player (Behavior BoomWorld))
     , _gamePushWorld :: Map.Map Player (BoomWorld -> Reactive ())
     -- , _gameWires         :: !(Map.Map Player (WorldWire () ()))
     , _gameRenderManager :: !RenderManager
     , _gameRenderer      :: !Renderer
     , _gameEntityData :: !(Maybe EntityData)
     }
     
makeLenses ''Game
makeLenses ''EntityData

instance Show Game where
    show g = show "Game" -- (g^.gameWorlds)

deltaId :: BoomWorldDelta ()
deltaId = liftF (DeltaId ())
  
type BoomWorldDelta a = Free (BoomWorldDelta') a
deltaMove :: EntityId -> (Float, Float) -> BoomWorldDelta ()
deltaMove eId pos = liftF (DeltaMove eId pos ())

spawnEntity :: (EntityId -> Reactive ()) -> BoomWorldDelta EntityId          
spawnEntity pushId = liftF (DeltaSpawn pushId id)
            
removeEntity :: EntityId -> BoomWorldDelta ()          
removeEntity eId = liftF (DeltaRemoveEntity eId ())

makeLenses ''BoomWorld
makeLenses ''BoomWorldInput
           
data Bullets = Bullets (Map.Map EntityId Float) deriving (Show)
emptyBullets = Bullets Map.empty
removeBullet :: EntityId -> Bullets -> Bullets
removeBullet eId (Bullets m) = (Bullets $ Map.delete eId m)
             
addBullet :: EntityId -> Bullets -> Bullets
addBullet eId (Bullets m) = Bullets $ Map.insert eId 0 m
          
tickBullets :: Float -> Bullets -> Bullets
tickBullets (dt) (Bullets m) = Bullets $ Map.map (+dt) m
            
bulletsToRemove :: Bullets -> [EntityId]
bulletsToRemove (Bullets m) = Map.keys $ Map.filter (>100) m
                
moveBullets :: Float -> Bullets -> BoomWorldDelta ()
moveBullets dt (Bullets m) = sequence_ $ map (\eId -> deltaMove eId (-dt, -dt)) $ Map.keys m
                
merge a b = split . coalesce mappend . fmap (:[]) $ S.merge a b
merge3 a b c = split . coalesce mappend . fmap (:[]) $ S.merge (S.merge a b) c
           
spawnBullets :: Behavior BoomWorld -> Reactive (Event (BoomWorldDelta ()))
spawnBullets bBw = do
  bw <- sample bBw
  let input = bw^.bInput
  -- behEntities <- accum [] $ (:) <$> getPlayerPos (input^.bwFire) :: Reactive (Behavior [(Float, Float)])
  
  let eNew = execute $ spawn <$> (input^.bwFire)
  
  let f = fst <$> eNew :: Event (Event EntityId)
  bf <- hold never f :: Reactive (Behavior (Event EntityId))
  let b = switchE bf :: Event EntityId
  
  rec
    bullets <- accum emptyBullets $ merge3
      (addBullet <$> b)
      (removeBullet <$> split toRemove)
      (tickBullets <$> (input^.bwTick))

    let toRemove = bulletsToRemove <$> updates bullets
  let movement = snapshot moveBullets (input^.bwTick) bullets

  -- let behMovement = snapshot (\_ entities -> entities) (input^.bwTick) behEntities
  -- let behMov = fmap (\poss -> sequence_ $ map (do
  --   eId <- spawnEntity
  --   deltaMove eId
  --   ) poss) behMovement :: Event (BoomWorldDelta ())
  
  return $ merge3 (snd <$> eNew) movement ((sequence_ . fmap removeEntity) <$> toRemove)
           
  where
    when :: Event a -> Getter BoomWorld b -> Event b
    when evt g = snapshot (\_ w -> w^.g) evt bBw

    getPlayerPos :: Event a -> Event (Float, Float)
    getPlayerPos evt = when evt $ bPositions.at 1.to fromJust
  
    getPlayerPos' = sample bBw >>= \w -> return $ w^.bPositions.at 1.to fromJust
  
    spawn _ = do
        (evEntityId, pushEntityId) <- newEvent
        startPos <- getPlayerPos'

        return (evEntityId, do
          _ <- spawnEntity pushEntityId
          return ()
          )

enterTheGame :: Behavior BoomWorld -> Reactive (Event (BoomWorldDelta ()))
enterTheGame bBw = do
  bw <- sample bBw
  let input = bw^.bInput
  speed <- (moveDirection $ input^.bwMoveEvents)
  let movement = snapshot (\tick (vx, vy) -> (vx*tick, vy*tick)) (input^.bwTick) speed
  let moveEvents = deltaMove 1 <$> movement :: Event (BoomWorldDelta ())
  
  behMov <- spawnBullets bBw 
  
  let e = mergeWith (>>) behMov (moveEvents) 
  return e
  
add (x, y) (x0, y0) = (x + x0, y + y0)
 
handleEvents :: [BoomWorldDeltaApplied] -> [BoomWorldDeltaApplied] -> StateT Game RenderControl ()
handleEvents  eventList1 eventList2 = do
  mapM_ handleEvent1 eventList1
  mapM_ handleEvent2 eventList2
  -- return $ rc1 >> rc2
  where
    handleEvent1 ((DeltaMove' 1 (pos))) = do
      (EntityData (p1Body, p1Feet, p1Gun) (p2Body, p2Feet, p2Gun) _) <- use $ gameEntityData.to fromJust
      lift $ do
        move p1Feet pos
        move p1Gun pos
        move p1Body pos
  
    handleEvent1 ((DeltaRotate' 1 (rot))) = do
      (EntityData (p1Body, p1Feet, p1Gun) (p2Body, p2Feet, p2Gun) _) <- use $ gameEntityData.to fromJust
      lift $ H.rotate p1Gun rot

    handleEvent1 ((DeltaSpawn' eId)) = do
      spriteInst <- lift $ do
        ru <- getSpriteRenderUnit "Bullets"
        sprite <- getSprite "boom" "bullet"
        addSprite ru sprite (0, 0) 0
      gameEntityData._Just.edBullets %= Map.insert eId spriteInst
      return ()
  
    handleEvent1 ((DeltaMove' eId pos)) = do
      Just spriteInst <- use $ gameEntityData.to fromJust.edBullets.at eId
      lift $ move (spriteInst) pos
  
    handleEvent1 ((DeltaRemoveEntity' eId)) = do
      Just spriteInst <- use $ gameEntityData.to fromJust.edBullets.at eId
      return () 
  
    handleEvent1 _ = return ()
  
    handleEvent2 ((DeltaMove' 1 (pos))) = do
      (EntityData (p1Body, p1Feet, p1Gun) (p2Body, p2Feet, p2Gun) _) <- use $ gameEntityData.to fromJust
      lift $ do
        move p2Feet pos
        move p2Gun pos
        move p2Body pos

    handleEvent2 _ = return ()

applyDelta :: BoomWorldDelta () -> BoomWorldDelta () 
           -> BoomWorld -> BoomWorld
           -> (BoomWorld, BoomWorld, StateT Game RenderControl ()) -- (BoomWorld, BoomWorld)

applyDelta changeList1 changeList2 bw1 bw2 = (bw1', bw2', do
  handleEvents deltaApplied1 deltaApplied2
  return ())
  where
    (deltaApplied1, bw1') = runState (applyDelta' changeList1) bw1
    (deltaApplied2, bw2') = runState (applyDelta' changeList2) bw2
    applyDelta' d@(Free (DeltaMove id pos n)) = do
      bPositions %= Map.update (\oldPos -> Just (add oldPos pos)) id
      rc <- applyDelta' n
      return $ (DeltaMove' id pos) : rc

    applyDelta' d@(Free (DeltaRotate eId rot n)) = do
      return ()
      rc <- applyDelta' n
      return $ (DeltaRotate' eId rot) : rc

    applyDelta' d@(Free (DeltaBoundary eId boundary n)) = do
      return ()
      rc <- applyDelta' n
      return $ (DeltaBoundary' eId boundary) : rc

    applyDelta' d@(Free (DeltaSpawn r g)) = do
      eId <- use $ bNextEntityId
      bNextEntityId += 1
      bPendingActions %= mappend [(r eId)]
      rc <- applyDelta' (g eId)
      return $ (DeltaSpawn' eId) : rc
  
    applyDelta' d@(Free (DeltaRemoveEntity eId n)) = do
      rc <- applyDelta' n
      return $ (DeltaRemoveEntity' eId) : rc
  
    applyDelta' d@(Free (DeltaId n)) = do
      rc <- applyDelta' n
      return $ rc

    applyDelta' d@(Pure _) = do
      return []
        
moveDirection :: Event (MoveEvent Move) -> Reactive (Behavior (Float, Float))   
moveDirection eMove = hold (0, 0) $ fmap speed eMove
  where speed :: MoveEvent Move -> (Float, Float)
        speed (MoveEvent dir) = dir
        speed (StopMoveEvent) = (0, 0)              
  
-- test = do
--   (bw, pW) <- newBoomWorld
--   ev <- sync $ enterTheGame bw
--   w <- sync $ sample bw
--   r <- newIORef []
--   sync $ listen ev $ \delta -> do
--     writeIORef r delta
--   sync $ (w^.bInput.bwSendMoveEvent) (MoveEvent (5, 5))
--   sync $ (w^.bInput.bwSendFire) ()
--   sync $ (w^.bInput.bwSendTick) (16::Float)

--   delta <- readIORef r
--   let w' = execState (applyDelta delta) w
--   sync $ pW (w')
--   sync $ (w^.bInput.bwSendFire) ()

--   sync $ (w^.bInput.bwSendTick) (16::Float)
--   sync $ (w^.bInput.bwSendMoveEvent) (StopMoveEvent)
--   sync $ (w^.bInput.bwSendTick) (16::Float)
--   return ()
  
