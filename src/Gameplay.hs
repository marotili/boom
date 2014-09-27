{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs       #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE StandaloneDeriving       #-}
module Gameplay where
import           CleanFRP
import           Control.Applicative
import           Control.Lens hiding (both, coerce)
import           Control.Monad.Free
import           Control.Monad.RWS.Strict hiding (listen)
import           Data.IORef
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Typeable
import           Debug.Trace
import qualified FRP.Sodium as S
import           FRP.Sodium hiding (merge, accum, Behavior)
import qualified FRP.Sodium.Context as SC
import           Prelude
import           Prelude
import           System.Exit
import           System.Random hiding (split)
                 
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Collision
import qualified FRP.Sodium as S
import qualified FRP.Sodium.Context as SC
import           Control.Monad.State.Strict
import           qualified Control.Monad.State.Strict as State
import           Render.Halo
import qualified Render.Halo as H

import           Language.Haskell.Interpreter
import           Language.Haskell.Interpreter.Unsafe
import qualified Language.Haskell.Interpreter as I

data Bullets = Bullets 
  { bulletDuration :: (Map.Map EntityId Float)
  , bulletSpriteInstanceId :: Map.Map EntityId SpriteInstanceId
  --, bulletStartPos :: Map.Map EntityId (Float, Float)
  } deriving (Show)
  
emptyBullets :: Bullets
emptyBullets = Bullets Map.empty Map.empty
             
removeBullet :: EntityId -> Bullets -> Bullets
removeBullet eId (Bullets m sIdMap) = Bullets (Map.delete eId m) (Map.delete eId sIdMap)
             
addBullet :: EntityId -> Bullets -> Bullets
addBullet eId (Bullets m s) = Bullets (Map.insert eId 0 m) s
          
addBulletSprite :: (EntityId, SpriteInstanceId) -> Bullets -> Bullets
addBulletSprite (eId, sId) (Bullets m s) = Bullets m (Map.insert eId sId s)
          
tickBullets :: Float -> Bullets -> Bullets
tickBullets (dt) (Bullets m s) = Bullets (Map.map (+dt) m) s
            
bulletsToRemove :: Bullets -> [EntityId]
bulletsToRemove (Bullets m _) = Map.keys $ Map.filter (>100) m
                
moveBullets :: Float -> Bullets -> BoomWorldDelta ()
moveBullets dt (Bullets m sIds) = sequence_ $ map (\eId -> do
  deltaMove eId (-dt, -dt)
  let msId = sIds^.at eId
  case msId of
    Just sId -> deltaRender $ move sId (-dt, -dt)
    Nothing -> return ()
  ) $ Map.keys m

spawnBullets :: Behavior BoomWorld -> Reactive (Event (BoomWorldDelta ()))
spawnBullets bBw = do
  bw <- sample bBw
  let input = bw^.bInput
  -- behEntities <- accum [] $ (:) <$> getPlayerPos (input^.bwFire) :: Reactive (Behavior [(Float, Float)])
  
  let eNew = execute $ spawn <$> (input^.bwFire)
  
  let newEntityId = (\(a,_,_)->a) <$> eNew :: Event (Event EntityId)
  let newSpriteInstanceId = (\(_,b,_)->b) <$> eNew

  bf <- hold never newEntityId :: Reactive (Behavior (Event EntityId))
  bf2 <- hold never newSpriteInstanceId
  let newEntityIdEvent = switchE bf :: Event EntityId
  let newSpirteInstanceEvent = switchE bf2 :: Event (EntityId, SpriteInstanceId)
  
  rec
    bullets <- accum emptyBullets $ merge4
      (addBullet <$> newEntityIdEvent)
      (addBulletSprite <$> newSpirteInstanceEvent)
      (removeBullet <$> split toRemove)
      (tickBullets <$> (input^.bwTick))

    let toRemove = bulletsToRemove <$> updates bullets
  let movement = snapshot moveBullets (input^.bwTick) bullets

  -- let behMovement = snapshot (\_ entities -> entities) (input^.bwTick) behEntities
  -- let behMov = fmap (\poss -> sequence_ $ map (do
  --   eId <- spawnEntity
  --   deltaMove eId
  --   ) poss) behMovement :: Event (BoomWorldDelta ())
  
  return $ merge3 ((\(_,_,c)->c) <$> eNew) movement ((sequence_ . fmap removeEntity) <$> toRemove)
           
  where
    when :: Event a -> Getter BoomWorld b -> Event b
    when evt g = snapshot (\_ w -> w^.g) evt bBw

    getPlayerPos :: Event a -> Event (Float, Float)
    getPlayerPos evt = when evt $ bPositions.at 1.to fromJust
  
    getPlayerPos' = sample bBw >>= \w -> return $ w^.bPositions.at 1.to fromJust
  
    spawn :: a -> Reactive (Event EntityId, Event (EntityId, SpriteInstanceId), BoomWorldDelta ())
    spawn _ = do
        (evEntityId, pushEntityId) <- newEvent
        (evSpriteId, pushSpriteId) <- newEvent
        startPos <- getPlayerPos'

        return (evEntityId, evSpriteId, do
          eId <- spawnEntity pushEntityId
          sId <- deltaRenderAdd eId pushSpriteId $ do
            spr <- getSprite "boom" "bullet"
            ru <- getSpriteRenderUnit "Bullets"
            addSprite ru spr startPos 0
          _ <- deltaMove eId startPos
          return ()
          )

runTest :: IO ()
runTest = print "test"

data MyPlayer' f = MyPlayer'
  { _entityId :: f EntityId
  , _body :: f SpriteInstanceId
  , _feet :: f SpriteInstanceId
  , _gun :: f SpriteInstanceId
  }
  
makeLenses ''MyPlayer'
           
 
type MyPlayer = MyPlayer' Identity
deriving instance Show MyPlayer
type MyPlayerUnfinished = MyPlayer' Maybe 
deriving instance Show MyPlayerUnfinished

playerEntityId :: Getter MyPlayer EntityId
playerEntityId = to (runIdentity . (^.entityId))
               
playerBody :: Getter MyPlayer SpriteInstanceId
playerBody = body.to runIdentity
playerFeet = feet.to runIdentity
playerGun = gun.to runIdentity
          
          
-- | Probably not needed
class Movable a where
  doMove :: a -> (Float, Float) -> BoomWorldDelta()
  
instance Movable MyPlayer where
  doMove player dir = do deltaMove (player^.playerEntityId) dir
                         deltaRender $ do
                           move (player^.playerBody) dir
                           move (player^.playerFeet) dir
                           move (player^.playerGun) dir
  
data Wall' f = Wall'
  { _wallHP :: Int
  , _wallEntityId :: f EntityId
  , _wallSpriteId :: f SpriteInstanceId
  }
  
makeLenses ''Wall'
  
type UnfinishedWall = Wall' Maybe 
type Wall = Wall' Identity
     
deriving instance Show Wall
     
instance BasicRenderable Wall' where
  mkEmpty = Wall' 1 Nothing Nothing
  mkFull (Wall' 1 (Just eId) (Just sId)) = Just $ Wall' 1 (Identity eId) (Identity sId)
  mkFull _ = Nothing
  setEntityId eId wall = wall & wallEntityId .~ Just eId
  setSpriteInstanceId sId wall = wall & wallSpriteId .~ Just sId
        
class BasicRenderable a where
  mkEmpty :: a Maybe
  mkFull :: a Maybe -> Maybe (a Identity)
  setEntityId :: EntityId -> a Maybe -> a Maybe
  setSpriteInstanceId :: SpriteInstanceId -> a Maybe -> a Maybe
  
data WallManager = WallManager
  { _wmWalls :: Map.Map EntityId Wall
  } deriving Show
  
makeLenses ''WallManager
  
newWallManager = WallManager Map.empty
addWall w wm = wm & wmWalls.at (w^.wallEntityId.to runIdentity) .~ Just w

mkSomething :: BasicRenderable a => Event () -> String 
            -> (Float, Float) -> Float -> (Float, Float) -> Float
            -> String -> String
            -> Reactive (Event (BoomWorldDelta ()), Event (a Identity))
mkSomething init renderUnit pos rot rPos rRot atlas spriteName = do
  (evEntityId, pushEntityId) <- newEvent
  (evSpriteId, pushSpriteId) <- newEvent
  let worldDelta = do
       eId <- spawnEntity pushEntityId
       deltaMove eId pos
       deltaRotate eId rot
       deltaRenderAdd eId pushSpriteId $ do
         spr <- getSprite atlas spriteName
         ru <- getSpriteRenderUnit renderUnit
         addSprite ru spr rPos rRot
  
  something <- accum mkEmpty $ merge
    (setEntityId <$> evEntityId)
    (setSpriteInstanceId . snd <$> evSpriteId)
  
  let somethingFinal = filterJust (mkFull <$> (updates something))
  --final <- hold undefined somethingFinal
  --return (void worldDelta, final)
  return (const (void worldDelta) <$> init, somethingFinal)
  
mkWalls init renderUnit = do
  let g = mkStdGen 0
  let coords = map (\(x, y) -> (fromIntegral x, fromIntegral y)) $ evalState gen g 
  
  walls' <- mapM (\(x, y) -> mkSomething init renderUnit (x*32, y*32) 0 (x*32, y*32) 0 "boom" "wallBasic") coords

  let (inits, walls) = unzip walls'
  
  wm <- accum newWallManager $ addWall <$> (mergeAll walls)
  
  listen (updates wm) print
  
  return (foldr (mergeWith (>>)) never inits)

  where 
    gen :: State StdGen [(Int, Int)]
    gen = mapM (\_ -> do
                s <- State.get
                let (x, newS) = randomR (-10, 10::Int) s
                let (y, newS') = randomR (-10, 10::Int) newS
                put newS'
                return (x, y)
             ) [0..30::Int]



mkPlayer :: String -> Reactive (BoomWorldDelta (), Behavior (Maybe MyPlayer))
mkPlayer renderUnit = do
  (evEntityId, pushEntityId) <- newEvent
  (evBodyId, pushBodyId) <- newEvent
  (evFeetId, pushFeetId) <- newEvent
  (evGunId, pushGunId) <- newEvent
  let worldDelta = 
           do eId <- spawnEntity pushEntityId
              deltaMove eId (0, 0)
              deltaRotate eId 0
              deltaRenderAdd eId pushBodyId $ do
                spr <- getSprite "boom" "blue_mid"
                ru <- getSpriteRenderUnit renderUnit
                addSprite ru spr (0, 0) 0
              deltaRenderAdd eId pushFeetId $ do
                spr <- getSprite "boom" "blue_buttom"
                ru <- getSpriteRenderUnit renderUnit
                addSprite ru spr (0, 0) 0
              deltaRenderAdd eId pushGunId $ do
                spr <- getSprite "boom" "blue_gun"
                ru <- getSpriteRenderUnit renderUnit
                addSprite ru spr (0, 0) 0
  
  player <- accum (MyPlayer' Nothing Nothing Nothing Nothing) $ merge4
    ((\eId p -> p & entityId .~ Just eId) <$> evEntityId)
    ((\(_, bId) p -> p & body .~ Just bId) <$> evBodyId)
    ((\(_, fId) p -> p & feet .~ Just fId) <$> evFeetId)
    ((\(_, gId) p -> p & gun .~ Just gId) <$> evGunId)
  
  let playerFinal = filterJust $ fmap playerFinished (updates player)

  pl <- hold Nothing (Just <$> playerFinal)
  
  return (void worldDelta, pl)
  where
    playerFinished (MyPlayer' (Just eId) (Just body) (Just feet) (Just gun)) = 
      Just (MyPlayer' (Identity eId) (Identity body) (Identity feet) (Identity gun))
    playerFinished _ = Nothing
  
e1 $>$ e2 = mergeWith (>>) e1 e2

-- enterTheGame :: Behavior BoomWorld -> Reactive (Event (BoomWorldDelta ()))
enterTheGame :: String -> F
enterTheGame ru (WrapB bBw) = WrapR $ do
  (initialDelta, player) <- mkPlayer ru
  (initEv, init) <- newEvent

  (initialWallDelta) <- mkWalls initEv ru

  bw <- sample bBw
  let input = bw^.bInput
  speed <- (moveDirection $ input^.bwMoveEvents)
  let movement = snapshot (\tick (vx, vy) -> (vx*tick, vy*tick)) (input^.bwTick) speed
  let moveEvents = snapshot doMovement movement player :: Event (BoomWorldDelta ())
  
  behMov <- spawnBullets bBw 
  
  l <- playerLookat bBw player
  
  let e = initialWallDelta $>$ (const initialDelta <$> initEv) $>$ behMov $>$ moveEvents $>$ l

  return (e, init)
  
  where doMovement dir (Just player) = doMove player dir
        doMovement _ _ = return ()
  
playerLookat bbw behPlayer = do
  bw <- sample bbw
  let input = bw^.bInput
  listen (bw^.bInput.bwTick) (\t -> print ("tick", t))

  lookAt <- updates <$> playerLookatBeh (input^.bwLookAtEvent)
  
  listen lookAt print
  
  let rotate = snapshot getRotationTarget (playerId lookAt behPlayer) bbw
  let l = snapshot doRotate rotate behPlayer
  
  l' <- hold never ((const $ l) <$> (bw^.bInput.bwTick)) -- events only on tick
  return $ switchE $ once <$> l'

  where doRotate dir (Just player) = do deltaRotate (player^.playerEntityId) dir
                                        deltaRender $ do
                                          traceShow ("rotate", dir) $ rotate (player^.playerGun) dir
         
        getRotationTarget ((x, y), pId) bw = let
          Just lookinNow = getDirection pId bw
          target = atan2 y x
          delta = target - lookinNow
          in (delta)
 
playerId :: Event a -> Behavior (Maybe MyPlayer) -> Event (a, EntityId)
playerId e bP = filterJust $ snapshot getPlayer e bP
  where
    getPlayer a (Just player) = Just $ (a, player^.playerEntityId)
  
-- getDirection :: Getter (Behavior BoomWorld) (Event (EntityId) -> Event (Maybe Float))
-- getDirection = to (\bw -> (\e -> snapshot getDirection' e bw))
  
getDirection :: EntityId -> BoomWorld -> Maybe Float
getDirection eId bw = bw^.bRotations.at eId
 
playerLookatBeh eLookAt = hold (0, 0) $ fmap lookat $ filterE (\e -> case e of StopLookAt -> False; _ -> True) eLookAt
  where lookat (LookAt dir) = dir
        lookat (StopLookAt) = error "StopLookAt should not be reached"
         
moveDirection :: Event (MoveEvent Move) -> Reactive (Behavior (Float, Float))   
moveDirection eMove = hold (0, 0) $ fmap speed eMove
  where speed :: MoveEvent Move -> (Float, Float)
        speed (MoveEvent (x, y)) = (x, y)
        speed (StopMoveEvent) = (0, 0)              
  

enterTheGame' = return . enterTheGame --run
-- initGame :: Behavior BoomWorld -> Behavior BoomWorld -> IO GameFRP
initGame vDelta1 vDelta2 bw1 bw2 = do
  func1 <- enterTheGame' "Main1"
  func2 <- enterTheGame' "Main2"
  (deltaEv, initW1) <- sync $ unWrapR $ func1 (WrapB bw1)
  (deltaEv2, initW2) <- sync $ unWrapR $ func2 (WrapB bw2)

  unlistener1 <- sync $ do
    S.listen deltaEv (\delta -> do
      modifyIORef vDelta1 (\old -> old >> delta)
      )

  unlistener2 <- sync $ do
    S.listen deltaEv2 (\delta -> do
      modifyIORef vDelta2 (\old -> old >> delta) 
      )
  
  sync $ initW1 ()
  sync $ initW2 ()
  
  return GameFRP
    { gameFRP1 = func1
    , gameFRP2 = func2
    , unlistener1 = unlistener1
    , unlistener2 = unlistener2
    }
    
reloadGame vDelta1 vDelta2 bw1 bw2 frp = do
  unlistener1 frp
  unlistener2 frp
  
  initGame vDelta1 vDelta2 bw1 bw2

data GameFRP = GameFRP
  { gameFRP1 :: F
  , gameFRP2 :: F
  , unlistener1 :: IO ()
  , unlistener2 :: IO ()
  }
  
