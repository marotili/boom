{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DeriveDataTypeable       #-}
{-# LANGUAGE StandaloneDeriving       #-}
module CleanFRP 
( BoomWorld(..)
, bInput
, bRotations
, bwSendMoveEvent
, bwLookAtEvent
, MoveEvent(..)
, LookEvent(..)
, bwSendFire
, Game(..)
, EntityData(..)
, newBoomWorld
, gameWorlds
, gamePushWorld
, gameRenderer
, gameEntityData
, FireEvent(..)
, EntityManager(..)
, Player(..)
  
, deltaId
, bwSendTick
, flattenDelta
, gameEntityManager
, collisionSystem
, applyDelta
, bPendingActions
, gameRenderManager
, merge
, mergeAll
, bwSendLookAtEvent
  
-- Gameplay
, MoveEvents(..)
, F, WrapB(..), WrapR(..)
  
, spawnEntity
, deltaRotate
, deltaMove
, accum
, BoomWorldDelta
, Behavior
, EntityId
, bwFire
, bwTick
, deltaRender
, deltaRenderAdd
, merge3
, merge4
, bPositions
, removeEntity
, bwMoveEvents
) where

import           Prelude
import           System.Exit
import           Linear
import           GHC.Float
import           Data.Octree (Vector3 (..))
import           Data.Typeable
import           Control.Monad.Free
import           Control.Applicative
import           Control.Lens hiding (both, coerce)
import           Control.Monad.RWS.Strict hiding (listen)
import qualified Control.Monad.RWS.Strict as State
import           Data.IORef
import           Debug.Trace
                 
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe
import           Collision
import           FRP.Sodium hiding (merge, accum)
import qualified FRP.Sodium as S
import qualified FRP.Sodium.Context as SC
import           Control.Monad.State.Strict
import           Render.Halo
import qualified Render.Halo as H

import qualified Language.Haskell.Interpreter as I
import           Language.Haskell.Interpreter hiding (lift)
import           Language.Haskell.Interpreter.Unsafe
       
accum :: a -> Event (a -> a) -> Reactive (Behavior a)                 
accum z efa = S.accum z . split . coalesce mappend . fmap (:[]) $ efa

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
     
data LookEvent = 
  LookAt (Float, Float)
  | StopLookAt

-- type WorldWire a b = Sys.WorldWire BoomWorld a b
data BoomWorldInput = BoomWorldInput
  { _bwMoveEvents :: Event (MoveEvent Move)
  , _bwSendMoveEvent :: MoveEvent Move -> Reactive ()
  , _bwLookAtEvent :: Event LookEvent
  , _bwSendLookAtEvent :: LookEvent -> Reactive ()
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
  (la, sla) <- newEvent
  return $ BoomWorldInput me sme la sla fe sfe te ste
  
-- type RenderInfo = (SpriteRenderUnit, 

data BoomWorld = BoomWorld
  { _bPositions :: Component (Float, Float)
  , _bRotations :: Component Float
  , _bBoundarySizes :: Component (Float, Float)
  , _bOctree :: GameOctree
  
  , _bCollision :: Event (EntityId, EntityId)
  
  , _bInput :: BoomWorldInput
  , _bNextEntityId :: EntityId
  
  , _bPendingActions :: [Reactive ()]
  } deriving (Typeable)
deriving instance Typeable Plain
deriving instance Typeable SC.Behavior
deriving instance Typeable SC.Event
deriving instance Typeable SC.Reactive
  
data BoomWorldDelta' next =
  DeltaMove EntityId (Float, Float) next
  | DeltaRotate EntityId Float next
  | DeltaBoundary EntityId (Float, Float) next
  | DeltaSpawn (EntityId -> Reactive ()) (EntityId -> next)
  | DeltaRemoveEntity EntityId next
  | DeltaId next
  | DeltaRenderAdd EntityId ((EntityId, SpriteInstanceId) -> Reactive ()) (RenderControl SpriteInstanceId) (SpriteInstanceId -> next)
  | DeltaRender (RenderControl ()) next
  deriving (Functor, Typeable) 
  
data BoomWorldDeltaApplied =
  DeltaMove' EntityId (Float, Float)
  | DeltaRotate' EntityId Float
  | DeltaBoundary' EntityId (Float, Float)
  | DeltaSpawn' EntityId (EntityId -> Reactive ())
  | DeltaRemoveEntity' EntityId
  | DeltaRenderControl' (RenderControl ())
  | DeltaNewSpriteId' (EntityId, SpriteInstanceId) ((EntityId, SpriteInstanceId) -> Reactive ())
  
instance Show BoomWorldDeltaApplied where
  show (DeltaMove' eId pos) = show ("DeltaMove'", eId, pos)
  show (DeltaRotate' eId pos) = show ("DeltaRotate'", eId, pos)
  show (DeltaBoundary' eId pos) = show ("DeltaBoundary'", eId, pos)
  show (DeltaSpawn' eId _) = show ("DeltaSpawn'", eId)
  show (DeltaRemoveEntity' eId) = show ("DeltaRemoveEntity'", eId)
  show _ = "Delta"
 
instance Show a => Show (BoomWorldDelta' a) where
  show ((DeltaMove eId pos n)) = "DeltaMove " ++ show (eId, pos) ++ "\n" ++ show n
  show ((DeltaRotate eId pos n)) = "DeltaRotate " ++ show (eId, pos) ++ "\n" ++ show n
  show ((DeltaBoundary eId pos n)) = "DeltaBoundary " ++ show (eId, pos) ++ "\n" ++ show n
  show ((DeltaSpawn _ g)) = "DeltaSpawn\n" ++ show (g 0)
  show ((DeltaRemoveEntity eId n)) = "DeltaRemoveEntity " ++ show (eId) ++ "\n" ++ show n
  show ((DeltaId n)) = show n
  show _ = "Delta"

data EntityData = EntityData
  { _p1Sprites :: (SpriteInstanceId, SpriteInstanceId, SpriteInstanceId)
  , _p2Sprites :: (SpriteInstanceId, SpriteInstanceId, SpriteInstanceId)
  , _edBullets :: Map.Map EntityId SpriteInstanceId
  }

data EntityManager = EntityManager
  { _emNextEntityId :: EntityId
  } deriving Show
     
data Game = Game
     { _gameWorlds        :: !(Map.Map Player (Behavior BoomWorld))
     , _gamePushWorld :: Map.Map Player (BoomWorld -> Reactive ())
     -- , _gameWires         :: !(Map.Map Player (WorldWire () ()))
     , _gameRenderManager :: !RenderManager
     , _gameRenderer      :: !Renderer
     , _gameEntityData :: !(Maybe EntityData)
     , _gameEntityManager :: !(EntityManager)
     }
 
makeLenses ''EntityManager
 
makeLenses ''Game
makeLenses ''EntityData

instance Show Game where
    show g = show "Game" -- (g^.gameWorlds)

deltaId :: BoomWorldDelta ()
deltaId = liftF (DeltaId ())
  
type BoomWorldDelta = Free BoomWorldDelta'
deltaMove :: EntityId -> (Float, Float) -> BoomWorldDelta ()
deltaMove eId pos = liftF (DeltaMove eId pos ())
          
deltaRotate :: EntityId -> Float -> BoomWorldDelta ()
deltaRotate eId rot = liftF (DeltaRotate eId rot ())

spawnEntity :: (EntityId -> Reactive ()) -> BoomWorldDelta EntityId          
spawnEntity pushId = liftF (DeltaSpawn pushId id)
            
removeEntity :: EntityId -> BoomWorldDelta ()          
removeEntity eId = liftF (DeltaRemoveEntity eId ())
             
deltaRenderAdd eId pushId rc = liftF (DeltaRenderAdd eId pushId rc id)
deltaRender rc = liftF (DeltaRender rc ())

makeLenses ''BoomWorld
makeLenses ''BoomWorldInput
               
merge a b = split . coalesce mappend . fmap (:[]) $ S.merge a b
merge3 a b c = split . coalesce mappend . fmap (:[]) $ S.merge (S.merge a b) c
merge4 a b c d = split . coalesce mappend . fmap (:[]) $ S.merge (S.merge (S.merge a b) c) d
 
add (x, y) (x0, y0) = (x + x0, y + y0)
 
flattenDelta :: BoomWorldDelta a -> State (EntityManager, Renderer) [BoomWorldDeltaApplied]
flattenDelta d@(Free (DeltaMove eId pos n)) = do
    rc <- flattenDelta n
    return $ (DeltaMove' eId pos) : rc

flattenDelta d@(Free (DeltaRotate eId rot n)) = do
    rc <- flattenDelta n 
    return $ (DeltaRotate' eId rot) : rc

flattenDelta d@(Free (DeltaBoundary eId boundary n)) = do
    rc <- flattenDelta n 
    return $ (DeltaBoundary' eId boundary) : rc

flattenDelta d@(Free (DeltaSpawn r g)) = do
    eId <- use $ _1.emNextEntityId
    _1.emNextEntityId += 1
    rc <- flattenDelta (g eId) 
    return $ (DeltaSpawn' eId r) : rc

flattenDelta  d@(Free (DeltaRemoveEntity eId n)) = do
    rc <- flattenDelta n
    return $ (DeltaRemoveEntity' eId) : rc

flattenDelta  d@(Free (DeltaId n)) = do
    rc <- flattenDelta n
    return $ rc
    
flattenDelta d@(Free (DeltaRenderAdd eId pushRea rc f))= do
    renderer <- use _2
    let (rcResult, renderer') = runState (getRenderControlResult rc) renderer
    _2 .= renderer'
    res <- flattenDelta (f rcResult) 
    return $ (DeltaNewSpriteId' (eId, rcResult) pushRea):(DeltaRenderControl' (void rc)):res

flattenDelta (Free (DeltaRender rc n)) = do
    res <- flattenDelta n              
    return $ (DeltaRenderControl' rc):res

flattenDelta  d@(Pure _) = do
    return []

applyDelta :: [BoomWorldDeltaApplied] -> [BoomWorldDeltaApplied]
           -> BoomWorld -> BoomWorld
           -> (BoomWorld, BoomWorld, RenderControl ()) -- (BoomWorld, BoomWorld)

applyDelta changeList1 changeList2 bw1 bw2 = (bw1', bw2',
  --handleEvents changeList1 changeList2
  foldr (>>) (Pure ()) . catMaybes . map collectRC $ changeList1 ++ changeList2
  )
  --return ())

  where
    collectRC (DeltaRenderControl' rc) = Just rc
    collectRC _ = Nothing
    (_, bw1') = runState (mapM_ applyDelta' changeList1) bw1
    (_, bw2') = runState (mapM_ applyDelta' changeList2) bw2
    applyDelta' :: BoomWorldDeltaApplied -> State BoomWorld ()
    applyDelta' d@((DeltaMove' id pos)) = do
      let mod (Just oldPos) = Just $ add oldPos pos
          mod (Nothing) = Just pos
      bPositions %= Map.alter mod id

    applyDelta' d@((DeltaRotate' eId rot)) = do
      let mod (Just oldRot) = Just $ oldRot + rot
          mod (Nothing) = Just rot
      bRotations %= Map.alter mod eId

    applyDelta' d@((DeltaBoundary' eId boundary)) = do
     return ()

    applyDelta' d@((DeltaSpawn' eId r)) = do
      bPendingActions %= mappend [(r eId)]
    
    applyDelta' (DeltaNewSpriteId' sId r) = do
      bPendingActions %= mappend [(r sId)]
  
    applyDelta' (DeltaRenderControl' _) = do
      return ()
  
    applyDelta' d@((DeltaRemoveEntity' eId)) = do
      return ()
  
collisionSystem :: [BoomWorldDeltaApplied] -> RWS () [BoomWorldDeltaApplied] GameOctree ()
collisionSystem deltas = mapM_ collisionSystem' deltas               
  where
    collisionSystem' :: BoomWorldDeltaApplied -> RWS () [BoomWorldDeltaApplied] GameOctree ()
    collisionSystem' f@((DeltaMove' id (dx, dy))) = do
      mObj <- use $ octreeObject id
      case mObj of
        Just obj -> do
            let transaction = do
                  octreeUpdate [obj & ooPosition %~ (\(x, y) -> (x + dx, y + dy))]
                  octreeQueryObject id
            octree <- State.get
            let (collisions, newOctree) = runState transaction octree
            if collisions == [] then do
              put newOctree
              tell $ [(DeltaMove' id (dx, dy))]
            else
              -- lift $ liftF (DeltaCollision collisions)
              return ()
        Nothing -> tell [(DeltaMove' id (dx, dy))]
  
    collisionSystem' s@(DeltaSpawn' eId _) = do
      modify (execState $ octreeUpdateInsert [(eId, (0, 0), [(0, 5), (5, 5), (5, 0), (0, 0)])])
      tell [s]
      return ()

    collisionSystem' delta = tell [delta]
        
run :: IO F
run = do
  -- r <- runInterpreter initModule
  r <- unsafeRunInterpreterWithArgs ["-package-db .cabal-sandbox/x86_64-linux-ghc-7.8.2-packages.conf.d/"] initModule
  print "it worked"
  case r of
    Left err -> do
      print err
      exitFailure
      return $ const (WrapR $ return (never, const (return ())))
    Right func -> 
      return $ func

newtype WrapB = WrapB { unWrapB :: Behavior BoomWorld } deriving (Typeable)
newtype WrapR = WrapR { unWrapR :: Reactive (Event (BoomWorldDelta ()), () -> Reactive ()) } deriving (Typeable)
type F = WrapB -> WrapR
     

initModule :: Interpreter F
initModule = do
  I.set [languageExtensions := 
    [RecursiveDo
    ]]
  setImports ["Prelude"]
  loadModules ["Gameplay"]
  setTopLevelModules ["Gameplay"]
--  fun <- interpret "enterTheGame" (as :: SC.Behavior Plain BoomWorld -> Reactive (Event (BoomWorldDelta ())))
  t <- interpret "enterTheGame" (as :: F)
  -- let fun = const (return never) :: F
  return t

  
newBoomWorld :: IO (Behavior BoomWorld, BoomWorld -> Reactive ())
newBoomWorld = do
  input <- newBoomWorldInput

  sync . newBehavior $ BoomWorld (Map.fromList [(1, (0, 0))]) Map.empty Map.empty newOctree never input 2 []
  
  where
    initialCommands = do
      eId <- spawnEntity (const (return ()))
      _ <- deltaMove eId (20, 20)
      return ()

mergeAll :: [Event a] -> Event a
mergeAll events = S.split $ fmap (:[]) $ foldr (S.merge) never events
 
