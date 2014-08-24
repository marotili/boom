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
module Gameplay where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Maybe
import Prelude
import CleanFRP
import           FRP.Sodium hiding (merge, accum, Behavior)
import qualified FRP.Sodium as S
import qualified FRP.Sodium.Context as SC
import Prelude
import System.Exit
import Data.Typeable
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
import qualified FRP.Sodium as S
import qualified FRP.Sodium.Context as SC
import           Control.Monad.State.Strict
import Render.Halo                
import qualified Render.Halo as H

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe
import qualified Language.Haskell.Interpreter as I

type Behavior = SC.Behavior Plain

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

runTest :: IO ()
runTest = print "test"

-- enterTheGame :: Behavior BoomWorld -> Reactive (Event (BoomWorldDelta ()))
enterTheGame :: BBW -> REBWD
enterTheGame (BBW bBw) = REBWD $ do
  bw <- sample bBw
  let input = bw^.bInput
  speed <- (moveDirection $ input^.bwMoveEvents)
  let movement = snapshot (\tick (vx, vy) -> (vx*tick, vy*tick)) (input^.bwTick) speed
  let moveEvents = deltaMove 1 <$> movement :: Event (BoomWorldDelta ())
  
  behMov <- spawnBullets bBw 
  
  let e = mergeWith (>>) behMov (moveEvents) 
  return e
         
moveDirection :: Event (MoveEvent Move) -> Reactive (Behavior (Float, Float))   
moveDirection eMove = hold (0, 0) $ fmap speed eMove
  where speed :: MoveEvent Move -> (Float, Float)
        speed (MoveEvent dir) = dir
        speed (StopMoveEvent) = (0, 0)              
  
