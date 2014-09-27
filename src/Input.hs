{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Input
( --( userInputWire1
-- , userInputWire2
-- , runInput
 BoomCommands
, BoomActions (..)
)
where

import           Control.Monad.Free
import           Control.Monad.RWS.Strict hiding (when)
import           Control.Wire
import qualified Graphics.UI.GLFW         as GLFW
import           Prelude                  hiding ((.))

import           HSys                     hiding (InputWire, Move, move)

import           Linear

type Direction = (Float, Float)

data BoomActions next =
    Move !Direction !next
    | StopMove !next
    | LookAt !(Float, Float) !next
    | StopLookAt !next
    | Shoot !next
    deriving (Show, Functor)

type BoomCommands a = Free BoomActions a

-- type Time = Timed NominalDiffTime ()
-- type InputMonad = RWS UserInput (BoomCommands ()) ()
-- type InputWire a b = Wire Time () InputMonad a b

smallVector :: V2 Float -> Bool
smallVector (V2 dx dy) = abs dx < 0.005 && abs dy < 0.005

-- move :: InputWire (V2 Float) () --BoomCommands ()
-- move = mkGen_ (\v@(V2 x y) -> v `seq` if (not . smallVector $ v)
--                            then do
--                              tell . liftF $ Move (x, y) ()
--                              return $ Right ()
--                            else return $ Left ()
--               )

-- stopMove :: InputWire () ()
-- stopMove = mkGen_ . const . fmap Right . tell . liftF $ StopMove ()

-- lookAt :: Direction -> InputWire () ()
-- lookAt dir = mkGen_ . const . fmap Right . tell . liftF $ LookAt dir ()

-- shoot :: InputWire () ()
-- shoot = mkGen_ . const . fmap Right . tell . liftF $ Shoot ()

-- userInputWire1 :: InputWire () ()
-- userInputWire1 = moveAction
--     where
--       moveAction = void (when smallVector) . liftA2 (+) directionX directionY
--                    --> move . liftA2 (+) directionX directionY
--                    --> inhibit () . asSoonAs . notYet . now . stopMove --> moveAction

-- userInputWire2 :: InputWire () ()
-- userInputWire2 = pure ()

-- directionX :: InputWire a (V2 Float)
-- directionX = pure (V2 1 0) . keyDown GLFW.Key'A . keyUp GLFW.Key'D <|>
-- 		pure (V2 (-1) 0) . keyDown GLFW.Key'D . keyUp GLFW.Key'A <|>
-- 		pure 0

-- directionY :: InputWire a (V2 Float)
-- directionY = pure (V2 0 (-1)) . keyDown GLFW.Key'W . keyUp GLFW.Key'S <|>
-- 		pure (V2 0 1) . keyDown GLFW.Key'S. keyUp GLFW.Key'W <|>
-- 		pure 0

-- -- runInput :: IO (InputWire () (), BoomCommands ())
-- -- runInput :: InputWire () () -> InputWire () () -> Time -> UserInput -> (InputWire () (), BoomCommands ())
-- runInput inputWire1 inputWire2 dt input =
--   let ((_, newWire1), _, freeInput1) = runRWS (stepWire inputWire1 dt (Right ())) input ()
--       ((_, newWire2), _, freeInput2) = runRWS (stepWire inputWire2 dt (Right ())) input ()
--   in (newWire1, newWire2, freeInput1, freeInput2)


