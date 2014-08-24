-- |

module Main
( main
)
where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Render.Halo
import           Sound.ALUT

import           Game

main = do
  w <- asyncBound $ withWindow 1920 1080 "Boom" initialize
  wait w
  where initialize win = withProgNameAndArgs runALUT $ \_ _ -> runGame win
