-- |

module Main
( main
)
where

import           Control.Concurrent
import           Render.Halo

import           Game

main = withWindow 1920 1080 "Boom" initialize
  where initialize win = runGame win
