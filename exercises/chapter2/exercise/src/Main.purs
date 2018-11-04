module Main where

import Prelude
import Effect (Effect)
import Effect.Console (logShow)
import Math (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * (r * r)

main :: Effect Unit
main = logShow (circleArea 3.0)