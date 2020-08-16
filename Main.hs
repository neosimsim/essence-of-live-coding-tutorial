{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}
module Main where

-- base
import Control.Arrow
import Control.Monad
import Data.Functor
import Data.Maybe
import GHC.Float

-- vector-space
import Data.VectorSpace

-- essence-of-live-coding
import LiveCoding hiding (integrate)

-- essence-of-live-coding-gloss
import LiveCoding.Gloss

import Debug.Trace
import Data.Function ((&))
import Control.Monad.Fix (MonadFix)

type Border = (Int, Int)
initBorder :: Border
initBorder = (300, 400)

ballRadius :: Num a => a
ballRadius = 20
glossSettings :: GlossSettings
glossSettings = defaultSettings
  { debugEvents = True
  , displaySetting = InWindow "Essence of Live Coding Tutorial" (initBorder ^* 2) (20, 20)
  }

liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell $ glossWrapC glossSettings $ glossCell
  -- & (`withDebuggerC` statePlay) -- Uncomment to display the internal state

glossCell :: Cell PictureM () ()
glossCell = proc () -> do
  events <- constM ask -< ()
  border <- watchBorder -< events
  ball <- ballSim border -< events
  addPicture -< ballPic ball
  returnA    -< ()

ballPic :: Ball -> Picture
ballPic Ball { pos = (x, y) } = translate x y $ color white $ thickCircle (ballRadius / 2) ballRadius

data Ball = Ball
  { pos :: (Float, Float)
  , vel :: (Float, Float)
  } deriving Data

posX = fst . pos
posY = snd . pos
velX = fst . vel
velY = snd . vel

ballSim :: (Monad m, MonadFix m) => Border -> Cell m [Event] Ball
ballSim border = proc events -> do
  rec
    let
        borderX = int2Float $ fst border
        borderY = int2Float $ snd border
        accMouse = sumV $ (^-^ pos ball) <$> clicks events
        accCollision = sumV $ catMaybes
          [ guard (posX ball < - borderX + ballRadius && velX ball < 0) $> (-2 * velX ball, 0)
          , guard (posX ball >   borderX - ballRadius && velX ball > 0) $> (-2 * velX ball, 0)
          , guard (posY ball < - borderY + ballRadius && velY ball < 0) $> (0, -2 * velY ball)
          , guard (posY ball >   borderY - ballRadius && velY ball > 0) $> (0, -2 * velY ball)
          ]
    frictionVel <- integrate -< (-0.9) *^ vel ball
    impulses <- sumS -< sumV [accMouse, 0.97 *^ accCollision]
    let newVel = frictionVel ^+^ impulses
    newPos <- integrate -< newVel
    let ball = Ball newPos newVel
  returnA -< ball

clicks :: [Event] -> [(Float, Float)]
clicks = catMaybes . map click

click :: Event -> Maybe (Float, Float)
click event@(EventKey (MouseButton LeftButton) Down _ pos) = traceShow event $ Just pos
click event = traceShow event $ Nothing

resizes :: [Event] -> [(Int, Int)]
resizes = catMaybes . map resize

resize :: Event -> Maybe (Int, Int)
resize event@(EventResize (x, y)) = Just (x, y)
resize _ = Nothing

watchBorder :: (Monad m, MonadFix m) => Cell m [Event] Border
watchBorder = proc events -> do
  let b = listToMaybe . reverse $ resizes events
  returnA -< fromMaybe initBorder b

main :: IO ()
main = runHandlingStateT $ foreground liveProgram

-- * To be moved to main library

sumS
  :: (Monad m, Data v, VectorSpace v)
  => Cell m v v
sumS = Cell
  { cellState = zeroV
  , cellStep = \accum v -> return (accum, accum ^+^ v)
  }

integrate
  :: (Monad m, Data v, VectorSpace v, Fractional (Scalar v))
  => Cell m v v
integrate = arr (^/ fromIntegral (stepsPerSecond glossSettings)) >>> sumS
