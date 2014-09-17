{-# LANGUAGE RecordWildCards #-}
{-
  Copyright (C) 2014  Esteban I. Ruiz Moreno (EXio4)

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>
-}

import System.Exit
import System.Random
import Control.Concurrent

import Graphics.UI.SDL
import qualified Graphics.UI.SDL.Keycode as Key


import Coloring
import Draw

-- resolution
size :: Size
size = Size 1024 600

-- number of rectangles/squares
numSq :: Int
numSq = 7

main :: IO ()
main = withInit [InitEverything] $
    withWindow "ZyFlex" (Position 0 0) size
        [WindowShown,WindowFullscreen] $ \win ->
    withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \ren -> do
        renderClear ren
        drawBoxes size ren palette numSq (numSq+2)
        renderPresent ren
        repeatKey ren
            (renderPresent ren >> threadDelay (10^4)) -- keep the screen updated
            (efloop size ren numSq (numSq*2))
        return ()

rendN :: Renderer -> Box -> IO ()
rendN ren (Box rect (Color r g b _)) =
    setRenderDrawColor ren r g b 255 >> renderFillRect ren rect

drawBoxes :: Size -> Renderer -> Pal -> Int -> Int -> IO ()
drawBoxes s ren pal mx on = mapM_ (rendN ren) boxes >> renderPresent ren
    where boxes = genBoxes s pal mx on

repeatKey :: Renderer -> IO a -> IO a -> IO ()
repeatKey ren keeper f = do
      mbEvent <- pollEvent
      _ <- keeper
      case fmap eventData mbEvent of
        Just Quit                              -> exitSuccess
        Just Keyboard{ keyMovement = KeyDown, keySym = Keysym{..} }
          | keyKeycode == Key.Space            -> f >> repeatKey ren keeper f
          | keyKeycode == Key.Escape           -> exitSuccess
        _otherwise                             -> repeatKey ren keeper f


efloop :: Size -> Renderer -> Int -> Int -> IO ()
efloop _  _   _   n | n <= 0 = return ()
efloop sz ren box n = do
    x <- randomNumber (1,box)
    drawBoxes sz ren palette box x
    threadDelay (10^5)
    efloop sz ren box (n-1)

randomNumber :: (Int,Int) -> IO Int
randomNumber = randomRIO
