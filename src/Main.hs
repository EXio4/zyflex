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
size = (Size 1024 600)

main :: IO ()
main = withInit [InitEverything] $
    withWindow "ZyFlex" (Position 0 0) size
        [WindowShown,WindowFullscreen] $ \win ->
    withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \ren -> do
        renderClear ren
        drawBoxes size ren palette 5 10
        renderPresent ren
        repeatKey ren $ loop size ren 5 15
        return ()

randomNumber :: (Int,Int) -> IO Int
randomNumber (a,b) = randomRIO (a,b)

rendN :: Renderer -> Box -> IO ()
rendN ren (Box rect (Color r g b _)) =
    setRenderDrawColor ren r g b 255 >> renderFillRect ren rect

drawBoxes :: Size -> Renderer -> Pal -> Int -> Int -> IO ()
drawBoxes s ren pal mx on = mapM_ (rendN ren) boxes
    where boxes = genBoxes s pal mx on

repeatKey :: Renderer -> IO a -> IO ()
repeatKey ren f = do
      mbEvent <- pollEvent
      case fmap eventData mbEvent of
        Just Quit                              -> exitSuccess
        Just Keyboard{ keyMovement = KeyDown, keySym = Keysym{..} }
          | keyKeycode == Key.Space            -> f >> repeatKey ren f
          | keyKeycode == Key.Escape           -> exitSuccess
        _otherwise                             -> repeatKey ren f


loop :: Size -> Renderer -> Int -> Int -> IO ()
loop _  _   _   n | n <= 0 = return ()
loop sz ren box n = do
    x <- randomNumber (1,box)
    putStrLn $ "drawing [" ++ (show n) ++ "] " ++ (show x)
    drawBoxes sz ren palette box x
    renderPresent ren
    threadDelay (10^5 :: Int)
    loop sz ren box (n-1)
