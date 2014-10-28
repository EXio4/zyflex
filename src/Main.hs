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

import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.Keycode as Key
import qualified Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.TTF.Types (TTFFont)
import Graphics.UI.SDL hiding (withInit)

import Coloring
import Draw
import Pregu

numSq :: Int
numSq = length preguntados

main :: IO ()
main = SDL.withInit [InitEverything] $
    TTF.withInit $
    withWindow "ZyFlex" (Position 0 0) sz
        [WindowShown,WindowFullscreenDesktop] $ \win ->
    withRenderer win (Device (-1)) [Accelerated, PresentVSync] $ \ren -> do
        titlefont <- TTF.openFont "/usr/share/fonts/truetype/droid/DroidSans.ttf" 44
        pregfont  <- TTF.openFont "/usr/share/fonts/truetype/droid/DroidSans.ttf" 32
        repeatKey sz ren
            (\_ -> renderPresent ren >> threadDelay (10^4)) -- keep the screen updated
            (\size -> renderClear ren >> drawBoxes size titlefont ren palette (numSq+2)) -- this is buggy now
            (\size -> efloop size (titlefont,pregfont) ren numSq (numSq*2,(numSq*4)))
  where sz = Size 640 480

rendN :: TTFFont -> Renderer -> Fig -> IO ()
rendN    _ ren (Box rect (Color r g b _)) =
    setRenderDrawColor ren r g b 255 >> renderFillRect ren rect
rendN font ren (Txt pos   color str) =
    drawW font ren pos color 0 (lines str)

drawW :: TTFFont -> Renderer -> Position -> Color -> Int -> [String] -> IO ()
drawW _    _      _             _     _    []      = return ()
drawW font ren p@(Position x y) color off (str:xs) = do
    textSurface <- TTF.renderUTF8Blended font str color
    textTexture <- SDL.createTextureFromSurface ren textSurface
    (width, height) <- TTF.sizeText font str
    renderCopy ren textTexture Nothing (Just $ Rect x (y+off) width height)
    drawW font ren p color (off+height+5) xs
    
drawBoxes :: Size -> TTFFont -> Renderer -> Pal -> Int -> IO ()
drawBoxes s font ren pal on = mapM_ (rendN font ren) boxes >> renderPresent ren
  where boxes = genBoxes preguntados s pal on

repeatKey :: Size -> Renderer -> (Size -> IO a) -> (Size -> IO a) -> (Size -> IO a) -> IO ()
repeatKey size ren keeper chang f = do
      mbEvent <- pollEvent
      _ <- keeper size
      case fmap eventData mbEvent of
        Just Quit                              -> exitSuccess
        Just Keyboard{ keyMovement = KeyDown, keySym = Keysym{..} }
          | keyKeycode == Key.Space            -> f size >> repeatKey size ren keeper chang f
          | keyKeycode == Key.Escape           -> exitSuccess
        Just (Window _ (Resized x))            -> chang x >> repeatKey x ren keeper chang f 
        _otherwise                             -> repeatKey size ren keeper chang f

efloop :: Size -> (TTFFont,TTFFont) -> Renderer -> Int -> (Int,Int) -> IO ()
efloop a b c d (mn, mx) = randomRIO (mn, mx) >>= efloop' a b c d 0

efloop' :: Size -> (TTFFont,TTFFont) -> Renderer -> Int -> Int -> Int -> IO ()
efloop' sz@(Size x y) (_,pregfont) ren _   l n | n <= 0 = do
    let (Sect _ part) = preguntados !! (l-1)
    rendN pregfont ren (Box (Rect 0 128 x y) (Color 50 50 50 0))
    showQuestion sz pregfont ren part
efloop' sz (titlefont,pregfont) ren box _ n = do
    x <- randomRIO (1,box)
    drawBoxes sz titlefont ren palette x
    threadDelay (10^5)
    efloop' sz (titlefont,pregfont) ren box x (n-1)

showQuestion :: Size -> TTFFont -> Renderer -> [Preg] -> IO ()
showQuestion (Size _ y) font ren list = do
    let nn = length list
    n <- randomRIO(0,nn-1)
    ask (list !! n)
  where ask (Preg qu xs) = rendN font ren (Txt (Position 20 180) (Color 190 0 190 0) qu) >> options xs
        options [] = return ()
        options xs = drawN xs 250
          where
              l = length xs
              esiz = (y - 250) `div` l
              drawN [] _ = return () -- impossible case
              drawN ((Opt x):xs) ay = do
                rendN font ren (Txt (Position 40 ay) (Color 190 0 190 0) x)
                drawN xs (ay+esiz)
                