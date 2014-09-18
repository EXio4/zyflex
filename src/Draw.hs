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

module Draw (
    Box(..),
    position,
    genBoxes
) where

import System.Random
import Data.Maybe

import Coloring
import Graphics.UI.SDL

data Box = Box Rect Color

genBoxes :: Size -> Pal -> Int -> Int -> [Box]
genBoxes (Size w h) pal mx n  =
        genBoxes' 0 (Size w o1) pal (0,m1) n
        ++ genBoxes' o1 (Size w o2) pal (m1,mx) n
    where
        (o1,o2) = (h `div`2 , h-o1)
        m1 = mx`div`2

genBoxes' :: Int -> Size -> Pal -> (Int,Int) -> Int -> [Box]
genBoxes' offset sz pal (st,mx) n = map (genB) [st..mx]
    where
        genB z
               | z == n    = Box pos cl
               | otherwise = Box pos (dark cl)
            where cl = col z
                  pos = position sz offset (mx-st) (z-st)
        col z = fromMaybe (snd (takeR pal z)) (lookup z pal)


position :: Size -> Int -> Int -> Int -> Rect
position (Size w h) off mx n = Rect x1 y1 x2 y2
    where
        a z = (w `div` mx) *  z
        (x1,y1) = (a (n-1), off)
        (x2,y2) = (a n    , h)

takeR :: [a] -> Int -> a
takeR list n = list !! indx
    where
        len = length list
        (indx,_) = randomR (0, len-1) (mkStdGen n)
