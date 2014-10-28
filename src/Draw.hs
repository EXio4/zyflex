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
    Fig(..),
    genBoxes
) where

import System.Random
import Data.Maybe

import Pregu
import Coloring
import Graphics.UI.SDL

data Fig = Box Rect Color
         | Txt Position Color String

genBoxes :: [Sect] -> Size -> Pal  -> Int -> [Fig]
genBoxes sects sz pal n = concatMap genB (zip sects [1..])
    where
        genB ((Sect txt _),z)
               | z == n    = [Box pos cl       , Txt (f pos) (dark cl) txt]
               | otherwise = [Box pos (dark cl), Txt (f pos)       cl  txt]
            where cl = col z
                  pos = position sz mx z
                  f (Rect x1 y1 _ _) = Position x1 y1
        col z = fromMaybe (snd (takeR pal z)) (lookup z pal)
        mx = length sects
        
position :: Size -> Int -> Int -> Rect
position (Size w h) mx n = Rect x1 y1 x2 y2
    where
        a z = (w `div` mx) *  z
        (x1,y1) = (a (n-1), 0)
        (x2,y2) = (a n    , h)
        
-- this is done this way so every run it is the same
takeR :: [a] -> Int -> a
takeR list n = list !! indx
    where
        len = length list
        (indx,_) = randomR (0, len-1) (mkStdGen n)
