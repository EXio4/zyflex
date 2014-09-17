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

import Coloring
import Graphics.UI.SDL

data Box = Box Rect Color

genBoxes :: Size -> Pal -> Int -> Int -> [Box]
genBoxes sz pal max n = map genB [1..max]
    where
        genB z | z == n    = Box pos cl
               | otherwise = Box pos (dark cl)
            where cl = col z
                  pos = position sz max z
        col = maybe (error "undefined color") id . flip lookup pal

position :: Size -> Int -> Int -> Rect
position (Size w h) max n = Rect x1 y1 x2 y2
    where
        a z = (w `div` max) *  z
        (x1,y1) = (a (n-1), 0)
        (x2,y2) = (a n    , h)
