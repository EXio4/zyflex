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

module Coloring (
    Pal,
    palette,
    dark
) where

import Graphics.UI.SDL

type Pal = [(Int,Color)]

palette :: Pal
palette =
    [(1, red)
    ,(2, blue)
    ,(3, purple)
    ,(4, green)
    ,(5, yellow)]

red    :: Color
blue   :: Color
purple :: Color
green  :: Color
yellow :: Color
red    = Color 255  0     0  0
blue   = Color 0    0   255  0
purple = Color 255  0   255  0
green  = Color 0    255   0  0
yellow = Color 255  255   0  0

dark :: Color -> Color
dark (Color a b c _) = Color (op a) (op b) (op c) 0
    where op x = (x `div` 5) * 2

