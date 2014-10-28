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

module Pregu (
    Sect(..),
    Preg(..),
    Opt(..),
    preguntados
) where

data Sect = Sect String [Preg]
data Preg = Preg String [Opt]
data Opt  = Opt String 

preguntados :: [Sect]
preguntados = 
  [Sect
    "GEOGRAFÍA"
    [Preg
      "1. ¿Cuantas sedes tiene la facu?"
      [],
     Preg
      "2. ¿En qué ciudad está la Sede Central?"
      [],
     Preg
      "3. ¿Podes estudiar en Victoria?"
      [],
     Preg
      "4. ¿Cuál es la sede que se encuentra más al norte de la provincia?"
      [],
     Preg
      "5. ¿Dónde se estudia la Licenciatura en Explosivos?"
      []],
   Sect
    "CIENCIA"
    [Preg
      "A que se dedica la ciencia criminalísta?"
      [Opt "Exhumación de cadáveres",
       Opt "Juzgar a los criminales",
       Opt "Estudiar los delitos"],
     Preg
      "Qué estudia Accidentologia Vial?"
      [Opt "- El siniestro de tránsito",
       Opt "- Accidentes naturales",
       Opt "- Construcción de rutas y caminos viales"],
     Preg
      "3. ¿Cuántos Centros de Investigación tiene la Facultad?"
      [],
     Preg
      "4. ¿Qué licenciatura estudia los ecosistemas?"
      [],
     Preg
      "5. ¿A qué carrera corresponden las materias Inteligencia Artificial\ny Sistemas Operativos?"
      []],
   Sect
    "ACADÉMICA"
    [Preg
      "1. ¿Qué carrera se estudia a distancia?"
      [],
     Preg
      "2. ¿Cuántas carreras de grado tiene la Facultad?"
      [],
     Preg
      "3. ¿Cómo se llama la maestría que se dicta en Diamante?"
      [],
     Preg
      "4. ¿Qué diferencia hay entre una carrera de grado y pregrado?"
      [Opt "La duración y incumbencia del título",
       Opt "Con la tecnicatura se estudia menos",
       Opt "El sueldo del licenciado es más alto"]],
   Sect
    "CIENCIAS\nSOCIALES"
    [Preg
      "1. ¿Tenés que pagar para estudiar en la Facultad?"
      [],
     Preg
      "2. ¿Qué significa que la universidad es pública?"
      [],
     Preg
      "3. ¿Cómo se llama el colegio histórico que pertenece a la Facultad?"
      [],
     Preg
      "4. ¿De quién depende la UADER?"
      [],
     Preg
      "5. ¿Cuál es la fan page de Facebook de la Facultad?"
      []]]