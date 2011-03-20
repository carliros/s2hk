module Semantica
( nombres
, todos
, readHLista
, mostrar
) where

import Core
import Estructura

mostrar fun hlista
    = do let ls = fun hlista
         mapM_ print ls

-- mis funciones semanticas
nombres :: HLista -> [String]
nombres ls 
    = [nombre | (HPersona (Just nombre) _ _ _) <- ls]

todos :: HLista -> [(String, String, String, String)]
todos ls 
    = [ (nombre, univ, email, nivel)
      | (HPersona (Just nombre)
                  (Just univ)
                  (Just email)
                  (Just nivel)) <- ls]



-- test
hlist :: HLista
hlist = (HPersona (Just "Carlos Gomez")   (Just "UMSS") (Just "carliros.g@gmail.com")  (Just "Medio"))
      : (HPersona (Just "Antonio Mamani") (Just "UMSS") (Just "antonio.mqg@gmail.com") (Just "Medio"))
      : (HPersona Nothing                 (Just "UMSS") (Just "nothing@gmail.com")     (Just "Avanzado"))
      : []

