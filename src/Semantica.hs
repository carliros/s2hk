module Semantica (
  module Estructura
, module Semantica
) where

import Core
import Estructura
import Data.Maybe
import Data.List

-- mis funciones semanticas
mostrar fun hlista
    = do let ls = fun hlista
         mapM_ putStrLn ls

formatoListCtrl :: HLista -> [[String]]
formatoListCtrl hl
    = [ [ show num
        , (maybe "" snd nombre)
        , (maybe "" snd univ)
        , (maybe "" snd email)
        , (maybe "" snd nivel)
        ]
        | (HPersona nombre univ email nivel, num) <- zip hl [1..]
      ]

nombres :: HLista -> [String]
nombres ls 
    = [nombre | (HPersona (Just (_,nombre)) _ _ _) <- ls]

lista :: HLista -> [String]
lista ls 
    = let titulos = "Nombre \t\tUniversidad \t\tEmail \t\tNivel"
          cuerpo  = [ ((maybe "" snd nombre) ++ "\t\t" ++
                       (maybe "" snd univ)   ++ "\t\t" ++
                       (maybe "" snd email)  ++ "\t\t" ++
                       (maybe "" snd nivel))
                    | (HPersona nombre univ email nivel) <- ls]
      in titulos : cuerpo

todos :: HLista -> [(String, String, String, String)]
todos ls 
    = [ (nombre, univ, email, nivel)
      | (HPersona (Just (_,nombre))
                  (Just (_,univ))
                  (Just (_,email))
                  (Just (_,nivel))) <- ls]

-- HLista writers
writeHLista :: HLista -> FilePath -> IO()
writeHLista hl fn = do let str = unlines $ map show hl
                       writeFile fn str 

readHLista :: FilePath -> IO HLista
readHLista file 
    = do input <- readFile file
         parseHLista input
