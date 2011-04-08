module Core 
( parseHLista
) where

import Text.ParserCombinators.UU
import Data.Char
import Estructura

-- Core interface
parseHLista :: String -> IO HLista
parseHLista = parseString pRoot

-- Parser Combinators
pRoot :: Parser HLista
pRoot = pInutil
            *> pListSep_ng pInutil1 pHPersona <*
        pInutil

pHPersona :: Parser HPersona
pHPersona = HPersona  <$ pInutil  <*> pNombre 
                      <* pInutil1 <*> pUniversidad
                      <* pInutil1 <*> pEMail
                      <* pInutil1 <*> pNivel

pNombre :: Parser Info
pNombre = pMaybe (pField "Nombre_Completo")

pUniversidad :: Parser Info
pUniversidad = pMaybe (pField "Universidad")

pEMail :: Parser Info
pEMail = pMaybe (pField "Correo_Electronico")

pNivel :: Parser Info
pNivel = pMaybe (pField "Conocimiento_en_Programación_Funcional")

pField :: String -> Parser (String,String)
pField fd = (,) <$> pToken fd <* pSimboloAmb ":" <*> pDato

pDato :: Parser String
pDato = unwords <$> pListSep_ng pSpaces1 pPalabra

pPalabra :: Parser String
pPalabra = pList1 pCaracter

pCaracter :: Parser Char
pCaracter = pSym (ctexto, "caracter texto", 'a')
    where ctexto c = if c == ':' then False     -- simbolo reservado
                                 else isAlphaNum c || isPunctuation c

pSpaces1 :: Parser String
pSpaces1 = pList1 (pSym ' ')

pInutil :: Parser String
pInutil = pList (pAnySym " \n\r\t")

pInutil1 :: Parser String
pInutil1 = pList1 (pAnySym " \n\r\t")

pSimbolo :: String -> Parser String
pSimbolo sim = pToken sim

pSimboloAmb :: String -> Parser String
pSimboloAmb sim = pInutil *> pToken sim <* pInutil

-- parser interface
parseString :: Parser a -> String -> IO a
parseString p input
    = do let (out,errs) = parse ((,) <$> p <*> pEnd) (listToStr input (0,0))
         show_errors errs
         return out

