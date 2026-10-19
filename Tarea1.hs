-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Carlos Moreno
--  Carnet: 21-10409
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado):
--  Copilot (Usado para consultas sobre sintaxis de Haskell)
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)
import Data.ByteString.Builder.Prim (cstring)
import Distribution.Simple.Utils (safeHead)
import System.Posix (SystemID(machine))
import GHCi.Message (THMessage(Reify))

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
--Funciones auxiliares:

headQueNoDaAviso :: [a] -> Maybe a
headQueNoDaAviso [] = Nothing
headQueNoDaAviso (x:_) = Just x

lastQueNoDaAviso :: [a] -> Maybe a
lastQueNoDaAviso [] = Nothing
lastQueNoDaAviso [b] = Just b 
lastQueNoDaAviso (_:cs) = lastQueNoDaAviso cs


normalizar :: String -> String
normalizar [] = []
normalizar (c:cs)
  | isAlphaNum c = toLower c : normalizar cs
  | otherwise = normalizar cs

comparacion :: String -> Bool
comparacion [] = True
comparacion [_] = True
comparacion cs
  | headQueNoDaAviso cs == lastQueNoDaAviso cs = comparacion(eliminarExtremos(cs))
  | otherwise = False

eliminarExtremos :: String -> String
eliminarExtremos [] = []
eliminarExtremos [_] = []
eliminarExtremos (_:cs) = eliminarCola cs

eliminarCola :: String -> String
eliminarCola []  = []
eliminarCola [_] = []
eliminarCola (c:cs) = c : eliminarCola cs

-- Funcion Principal de A1
esPalindromo :: String -> Bool
esPalindromo string = comparacion (normalizar string)


-- A2
-- La funcion auxiliar recursiva que chequea el primer elemento de la lista para guardarlo como mayor y menor.
-- Luego toma el siguiente elemento, los compara con el mayor y menor actual sustituyendolos dependiendo del caso,
-- entonces repite esto con el siguiente elemento hasta que no hayan mas elementos, pasando solo una vez por la lista.

-- Funciones Auxiliares de A2

maxyMin :: (Ord a) => a -> a -> [a] -> (a, a)
maxyMin mayor menor [] = (mayor, menor) 
maxyMin mayor menor (x:xs) =
  maxyMin (if x > mayor then x else mayor) 
  (if x < menor then x else menor)
  xs

-- Funcion Principal A2
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor (x:xs) = Just (maxyMin x x xs)

-- A3

-- Funcion Auxiliares de A3

chequearFormato :: String -> Bool
chequearFormato [h1, h2, ':', m1, m2] = 
  isDigit h1 && isDigit h2 && isDigit m1 && isDigit m2
chequearFormato _ = False

convertirStrEnInt :: String -> Int
convertirStrEnInt txt = read num :: Int
  where num = [txt !! 0, txt !! 1]

leerHora :: String -> Either String (Int, Int)
leerHora xs
  | not (chequearFormato xs) = Left "formato inválido"
  | hour >= 24 && min >= 60 = Left "hora fuera de rango"
  | hour >= 24 = Left "hora fuera de rango"
  | min >= 60 = Left "minutos fuera de rango"
  | otherwise = Right (hour, min)
  where hour = convertirStrEnInt(xs)
        min = convertirStrEnInt(drop (3) xs)

duracion :: String -> String -> Either String Int
duracion primera segunda = 
  case leerHora primera of
    Left errorPrimera -> Left errorPrimera
    Right (hour1, min1) -> case leerHora segunda of
      Left errorSegunda -> Left errorSegunda
      Right (hour2, min2) -> 
        let start = hour1*60 +min1
            end = hour2*60 + min2
        in Right (if end < start
                  then end + 24 * 60 - start
                  else end - start)

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined
