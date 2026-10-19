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
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez:
--
mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor = undefined

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora = undefined

duracion :: String -> String -> Either String Int
duracion = undefined

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
