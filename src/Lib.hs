module Lib where

import Text.Show.Functions

import Data.Char

-- Punto 1

type Habilidad = String

data Barbaro = Barbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
} deriving (Show)

type Objeto = Barbaro -> Barbaro

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla,espada 5]

cambiarHabilidades :: ([Habilidad] -> [Habilidad]) -> Barbaro -> Barbaro
cambiarHabilidades funcion barbaro = barbaro {habilidades = funcion (habilidades barbaro)}

cambiarObjetos :: ([Objeto] -> [Objeto]) -> Barbaro -> Barbaro
cambiarObjetos funcion barbaro = barbaro {objetos = funcion (objetos barbaro)}

asignarFuerza :: Int -> Objeto
asignarFuerza valorNuevo barbaro = barbaro {fuerza = valorNuevo}

espada :: Int -> Objeto
espada pesoEspada barbaro = asignarFuerza (pesoEspada * 2 + fuerza barbaro) barbaro

otorgarHabilidad :: Habilidad -> [Habilidad] -> [Habilidad] 
otorgarHabilidad habilidad habilidades = habilidad : habilidades

amuletosMisticos :: Habilidad -> Objeto
amuletosMisticos habilidadAOtorgar barbaro = cambiarHabilidades (otorgarHabilidad habilidadAOtorgar) barbaro

desaparecerObjetos :: [Objeto] -> [Objeto]
desaparecerObjetos objetos = []

varitasDefectuosas :: Objeto
varitasDefectuosas barbaro = cambiarObjetos (desaparecerObjetos).cambiarHabilidades (otorgarHabilidad "Hacer Magia") $ barbaro

ardilla :: Objeto
ardilla barbaro = barbaro

-- eliminarObjetos :: Objeto -> Objeto -> [Objeto] -> [Objeto]
-- eliminarObjetos objeto1 objeto2 objetos = filter (/=objeto2).filter (/=objeto1) $ objetos

agregarObjeto :: Objeto -> [Objeto] -> [Objeto]
agregarObjeto objeto objetos = objeto : objetos

cuerda :: Objeto -> Objeto -> Objeto
cuerda objeto1 objeto2 barbaro = cambiarObjetos ((agregarObjeto (objeto1.objeto2))) barbaro

-- Punto 2

ponerHabilidadEnMayuscula :: Habilidad -> Habilidad
ponerHabilidadEnMayuscula [] = []
ponerHabilidadEnMayuscula (x:xs) =  toUpper x : ponerHabilidadEnMayuscula xs

concatenarHabilidades :: [Habilidad] -> [Habilidad]
concatenarHabilidades habilidades = [foldl1 (++) habilidades] 

megafono :: Objeto
megafono barbaro =  cambiarHabilidades (concatenarHabilidades.map ponerHabilidadEnMayuscula) barbaro

megafonoBarbarico :: Objeto -> Objeto -> Objeto
megafonoBarbarico objeto1 objeto2 barbaro = megafono.ardilla.cuerda objeto1 objeto2 $ barbaro

-- Punto 3

type Evento = Barbaro -> Bool
type Aventura = [Evento]

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes barbaro = elem "Escribir Poesía Atroz" (habilidades barbaro)

cremalleraDelTiempo :: Evento
cremalleraDelTiempo barbaro = (||) ((=="Foffy").nombre $ barbaro) ((=="Astro").nombre $ barbaro) 

sobreviveASaqueo :: Evento
sobreviveASaqueo barbaro = elem "Robar" (habilidades barbaro) && fuerza barbaro > 80

primeraLetraMayuscula :: Habilidad -> Bool
primeraLetraMayuscula habilidad = head habilidad == (toUpper.head) habilidad 

poderDeGritoDeGuerra :: [Habilidad] -> Int
poderDeGritoDeGuerra habilidades = maximum.map length $ habilidades

-- sobreviveAGritoDeGuerra :: Evento
-- sobreviveAGritoDeGuerra barbaro 

esVocal :: Char -> Bool
esVocal letra = letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u' 

tieneMasDeTresVocales :: Habilidad -> Bool
tieneMasDeTresVocales habilidad = (>3) (length.filter (esVocal) $ habilidad)

sobreviveACaligrafia :: Evento
sobreviveACaligrafia barbaro = all (primeraLetraMayuscula) (habilidades barbaro) && all (tieneMasDeTresVocales) (habilidades barbaro)

ritualDeFechorias :: Evento
ritualDeFechorias barbaro = sobreviveASaqueo barbaro ||  sobreviveACaligrafia barbaro

sobreviveA :: Barbaro -> Evento -> Bool
sobreviveA barbaro condicion = condicion barbaro 

sobreviviente :: Aventura -> Barbaro -> Bool
sobreviviente aventura barbaro = foldl (&&) True ((map (sobreviveA barbaro)) $ aventura)

sobrevivientes :: Aventura -> [Barbaro] -> [Bool]
sobrevivientes aventura barbaros = map (sobreviviente aventura) barbaros

-- Punto 4

-- A)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = x : sinRepetidos (filter (/= x) xs)

-- B)

generacionIncial = 0

funcionAAplicar :: Barbaro -> (Barbaro -> Barbaro)
funcionAAplicar barbaro = foldl1 (.) (objetos barbaro)

asignarNombre :: String -> Barbaro -> Barbaro
asignarNombre nuevoNombre barbaro = barbaro {nombre = nuevoNombre}

obtenerHijo :: Int -> Barbaro -> Barbaro 
obtenerHijo generacionInicial barbaro = funcionAAplicar barbaro.cambiarHabilidades (sinRepetidos).asignarNombre (nombre barbaro ++ replicate generacionInicial '*') $ barbaro

descendientes :: Int -> Barbaro -> [Barbaro]
descendientes generacionInicial barbaro = (obtenerHijo generacionInicial barbaro) : [obtenerHijo (generacionInicial+1) barbaro]

-- C)
-- No se puede aplicar "sinRepetidos" sobre la lista de objetos ya que estas son funciones por lo tanto no pertenecen a la familia de "Eq"
-- Si se aplica sobre el nombre de un bárbaro, se eliminan las letras repetidas.