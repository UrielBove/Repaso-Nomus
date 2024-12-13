module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Nomu = UnNomu{
    tieneAlas :: Bool,
    tieneMultiplesBrazos :: Bool,
    ojosCant :: Number,
    colorPiel :: ColorDePiel,
    cantVida :: Number,
    fuerza :: Number,
    poderes :: [Poder]
}deriving(Show)

data ColorDePiel = Negro | Blanco | Azul | Gris deriving (Show)

puedeVer :: Nomu -> Bool
puedeVer nomu = ojosCant nomu > 0

data Categoria = Comun | Fuerte | HighEnd | Pichi deriving(Show)

categoria :: Nomu -> Categoria
categoria nomu
    |fuerza nomu > 10000 = HighEnd
    |fuerza nomu > 3000 = Fuerte
    |fuerza nomu > 1000 = Comun
    |otherwise = Pichi

data Poder = UnPoder{
    cantCuracion :: Number,
    cantDanio :: Number,
    rangoAtaque :: Number,
    probabilidadDanioCritico :: Number
}deriving (Show)

dameProbabilidadDanioCritico :: Nomu -> Number
dameProbabilidadDanioCritico nomu = probabilidadDanioCritico (last (poderes nomu)) 

esCuerpoACuerpo :: Poder -> Bool
esCuerpoACuerpo poder = rangoAtaque poder < 100

esSoloDeCuracion :: Poder -> Bool
esSoloDeCuracion poder = cantDanio poder == 0 && cantCuracion poder > 0


nomu1 = UnNomu{tieneAlas = True, tieneMultiplesBrazos = True, ojosCant = 2, colorPiel = Negro, cantVida = 100, fuerza = 50, poderes = [superRegeneracion, fuego]}

superRegeneracion :: Poder
superRegeneracion = UnPoder{cantCuracion = 1000, cantDanio = 0, rangoAtaque = 0, probabilidadDanioCritico = 2}

fuego :: Poder
fuego = UnPoder{cantCuracion = 0, cantDanio = 600, rangoAtaque = 40, probabilidadDanioCritico = 300}

--Practica
esTop :: (Ord a) => a -> a-> Bool
esTop algo otroAlgo = algo < otroAlgo