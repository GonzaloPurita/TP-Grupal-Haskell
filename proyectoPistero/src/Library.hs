module Library where
import PdePreludat

data Auto = UnAuto{
    marca :: String,
    modelo :: Modelo,
    desgasteRuedas :: Number,
    desgasteChasis :: Number,
    velocidadMax :: Number,
    tiempoDeCarrera :: Number,
    apodos :: [String]
}deriving(Show, Eq)

type Modelo = String

ferrari :: Auto
ferrari = UnAuto "Ferrari" "F50" 0 0 65 0 ["La Nave", "El Fierro", "Ferrucho"]

lamborghini :: Auto
lamborghini = UnAuto "Lamborghini" "Diablo" 4 7 73 0 ["Lambo", "La Bestia"]

fiat :: Auto
fiat = UnAuto "Fiat" "600" 27 33 44 0 ["La Bocha", "La Bolita", "Fitito"]

peugeot :: Auto
peugeot = UnAuto "Peugeot" "504" 0 0 40 0 ["El Rey Del Desierto"]

-- 2)a)
estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado (UnAuto "Peugeot" _ _ _ _ _ _) = False
estaEnBuenEstado auto
--                | marca auto == "Peugeot" = False
                | tiempoDeCarrera auto < 100 = desgasteChasis auto < 20
                | otherwise = desgasteChasis auto < 40 && desgasteRuedas auto < 60

-- 2)b)
noDaMas :: Auto -> Bool
noDaMas auto = ((("La " ==) . take 3 . head . apodos) auto && desgasteChasis auto > 80) || desgasteRuedas auto > 80

-- 2)c)
esUnChiche :: Auto -> Bool
esUnChiche auto
            | (even . length . apodos) auto = desgasteChasis auto < 20
            | otherwise = desgasteChasis auto < 50

-- 2)d)
esUnaJoya :: Auto -> Bool
esUnaJoya auto = desgasteChasis auto == 0 && desgasteRuedas auto == 0 && ((<= 1) . length . apodos) auto 

-- 2)e)
nivelDeChetez :: Auto -> Number
nivelDeChetez auto = 20 * (length . apodos) auto * (length . modelo) auto

-- 2)f)
capacidadSupercalifragilisticaespialidosa :: Auto -> Number
capacidadSupercalifragilisticaespialidosa = length . head . apodos

-- 2)g)
nivelDeRiesgo :: Auto -> Number
nivelDeRiesgo auto 
            | estaEnBuenEstado auto = calculoRiesgo auto
            | otherwise = 2 * calculoRiesgo auto

calculoRiesgo :: Auto -> Number
calculoRiesgo auto = velocidadMax auto * (0.1 * desgasteRuedas auto)

-- 3)a)
repararUnAuto :: Auto -> Auto
repararUnAuto auto = auto{desgasteChasis = desgasteChasis auto * 0.15, desgasteRuedas = 0}

-- 3)b)
aplicarPenalidad :: Auto -> Number -> Auto
aplicarPenalidad auto segundos = auto{tiempoDeCarrera = tiempoDeCarrera auto + segundos}

-- 3)c)
ponerleNitro :: Auto -> Auto
ponerleNitro auto = auto{velocidadMax = velocidadMax auto + velocidadMax auto * 0.2}

-- 3)d)
bautizarAuto :: Auto -> String -> Auto
bautizarAuto auto nuevoApodo = auto{apodos = apodos auto ++ [nuevoApodo]}

-- 3)e)
desarmarAuto :: Auto -> String -> String -> Auto
desarmarAuto auto nuevaMarca nuevoModelo = (perderApodos auto){marca = nuevaMarca, modelo = nuevoModelo, apodos = apodos auto ++ ["Nunca taxi"]}

perderApodos :: Auto -> Auto
perderApodos auto = auto{apodos = []}

-- 4)

data Pista = UnaPista{
    nombre :: String,
    pais :: String,
    precioBase :: Number,
    tramos :: [Tramo]
}deriving(Show, Eq)


data Tramo =
            Curva{angulo :: Number, longitud :: Number}
            | Recta{longitud :: Number}
            | Zigzag{cambiosDeDireccion :: Number}
            | Rulo{diametro :: Number}
            deriving(Show, Eq)

-- CURVA --

curvaPeligrosa :: Tramo
curvaPeligrosa = Curva 60 300

curvaTranca :: Tramo
curvaTranca = Curva 110 550

atravesarCurva :: Auto -> Tramo -> Auto
atravesarCurva auto (Curva angulo longitud) = auto{desgasteRuedas = desgasteRuedas auto + (3 * longitud / angulo), tiempoDeCarrera = tiempoDeCarrera auto + (longitud / (velocidadMax auto / 2))}


-- RECTA --

tramoRectoClassic :: Tramo
tramoRectoClassic = Recta 715

tramito :: Tramo
tramito = Recta 260

atravesarRecta :: Auto -> Tramo -> Auto
atravesarRecta auto (Recta longitud) = auto{desgasteChasis = desgasteChasis auto + 0.1 * longitud, tiempoDeCarrera = tiempoDeCarrera auto + longitud / velocidadMax auto}


-- ZIGZAG --

zigZagLoco :: Tramo
zigZagLoco = Zigzag 5

casiCurva :: Tramo
casiCurva = Zigzag 1

atravesarZigzag :: Auto -> Tramo -> Auto
atravesarZigzag auto (Zigzag cambiosDeDireccion) = auto{tiempoDeCarrera = tiempoDeCarrera auto + cambiosDeDireccion * 3, desgasteChasis = desgasteChasis auto + 5, desgasteRuedas = desgasteRuedas auto + velocidadMax auto * cambiosDeDireccion / 100}

-- RULO EN EL AIRE --

ruloClasico :: Tramo
ruloClasico = Rulo 13

deseoDeMuerte :: Tramo
deseoDeMuerte = Rulo 26

atravesarRulo :: Auto -> Tramo -> Auto
atravesarRulo auto (Rulo diametro) = auto{desgasteRuedas = desgasteRuedas auto + diametro * 1.5, tiempoDeCarrera = tiempoDeCarrera auto + 5 * diametro / velocidadMax auto}


-- 5)a)
nivelDeJoyez :: [Auto] -> Number
nivelDeJoyez autos = sum (map parseoANivel (filter esUnaJoya autos))

parseoANivel :: Auto -> Number
parseoANivel auto
            | tiempoDeCarrera auto < 50 = 1
            | otherwise = 2

-- 5)b)
sonParaEntendidos :: [Auto] -> Bool
sonParaEntendidos = all condicionParaSerEntendido

condicionParaSerEntendido :: Auto -> Bool
condicionParaSerEntendido auto = estaEnBuenEstado auto && tiempoDeCarrera auto <= 200

