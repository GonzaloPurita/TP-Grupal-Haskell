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
--estaEnBuenEstado (UnAuto "Peugeot" _ _ _ _ _ _) = False
estaEnBuenEstado UnAuto{marca = "Peugeot"} = False
estaEnBuenEstado auto
                | tiempoDeCarrera auto < 100 = desgasteChasis auto < 20
                | otherwise = desgasteChasis auto < 40 && desgasteRuedas auto < 60

-- 2)b)
noDaMas :: Auto -> Bool
noDaMas auto = (primerApodoEmpiezaConLa auto && desgasteChasis auto > 80) || desgasteRuedas auto > 80

primerApodoEmpiezaConLa :: Auto -> Bool
primerApodoEmpiezaConLa = ("La " ==) . take 3 . primerApodo

primerApodo :: Auto -> String
primerApodo = head . apodos

-- 2)c)
esUnChiche :: Auto -> Bool
esUnChiche auto
            | tieneApodosPares auto = desgasteChasis auto < 20
            | not (tieneApodosPares auto) = desgasteChasis auto < 50

tieneApodosPares :: Auto -> Bool
tieneApodosPares = even . cantApodos

cantApodos :: Auto -> Number
cantApodos = length . apodos

-- 2)d)
esUnaJoya :: Auto -> Bool
esUnaJoya auto = (not . tieneDesgaste) auto && (not . tieneApodos) auto

tieneDesgaste :: Auto -> Bool
tieneDesgaste UnAuto{desgasteChasis = 0, desgasteRuedas = 0} = False
tieneDesgaste UnAuto{desgasteChasis = _, desgasteRuedas = _} = True

tieneApodos :: Auto -> Bool
tieneApodos UnAuto{apodos = []} = False
tieneApodos UnAuto{apodos = [_]} = False
tieneApodos UnAuto{apodos = _} = True

-- 2)e)
nivelDeChetez :: Auto -> Number
nivelDeChetez auto = 20 * cantApodos auto * (length . modelo) auto

-- 2)f)
capacidadSupercalifragilisticaespialidosa :: Auto -> Number
capacidadSupercalifragilisticaespialidosa = length . primerApodo

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
aplicarPenalidad :: Number -> Auto -> Auto
aplicarPenalidad segundos auto = auto{tiempoDeCarrera = tiempoDeCarrera auto + segundos}

-- 3)c)
ponerleNitro :: Auto -> Auto
ponerleNitro auto = auto{velocidadMax = velocidadMax auto + velocidadMax auto * 0.2}

-- 3)d)
bautizarAuto :: String -> Auto -> Auto
bautizarAuto nuevoApodo auto = auto{apodos = apodos auto ++ [nuevoApodo]}

-- 3)e)
desarmarAuto :: Auto -> String -> String -> Auto
desarmarAuto auto nuevaMarca nuevoModelo = auto{marca = nuevaMarca, modelo = nuevoModelo, apodos = ["Nunca taxi"]}

-- (perderApodos auto){marca = nuevaMarca, modelo = nuevoModelo, apodos = apodos auto ++ ["Nunca taxi"]}
-- perderApodos :: Auto -> Auto
-- perderApodos auto = auto{apodos = []}

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

atravesarCurva :: Tramo -> Auto -> Auto
atravesarCurva (Curva angulo longitud) auto = 
    auto{
        desgasteRuedas = desgasteRuedas auto + (3 * longitud / angulo), 
        tiempoDeCarrera = tiempoDeCarrera auto + (longitud / (velocidadMax auto / 2))
    }


-- RECTA --

tramoRectoClassic :: Tramo
tramoRectoClassic = Recta 715

tramito :: Tramo
tramito = Recta 260

atravesarRecta :: Tramo -> Auto -> Auto
atravesarRecta (Recta longitud) auto = 
    auto{
        desgasteChasis = desgasteChasis auto + 0.1 * longitud, 
        tiempoDeCarrera = tiempoDeCarrera auto + longitud / velocidadMax auto
    }


-- ZIGZAG --

zigZagLoco :: Tramo
zigZagLoco = Zigzag 5

casiCurva :: Tramo
casiCurva = Zigzag 1

atravesarZigzag :: Tramo -> Auto -> Auto
atravesarZigzag (Zigzag cambiosDeDireccion) auto = 
    auto{
        tiempoDeCarrera = tiempoDeCarrera auto + cambiosDeDireccion * 3, 
        desgasteChasis = desgasteChasis auto + 5, 
        desgasteRuedas = desgasteRuedas auto + velocidadMax auto * cambiosDeDireccion / 100
    }

-- RULO EN EL AIRE --

ruloClasico :: Tramo
ruloClasico = Rulo 13

deseoDeMuerte :: Tramo
deseoDeMuerte = Rulo 26

atravesarRulo :: Tramo -> Auto -> Auto
atravesarRulo (Rulo diametro) auto = 
    auto{
        desgasteRuedas = desgasteRuedas auto + diametro * 1.5, 
        tiempoDeCarrera = tiempoDeCarrera auto + 5 * diametro / velocidadMax auto
    }


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

