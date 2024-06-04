--Alumno: Valentín Fernández Pizzella

module Library where
import PdePreludat

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b | f a > f b = a
                 | otherwise = b


--PUNTO 1
--Palos de Golf
type Palo = Habilidad -> Tiro
--a)Modelado de diferentes palos
--i)
putter :: Palo
putter habilidad = UnTiro {velocidad=10, precision= (precisionJugador habilidad)*2, altura=0}
--ii)
madera :: Palo
madera habilidad = UnTiro {velocidad=100, precision= (precisionJugador habilidad)/2, altura =5}
--iii)
hierros :: Number -> Palo
hierros numero habilidad = UnTiro {velocidad=(fuerzaJugador habilidad)*numero, precision=(precisionJugador habilidad)/numero, altura=numero-3}

--b) Constante Palos
type Palos = [Palo]


--PUNTO 2
--Función golpe
golpe :: Jugador -> Palo -> Tiro
golpe jugador palo = palo (habilidad jugador)


--PUNTO 3
--Obstáculos
data Obstaculo = UnObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efecto :: Tiro -> Tiro
  }
intentarSuperar :: Obstaculo -> Tiro -> Tiro
intentarSuperar obstaculo tiro | (puedeSuperar obstaculo) tiro = (efecto obstaculo) tiro
                               | otherwise = tiroDetenido

tiroDetenido :: Tiro
tiroDetenido = UnTiro 0 0 0

--a)Tunel con rampita
intentaSuperarTunelConRampita :: Tiro -> Bool
intentaSuperarTunelConRampita tiro = precision tiro > 90 && altura tiro == 0

efectoTunelConRampita :: Tiro -> Tiro
efectoTunelConRampita tiro = tiro {velocidad = velocidad tiro *2, altura=0}

tunelConRampita :: Obstaculo
tunelConRampita = UnObstaculo intentaSuperarTunelConRampita efectoTunelConRampita

--b)Laguna
intentaSuperarLaguna :: Tiro -> Bool
intentaSuperarLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Number -> Tiro -> Tiro
efectoLaguna laguna tiro = tiro {altura=altura tiro / laguna}

laguna :: Number -> Obstaculo
laguna largo  = UnObstaculo intentaSuperarLaguna (efectoLaguna largo)

--c)Hoyo
intentaSuperarHoyo :: Tiro -> Bool
intentaSuperarHoyo tiro = (between 5 20 . velocidad) tiro && precision tiro > 95 && altura tiro == 0

efectoHoyo :: Tiro -> Tiro
efectoHoyo tiro = tiroDetenido

hoyo :: Obstaculo
hoyo = UnObstaculo intentaSuperarHoyo efectoHoyo

--PUNTO 4
--a) Palos utiles
palosUtiles :: Jugador -> Palos -> Obstaculo -> Palos
palosUtiles jugador palos obstaculo = filter (puedeSuperar obstaculo . golpe jugador) palos

--b) Obstaculos seguidos
obstaculosSeguidos :: Tiro -> [Obstaculo] -> [Obstaculo]
obstaculosSeguidos tiro (x:y:xs) = 