
module Cartas
( barajar
, baraja
, vacia
, cantidadCartas
, valor
, busted
, blackjack
, ganador
, separar
, inicialLambda
, desdeMano
, puedePicar
, aplanar
, reconstruir
, robar
, juegaLambda
) where

{-
    Proyecto Haskell
    JackLamda: Es un juego interctivo de BlackJack.
    Autores: Honorio Rodriguez #09-11023.
    Javier Medina     #12-10400.
    Ult. Actualización: 09/02/2020.
-}
-- Imports
import System.Random
import Data.List

-- Tipos de datos para carta
data Palo = Treboles | Diamantes | Picas | Corazones  deriving (Eq,Ord)
data Rango = N Int | Jack | Queen | King | Ace deriving (Eq,Ord)

data Carta = Carta {
                        rango :: Rango,
                        palo :: Palo
                    } deriving (Eq)
-- Tipo Jugador
data Jugador = Dealer | Player deriving (Read)

-- Tipo Mano
newtype Mano = Mano [Carta] deriving (Eq)

-- tipo Mazo
data Mazo = Vacio | Mitad Carta Mazo Mazo deriving (Show,Eq)
data Eleccion = Izquierdo | Derecho

--Exports 

-- Funciones Show para cartas

nuevaCarta :: Rango -> Palo -> Carta
nuevaCarta r p  = Carta r p 


instance Show Palo where
    show = showPalo

showPalo :: Palo -> String
showPalo palo 
           | palo == Treboles = "♣"
           | palo == Diamantes = "♦"
           | palo == Picas = "♠"
           | palo == Corazones = "♥"


instance Show Rango where
    show = showRango

showRango :: Rango -> String
showRango rango
           | rango == Jack = "J"
           | rango == Queen = "Q"
           | rango == King = "K"
           | rango == Ace = "A"
           | rango == (N 2) = "2"
           | rango == (N 3) = "3"
           | rango == (N 4) = "4"
           | rango == (N 5) = "5"
           | rango == (N 6) = "6"
           | rango == (N 7) = "7"
           | rango == (N 8) = "8"
           | rango == (N 9) = "9"
           | rango == (N 10) = "10"



instance Show Carta  where
    show (Carta rango palo) = showPalo palo ++ showRango rango

-- Funciones show para Jugador

instance Show Jugador where
    show = showJugador

showJugador :: Jugador -> String
showJugador Dealer = "Dealer"
showJugador Player = "Player"

-- Funciones para Mazo

instance Show Mano  where
    show (Mano []) = []
    show (Mano x) = unwords $ map show x

vacia :: Mano
vacia = Mano  []

listaPalo :: [Palo]
listaPalo = [Treboles,Diamantes,Picas,Corazones]

listaRango :: [Rango]
listaRango = [N 2,N 3,N 4,N 5,N 6,N 7,N 8,N 9,N 10,Jack,Queen,King,Ace]


baraja :: Mano
baraja = Mano [Carta y x | x <- listaPalo, y <- listaRango]

-- Funciones de acceso

cantidadCartas :: Mano -> Int
cantidadCartas (Mano []) = 0
cantidadCartas (Mano lista) = sum [1 | _ <- lista]
--Determina la cantidad de cartas en una mano

toInt ::Rango -> Int
toInt Jack = 10
toInt Queen = 10
toInt King = 10
toInt Ace = 11
toInt (N i) = i

checkAces :: Mano -> Int
checkAces (Mano lista) = sum[1 | (Carta x y) <- lista, x == Ace]

valor:: Mano -> Int
valor (Mano []) = 0
valor (Mano lista) 
               | (sum [toInt x | (Carta x y) <- lista])>21 = (sum [toInt x | (Carta x y) <- lista]) - ((checkAces (Mano lista) *10))
               | otherwise = (sum [toInt x | (Carta x y) <- lista])
--Recibe una Mano y devuelve un entero con el valor de la misma.

busted :: Mano -> Bool
busted m = if valor m > 21 then True else False 
--Recibe una Mano y devuelve True si su valor excede los 21, y False de otra forma

blackjack :: Mano ->Bool
blackjack m = if valor m == 21 then True else False 
--Recibe una Mano y devuelve True si la mano es un blackjack y False de otra forma. Recibe una Mano y
--devuelve True si su valor excede 21, y False de otra forma.


ganador :: Mano ->Mano -> Jugador
ganador manoDealer manoJugador 
    | busted manoDealer = Player
    | busted manoJugador = Dealer
    | (blackjack manoDealer)&&(cantidadCartas manoDealer == 2) = Dealer
    | (blackjack manoJugador)&&(cantidadCartas manoJugador == 2) = Player
    | (valor manoDealer == valor manoJugador) = Dealer
    | (valor manoDealer > valor manoJugador) = Dealer
    | (valor manoDealer < valor manoJugador) = Player
    | otherwise = Player
--Recibe la Mano del Dealer de primero, la mano del Player de segundo, y devuelve el ganador, seg´un las
--reglas del juego antes descritas.


-- takeMano devuelve un tipo [Carta] pero al ser llamada debe ser llamada como Mano $ takeMano ...
-- Funcion auxiliar para separar la mano, devuelve los primeros indice cartas de una mano
takeMano :: Mano -> Int -> [Carta]
takeMano _ 0 = []
takeMano (Mano lista) indice =  ((head lista):takeMano (Mano (tail lista)) (indice-1))

-- dropMano devuelve un tipo [Carta] pero al ser llamada debe ser llamada como Mano $ dropMano ...
-- Funcion auxiliar para separar la mano, devuelve los ultimos indice cartas de una mano
dropMano :: Mano -> Int -> [Carta]
dropMano (Mano []) _ = []
dropMano (Mano lista) indice
                         | indice > 0 = dropMano (Mano (tail lista)) (indice-1)
                         | indice == 0 = lista

-- IMPORTANTE para obtener el elemento medio debo tomar HEAD de la lista que devuelve llamar dropMano con indice = cantidadCartas `div` 2. 
-- Sabiendo esto entonces

takeCart :: Mano -> Int -> Carta
takeCart mano indice = head $ dropMano mano $ indice-1
-- takeCart toma la carta numero indice-1 de la mano 

separar :: Mano -> (Mano, Carta, Mano)
separar mano
        | even $ cantidadCartas mano = (Mano $ takeMano mano (cantidadCartas mano `div` 2),takeCart mano ((cantidadCartas mano `div` 2)+1), Mano $ dropMano mano ((cantidadCartas mano `div` 2)+1))
        | otherwise = (Mano $ takeMano mano (cantidadCartas mano `div` 2), takeCart mano ((cantidadCartas mano `div` 2)+1), Mano $ dropMano mano ((cantidadCartas mano `div` 2)+1))

addCart :: Carta -> Mano -> Mano
addCart carta (Mano lista) = Mano (lista ++ [carta])
-- addCarta AGREGA una carta a una mano de cartas


dropCart::Mano->Carta->[Carta]
dropCart (Mano []) carta = []
dropCart (Mano lista) carta
    | head lista == carta = dropCart (Mano (tail lista)) carta
    | otherwise = head lista:( dropCart (Mano (tail lista)) carta )

-- dropCarta elimina una carta de una mano


generaRandom :: Int -> Int -> StdGen -> (Int,StdGen)
generaRandom a b g = randomR(a,b) g
-- generaRandom toma 2 numeros y un StGen y usando la funcion randomRs (que genera una lista), tomamos uno de esos elementos.


elemCart :: Carta -> Mano -> Bool
elemCart carta (Mano lista) = carta `elem` lista 

unirManos :: Mano -> Mano -> Mano
unirManos mano (Mano []) = mano
unirManos (Mano []) mano = mano
unirManos (Mano lista1) (Mano lista2) = Mano (nub $ lista1++lista2)



barajar :: StdGen -> Mano -> Mano
barajar gen (Mano []) = Mano []
barajar gen mano = unirManos (Mano ([takeCart mano $ fst (generaRandom 1 (cantidadCartas mano) gen)])) (barajar (snd (generaRandom 1 ((cantidadCartas mano)*14) gen)) (Mano $ dropCart mano $ (takeCart mano (fst(generaRandom 1 (cantidadCartas mano) gen)))))
--
{-
(Mano ([takeCart mano $ fst (generaRandom 0 51 gen)])) devuelve una MANO que tiene solo una carta, que se aleatoria trae de mano, 
(barajar snd (generaRandom 0 51 gen) $ Mano $ dropCart snd (generaRandom 0 51 gen) $ (takeCart mano (fst(generaRandom 0 51 gen)))) es otra vez barajar sobre:

    snd (generaRandom 0 51 gen) genera una nueva semilla g
    Mano $ dropCart snd (generaRandom 0 51 gen) $ (takeCart mano (fst(generaRandom 0 51 gen)))) - Mano que tiene toda la mano inicial menos la carta que ya salio

-}
{-


Esta función será usada para barajar las cartas al inicio de cada ronda. Para ello, es necesario el tipo
de datos StdGen, incluido en el módulo System.Random. El segundo argumento recibido por la función
es la Mano a barajar, y debe devolverse la mano ya barajada. Para ello, se debe empezar por una Mano
vacı́a para acumular. Luego, debe seleccionarse una carta al azar, la cuál debe ser colocada al principio
de la Mano acumuladora. Esto debe hacerse hasta que se hayan pasado todas las cartas hasta la Mano
acumuladora.
-}



inicialLambda :: Mano -> (Mano, Mano)
inicialLambda mano = (Mano $ takeMano mano 2, Mano $ dropMano mano 2)
--Recibe la baraja inicial barajada como Mano, y devuelve la Mano inicial de Lambda tomando las dos
--primeras cartas, y la baraja resultante de retirar dichas cartas.
-- Aprovechamos de usar nuestras funciones takeMano y dropMano que sirven para quitar cartas.

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thrd3:: (a,b,c) -> c
thrd3 (_,_,c) = c


-- Funciones para Mazo

--Funciones de construccion

desdeMano :: Mano -> Mazo
desdeMano (Mano []) = Vacio
desdeMano mano = Mitad (snd3 $ separar mano) (desdeMano $ fst3 $ separar mano) (desdeMano $ thrd3 $ separar mano)


-- Funciones de acceso

puedePicar :: Mazo -> Bool
puedePicar Vacio = False
puedePicar (Mitad _ Vacio _) = False
puedePicar (Mitad _ _ Vacio) = False
puedePicar (Mitad _ _ _ ) = True

-- Funciones de modificacion
inorder :: Mazo -> [Carta]
inorder Vacio         =  []
inorder (Mitad carta l r) = inorder l ++ (carta : inorder r)

-- Recorre el mazo en inorden y devuelve una lissta de cartas

aplanar :: Mazo -> Mano
aplanar mazo = Mano (inorder mazo)



restarManos :: Mano -> Mano -> Mano
restarManos (Mano lista1) (Mano lista2) = Mano (lista2 \\ lista1)
--  restarManos toma 2 manos y devuelve una Mano que contiene las cartas que estaban en la primera pero no en la segunda
-- es lista2 - lista1 por como esta llamada en reconstruir

devuelveEleccion :: Mazo -> Eleccion -> Mazo
devuelveEleccion Vacio _ = Vacio
devuelveEleccion (Mitad carta l r) Izquierdo = l
devuelveEleccion (Mitad carta l r) Derecho = r

dropCartMazo :: Mazo -> Carta-> Mazo
dropCartMazo mazo carta = desdeMano $ Mano (dropCart (aplanar mazo) carta)
-- Elimina una Carta del mazo

firstCart :: Mazo -> Carta
firstCart (Mitad carta l r) =  carta

-- Devuelve la primera carta del mazo

reconstruir :: Mazo -> Mano -> Mazo
reconstruir mazo mano =  desdeMano $ restarManos mano $ aplanar mazo


robar :: Mazo ->Mano -> Eleccion -> Maybe (Mazo,Mano)
robar Vacio _ _ = Nothing
robar mazo mano eleccion = Just ((devuelveEleccion mazo eleccion), unirManos mano $ Mano [firstCart (devuelveEleccion mazo eleccion)])


{- 
	Recibe el Mazo actual, la mano del jugador y una Eleccion. Debe devolver una tupla con el Mazo
	resultante (la Eleccion indica qué hijo del Mazo se usará) y la Mano resultante. Devolverá Nothing si no
	quedan cartas que sacar (aunque esto no deberı́a ocurrir).
-}

robarDealer:: Mazo -> Mano -> Mano
robarDealer (Mitad carta l r) (Mano lista) = Mano (lista++[carta])

-- robarDealer toma un mazo y una mano y devuelve la mano despues de robar la primera carta del mazo


juegaLambda :: Mazo -> Mano ->Maybe Mano
juegaLambda (Mitad carta l r) mano
                   | (valor mano) > 16  = Just mano
                   | otherwise = juegaLambda (dropCartMazo (Mitad carta l r) carta) (robarDealer (Mitad carta l r) mano)
{-
	Recibe el Mazo actual, lo vuelve a convertir en una Mano en el orden apropiado, recibe la Mano del dealer,
	y devuelve la Mano resultante de robar hasta que supere un valor de 16. Devolverá Nothing si no quedan
	cartas que sacar (aunque esto no deberı́a ocurrir).
-}

