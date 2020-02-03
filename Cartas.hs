-- Imports

-- Tipos de datos para carta
data Palo = Treboles | Diamantes | Picas | Corazones  deriving (Eq,Ord)
data Rango = N Int | Jack | Queen | King | Ace deriving (Eq,Ord)

data Carta = Carta {
                        rango :: Rango,
                        palo :: Palo
                    } 
-- Tipo Jugador
data Jugador = Dealer | Player deriving (Read)

-- Tipo Mano
newtype Mano = Mano [Carta]


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
    show (Mano x) = show x


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
toInt Queen = 11
toInt King = 12
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


ganador :: Mano ->Mano ->Jugador
ganador manoDealer manoJugador 
    | (blackjack manoDealer)&&(cantidadCartas manoDealer == 2) = Dealer
    | otherwise = Player
--Recibe la Mano del Dealer de primero, la mano del Player de segundo, y devuelve el ganador, seg´un las
--reglas del juego antes descritas.



