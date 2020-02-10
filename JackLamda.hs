{-
    Proyecto Haskell
    JackLamda: Es un juego interctivo de BlackJack.
    Autores: Honorio Rodriguez #09-11023.
    Javier Medina     #12-10400.
    Ult. Actualización: 09/02/2020.
-}

--imports
--import System.Random
import Cartas
import System.Environment
import System.IO
import System.Directory
import System.Random
import System.IO.Error
import Data.List
import Data.Char
import Control.Monad

data GameState = GS {
                    juegosJugados  :: Int,
                    victoriasLamda :: Int,
                    victoriasJugador :: Int,
                    nombre         :: String,
                    generador      :: StdGen,
                    dinero         :: Int,
                    objetivo       :: Int,
                    apuesta        :: Int
                    } deriving (Show, Read)


--Varibles globales
nombreInfo = " "
juegosInt = 0
vicLamdaInt = 0
vicJugInt = 0
dineroInt = 0 
objetoInt = 0
apuestaInt = 0


gameStateGenerico = GS 0 0 0 "" 0 0 0

crearGameState g juegosNew vLamdaNew vJugadorNew nombreNew dineroNew objetivoNew apuestaNew = g {juegosJugados = juegosNew, victoriasLamda = vLamdaNew, victoriasJugador= vJugadorNew, nombre = nombreNew,dinero= dineroNew, objetivo=objetivoNew, apuesta = apuestaNew}


main :: IO ()
main = do
    putStrLn " Bienvenido(a) a JackLamda \n"
    putStrLn "      Si desea cargar una partida introduzca 1"
    putStrLn "      No desea cargar una partida introduzca 2"
    intro <- getLine
    if intro == "1"
        then
            cargarPartida
    else if intro == "2"
        then
            info 
    else do
        putStrLn "Introduzco una opción incorrecta por favor vuelva a introducción una opción \n"        
        main

ciclo gs = do
    putStrLn "\nEstado del juego"
    let vicLamdaInfo = show vicLamdaInt
        vicJugadorInfo = show vicJugInt
        dineroInfo = show dineroInt
    putStrLn $ "Nombre del jugador: " ++ nombre gs
    putStrLn $ "Victorias de Jack Lambda: " ++ [chr(victoriasLamda gs)]
    putStrLn $ "Victorias de " ++ nombre gs ++ ": " ++ [chr $ victoriasJugador gs]
    putStrLn $ "Su dinero es: " ++ [chr $ dinero gs]

    putStrLn "Indica el numero de la opción que deseas realizar"
    putStrLn "    1. Jugar Ronda"
    putStrLn "    2. Guardar Partida"
    putStrLn "    3. Cargar Partida" 
    putStrLn "    4. Salir"
    opcion <- getLine               
    if opcion == "1" 
        then do
            putStrLn $ "ENTRAMOS A LA OPCION 1"
            jugarRonda
            ciclo gs
    else if opcion == "2"
        then do 
            putStrLn $ "OPCION 2"
            guardarPartida
            putStrLn " "
            ciclo gs           
    else if opcion == "3"
        then do 
            putStrLn $ "OPCION 3"
            cargarPartida
            ciclo gs
    else if opcion == "4"
        then do 
            putStrLn $ "HASTA LUEGO."                  
    else do
            putStrLn $ "Intrujo una opción incorrecta por favor vuelva a intentarlo."
            ciclo gs
    
info = do
    putStrLn "Introduzca su nombre de jugador"
    nombreInfo <- getLine
    putStrLn "Introduzca la cantidad de dinero con que comenzará a jugar"
    dineroInfo <- getLine
    putStrLn "Introduzca la cantidad de dinero que debe alcanzar para ganar la partida"
    objetoInfo <-getLine
    putStrLn "Introduza la cantidad de dinero a ser apostado"
    apuestaInfo <-getLine
    let dineroInt = read dineroInfo :: Int
    let objetoInt = read objetoInfo :: Int
    let apuestaInt = read apuestaInfo :: Int
    let resultadoGameState = crearGameState gameStateGenerico 0 0 0 nombreInfo dineroInt objetoInt apuestaInt
    ciclo resultadoGameState


jugarRonda = do
 {-   let baraInfo = baraja
        manoBarajada = barajar generador baraInfo
        manoLamda = inicialLamda manoBarajada
    putStrLn $ "Mano Lamda: " ++ manoLamda    
-}

guardarPartida = do 
    putStrLn "Introduzca un nombre de archivo"
    fileName <- getLine
    fileExists <-doesFileExist fileName
    if fileExists
        then do
            putStrLn "Lo sentimos, este archivo ya existe\n"
            guardarPartida
        else do
            let juegosInfo = show juegosInt
                vicLamdaInfo = show vicLamdaInt
                vicJugInfo = show vicJugInt
                dineroInfo = show dineroInt
                objetoInfo = show objetoInt
                apuestaInfo = show apuestaInt
                copyInfo = nombreInfo ++ "\n" ++ juegosInfo ++ "\n" ++ vicLamdaInfo ++ "\n" ++ vicJugInfo ++ "\n" ++ dineroInfo ++ "\n" ++ objetoInfo ++ "\n" ++ apuestaInfo
            writeFile fileName $ copyInfo
            main

cargarPartida = do 
    putStrLn "Introduzca un nombre de archivo"
    fileName <- getLine
    fileExists <- doesFileExist fileName
    if fileExists
        then do
            handle <- openFile fileName ReadMode
            contents <- hGetContents handle
            let toTypes = lines contents
                numbers = zipWith (\n line -> show n ++ " - " ++ line) [0..] toTypes
                nombreInfo = toTypes !! 0
                juegosInt = read (toTypes !! 1) :: Int
                vicLamdaInt = read (toTypes !! 2) :: Int
                vicJugInt = read (toTypes !! 3) :: Int
                dineroInt = read (toTypes !! 4) :: Int
                objetoInt = read (toTypes !!5) :: Int
                apuestaInt = read (toTypes !! 6) :: Int
               -- estado = GS {nombre = nombreInfo, dinero = dineroInt, juegosJugados = juegosInt, victoriasLamda = victoriasInt, objetivo = objetoInt, apuesta = apuestaInt}
            ciclo gameStateGenerico
        else do
            putStrLn "El archivo no existe \n"   
            cargarPartida      
            