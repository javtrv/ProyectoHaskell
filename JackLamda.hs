{-
    Proyecto Haskell
    JackLamda: Es un juego interctivo de BlackJack.
    Autores: Honorio Rodriguez #09-11023.
    Javier Medina     #12-10400.
    Ult. Actualización: 09/02/2020.
-}

--imports
--import System.Random
import System.Environment
import System.IO
import System.Directory
import System.IO.Error
import Data.List
import Control.Monad
import Control.Monad.State

data GameState = GS {
                    juegosJugados  :: Int,
                    victoriasLamda :: Int,
                    victoriasJugador :: Int,
                    nombre         :: String,
                   -- generador      :: StdGen,
                    dinero         :: Int,
                    objetivo       :: Int,
                    apuesta        :: Int
                    } deriving (Show, Read)

main :: IO ()
main = do
    --let estado = GS {juegosJugados = 0, victoriasLamda = 0, victoriasJugador = 0, nombre = "Jugador", dinero = 0, objetivo = 0, apuesta = 0}
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


ciclo = do
    putStrLn "\nEstado del juego"
    let vicLamdaInfo = show vicLamdaInt
        vicJugadorInfo = show vicJugInt
        dineroInfo = show dineroInt
    putStrLn $ "Nombre del jugador: " ++ nombreInfo
    putStrLn $ "Victorias de Jack Lambda: " ++ vicLamdaInfo
    putStrLn $ "Victorias de " ++ nombreInfo ++ ": " ++ vicJugadorInfo
    putStrLn $ "Su dinero es: " ++ dineroInfo

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
            ciclo
    else if opcion == "2"
        then do 
            putStrLn $ "OPCION 2"
            guardarPartida
            putStrLn " "
            ciclo           
    else if opcion == "3"
        then do 
            putStrLn $ "OPCION 3"
            cargarPartida
            ciclo
    else if opcion == "4"
        then do 
            putStrLn $ "HASTA LUEGO."                  
    else do
            putStrLn $ "Intrujo una opción incorrecta por favor vuelva a intentarlo."
            ciclo   
    
info = do
    putStrLn "Introduzca su nombre de jugador"
    nombreInfo <- getLine
    putStrLn "Introduzca la cantidad de dinero con que comenzará a jugar"
    dineroInfo <- getLine
    objectoDo dineroInfo
    apuestaDo dineroInfo
    --let estado = GS {nombre = nombreInfo, dinero = dineroInt, juegosJugados = juegosInt, victoriasLamda = victoriasInt, objetivo = objetoInt, apuesta = apuestaInt}
    ciclo            

objectoDo dineroInfo = do
            putStrLn "Introduzca la cantidad de dinero que debe alcanzar para ganar la partida"
            objetoInfo <-getLine
            let objetoInt = read objetoInfo :: Int
                dineroInt = read dineroInfo :: Int  
            if objetoInt > dineroInt 
                then do 
                    return ()
                else do
                    putStrLn $ "La cantidad introducida es menor al dinero inicial introucido que es: " ++ dineroInfo ++ "\n"
                    objectoDo dineroInfo

apuestaDo dineroInfo = do
            putStrLn "Introduza la cantidad de dinero a ser apostado"
            apuestaInfo <-getLine
            let apuestaInt = read apuestaInfo :: Int
                dineroInt = read dineroInfo :: Int
            if 0 < apuestaInt && apuestaInt <= dineroInt
                then do 
                    return ()
                else do
                    putStrLn $ "La apuesta introducida es menor a cero o es mayor a la cantidad inicial: " ++ dineroInfo ++ "\n"
                    apuestaDo dineroInfo


jugarRonda = do putStrLn $ "Entrates a jugar ronda" 

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
            --    variable = copyInfo.nombre
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
            ciclo
        else do
            putStrLn "El archivo no existe \n"   
            cargarPartida      
            