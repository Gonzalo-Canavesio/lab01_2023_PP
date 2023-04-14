module Main (main) where

import Data.Maybe (fromMaybe)
import System.Console.GetOpt (ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt)
import System.Environment (getArgs)
import Text.Read (readMaybe)

import Interp (Conf(name), initial)
import Dibujos.Ejemplo (ejemploConf)
import Dibujos.Feo (feoConf)
import Dibujos.Escher (escherConf)
import Dibujos.Grilla (grillaConf)
import Dibujos.Escher2 (escher2Conf)
import Dibujos.EscherEfe (escherEfeConf)
import Dibujos.Aureo (aureoConf)
import Dibujos.Sierpinski (sierpinskiConf)

data Flag = Lista deriving Eq

imgSize = 400

options :: [OptDescr Flag]
options = [ 
  Option ['l'] ["lista"] (NoArg Lista) "Lista los nombres de los dibujos disponibles"
  ]

-- Lista de configuraciones de los dibujos
configs :: [Conf]
configs = [ejemploConf, feoConf, escherConf, grillaConf, escher2Conf, escherEfeConf, aureoConf, sierpinskiConf]


showDibujos :: IO ()
showDibujos = do
        putStrLn "Dibujos disponibles:"
        mapM_ (\(index, config) -> putStrLn $ show index ++ ". " ++ name config) $ zip [1..] configs
        putStrLn "Escriba el indice del dibujo que quiera mostrar: "

executeConfigFromIndex :: [Conf] -> Int -> IO ()
executeConfigFromIndex configs index = do
  if index < length configs
    then 
      initial (configs !! index) imgSize
    else 
      error "El indice no existe!"

handleListaFlag :: IO ()
handleListaFlag = do
        showDibujos
        dibIndex <- getLine
        executeConfigFromIndex configs $ read dibIndex - 1

-- Dibuja el dibujo n
findAndExecuteConfig :: [Conf] -> String -> IO ()
findAndExecuteConfig [] n = do
    putStrLn $ "No hay un dibujo llamado " ++ n
findAndExecuteConfig (c : cs) input = 
    if input == name c then
        initial c imgSize
    else
        findAndExecuteConfig cs input

main :: IO ()
main = do
    args <- getArgs
    let (flags, _, _) = getOpt Permute options args
    if Lista `elem` flags 
      then handleListaFlag
      else findAndExecuteConfig configs $ head args

