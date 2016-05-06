# Limpieza del entorno
rm(list = ls())

##########################################################################
# Lista de paquetes a cargar

# foreign: leer ARFF
# data.table: rbindinglist
# class: kknn
# base: logaritmo
# ggplot2: gráficas
##########################################################################
# Cargamos pkgs
load.my.packages <- function(){
  pkgs = c("foreign", "data.table", "class", "base", "ggplot2")
  to.install <- pkgs[ ! pkgs %in% installed.packages() ]
  
  if ( length(to.install) > 0 )
    install.packages( to.install, dependencies = TRUE )
  
  sapply(pkgs, require, character.only=TRUE)
}

load.my.packages()


##########################################################################
### Algoritmos
##########################################################################

source(file="./NN3.r")
source(file="./SFS.r")
source(file="./BL.r")
source(file="./BMB.r")
source(file="./GRASP.r")
source(file="./ILS.r")
source(file="./ES.r")
source(file="./BT.r")
source(file="./BText.r")
source(file="./AGs.r")

##########################################################################
### Lectura de datos
##########################################################################

# Semillas aleatorias y parámetros de los algoritmos
source(file="./params.r")
# Fichero con la funcion objetivo, de particionado, de normalizacion...
source(file="./aux.r")


# Lectura de ficheros y normalizacion
mlibras <- normalize (read.arff("../data/movement_libras.arff"))
arrhythmia <- normalize (read.arff("../data/arrhythmia.arff"))
wdbc <- normalize (read.arff("../data/wdbc.arff"))

datasets <- list(wdbc, mlibras, arrhythmia)
datasets.names <- c("wdbc", "mlibras", "arrhythmia")


##########################################################################
### Obtencion de resultados
##########################################################################
#data.file <- "seleccion-caracteristicas.RData"
#load(file = data.file)

# Metodos de trayectorias simples
NN3.results <- cross.eval(NN3)
SFS.results <- cross.eval(SFS)
BL.results <- cross.eval(BL)
ES.results <- cross.eval(ES)
BT.results <- cross.eval(BT)
BT.ext.results <-cross.eval(BT.ext)

# Metodos multiarranque
BMB.results <- cross.eval(BMB)
GRASP.results <- cross.eval(GRASP)
ILS.results <- cross.eval(ILS)

# Algoritmos geneticos
AGG.results <- cross.eval(AGG)
AGE.results <- cross.eval(AGE)

#save.image(file = data.file, safe=TRUE)  